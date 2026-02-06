// This file implements ARC runtime routines for allocating objects and managing shared and weak reference counts.

#include "hybrid_runtime.h"
#include "memory/ref_count.h"

#include <algorithm>
#include <atomic>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <limits>
#include <map>
#include <mutex>
#include <sstream>
#include <string>
#include <thread>
#include <utility>
#include <vector>

using hybrid::memory::RefCount;

int hybrid_debug_leaks = 0;
int hybrid_debug_reftrace = 0;
int hybrid_debug_verify = 0;
int hybrid_debug_pool = 0;
using ArrayHeader = hybrid_array_header_t;
static const HybridTypeDescriptor *CachedArrayDescriptor = nullptr;
static std::once_flag ArrayDescriptorInit;

extern "C" void hybrid_array_dealloc(void *obj);

extern "C" void hybrid_release(void *obj);

static_assert(sizeof(hybrid::memory::ARCHeader) == sizeof(hybrid_refcount_t),
              "ARC header layout drifted from runtime header");
static_assert(
    offsetof(hybrid::memory::ARCHeader, descriptor) ==
        offsetof(hybrid_refcount_t, descriptor),
    "Descriptor pointer offset mismatch");
static_assert(offsetof(ArrayHeader, counts) == 0,
              "Array header must start with ARC counts");

namespace {

std::mutex DescriptorMutex;
std::map<std::string, const HybridTypeDescriptor *> DescriptorCache;
std::once_flag LeakAtexitRegistration;

std::mutex &getLeakRegistryMutex() {
  static auto *mutexPtr = new std::mutex();
  return *mutexPtr;
}

std::map<void *, std::thread::id> &getLeakRegistry() {
  static auto *registryPtr = new std::map<void *, std::thread::id>();
  return *registryPtr;
}

const HybridTypeDescriptor *getFallbackDescriptor() {
  static const HybridTypeDescriptor anonDesc{
      "<anonymous>", nullptr, nullptr, 0, nullptr, 0, nullptr};
  return &anonDesc;
}

struct LeakInfo {
  const HybridTypeDescriptor *descriptor = nullptr;
  std::thread::id threadId{};
};

struct TraceEvent {
  const char *op = nullptr;
  const char *label = nullptr;
  const HybridTypeDescriptor *descriptor = nullptr;
  void *object = nullptr;
  std::uint32_t strong = 0;
  std::uint32_t weak = 0;
};

thread_local std::vector<TraceEvent> TraceBuffer;
thread_local const char *PendingTraceLabel = nullptr;

const HybridTypeDescriptor *
canonicalizeDescriptor(const HybridTypeDescriptor *descriptor) {
  if (!descriptor || !descriptor->typeName)
    return descriptor;
  std::lock_guard<std::mutex> lock(DescriptorMutex);
  auto it = DescriptorCache.find(descriptor->typeName);
  if (it != DescriptorCache.end())
    return it->second;
  DescriptorCache.emplace(descriptor->typeName, descriptor);
  return descriptor;
}

bool descriptorLooksValid(const HybridTypeDescriptor *descriptor) {
  if (!descriptor)
    return true;
  if (descriptor->vtableSize > 0 && !descriptor->vtable)
    return false;
  if (descriptor->interfaceCount > 0 && !descriptor->interfaces)
    return false;
  return true;
}

bool descriptorRegistered(const HybridTypeDescriptor *descriptor) {
  if (!descriptor)
    return false;
  std::lock_guard<std::mutex> lock(DescriptorMutex);
  for (const auto &entry : DescriptorCache) {
    if (entry.second == descriptor)
      return true;
  }
  return false;
}

struct AutoreleasePool {
  std::vector<void *> objects;
};

thread_local std::vector<AutoreleasePool> AutoreleasePools;

static const char *consumeTraceLabel(const char *label) {
  if (PendingTraceLabel) {
    const char *pending = PendingTraceLabel;
    PendingTraceLabel = nullptr;
    return pending;
  }
  return label;
}

void recordTraceEvent(const char *op, void *object,
                      const HybridTypeDescriptor *descriptor,
                      const char *label, std::uint32_t strong,
                      std::uint32_t weak) {
  if (!hybrid_debug_reftrace)
    return;
  TraceEvent event;
  event.op = op;
  event.object = object;
  event.descriptor = descriptor;
  event.strong = strong;
  event.weak = weak;
  event.label = consumeTraceLabel(label);
  TraceBuffer.push_back(event);
}

void ensureLeakAtexitRegistered() {
  std::call_once(LeakAtexitRegistration,
                 []() { std::atexit(hybrid_arc_dump_leaks_default); });
}

void registerLeak(void *object, const HybridTypeDescriptor *descriptor,
                  const RefCount &counts) {
  if (!hybrid_debug_leaks || !object)
    return;
  ensureLeakAtexitRegistered();
  std::lock_guard<std::mutex> lock(getLeakRegistryMutex());
  (void)descriptor;
  (void)counts;
  getLeakRegistry()[object] = std::this_thread::get_id();
}

void unregisterLeak(void *object) {
  if (!hybrid_debug_leaks || !object)
    return;
  std::lock_guard<std::mutex> lock(getLeakRegistryMutex());
  getLeakRegistry().erase(object);
}

void drainPool(AutoreleasePool &pool) {
  for (void *obj : pool.objects) {
    hybrid_release(obj);
  }
  pool.objects.clear();
  if (hybrid_debug_reftrace)
    hybrid_arc_trace_flush(nullptr);
}

} // namespace

static bool verifyObjectPointer(void *object, std::string &reason) {
  reason.clear();
  if (!object) {
    reason = "null object";
    return false;
  }
  const std::uintptr_t addr = reinterpret_cast<std::uintptr_t>(object);
  if (addr % alignof(hybrid::memory::ARCHeader) != 0) {
    reason = "misaligned ARC header";
    return false;
  }
  RefCount counts = RefCount::fromObject(object);
  const HybridTypeDescriptor *descriptor = counts.descriptor();
  if (!descriptor || !descriptor->typeName) {
    reason = "missing descriptor";
    return false;
  }
  if (!descriptorRegistered(descriptor)) {
    reason = "descriptor not registered";
    return false;
  }
  if (!descriptorLooksValid(descriptor)) {
    reason = "descriptor layout invalid";
    return false;
  }

  const std::uint32_t strong = counts.strongCount();
  if (strong == 0) {
    reason = "zero refcount";
    return false;
  }
  return true;
}

struct HybridSharedControlBlock {
  std::atomic<std::uint32_t> strongCount;
  std::atomic<std::uint32_t> weakCount;
  void *payload;
};

extern "C" {

void hybrid_dealloc(void *obj);
size_t hybrid_array_payload_offset(void) { return sizeof(ArrayHeader); }

const HybridTypeDescriptor *hybrid_array_type_descriptor(void) {
  std::call_once(ArrayDescriptorInit, []() {
    static HybridTypeDescriptor descriptor{"array", nullptr, nullptr, 0,
                                           nullptr, 0, hybrid_array_dealloc};
    CachedArrayDescriptor = hybrid_register_type_descriptor(&descriptor);
    if (!CachedArrayDescriptor)
      CachedArrayDescriptor = &descriptor;
  });
  return CachedArrayDescriptor;
}

void hybrid_arc_set_debug_flags(int leakDetect, int refTrace, int verify,
                                int poolDebug) {
  hybrid_debug_leaks = leakDetect ? 1 : 0;
  hybrid_debug_reftrace = refTrace ? 1 : 0;
  hybrid_debug_verify = verify ? 1 : 0;
  hybrid_debug_pool = poolDebug ? 1 : 0;
  if (hybrid_debug_leaks)
    ensureLeakAtexitRegistered();
}

void hybrid_arc_trace_set_label(const char *label) {
  PendingTraceLabel = label;
}

void hybrid_arc_trace_flush(FILE *sink) {
  if (!hybrid_debug_reftrace)
    return;
  FILE *target = sink ? sink : stderr;
  for (const TraceEvent &event : TraceBuffer) {
    const char *typeName =
        event.descriptor && event.descriptor->typeName
            ? event.descriptor->typeName
            : "<unknown>";
    const char *label =
        event.label && event.label[0] ? event.label : "-";
    std::fprintf(target,
                 "[arc-trace] op=%s ptr=%p type=%s strong=%u weak=%u "
                 "label=%s\n",
                 event.op ? event.op : "-", event.object, typeName,
                 static_cast<unsigned>(event.strong),
                 static_cast<unsigned>(event.weak), label);
  }
  TraceBuffer.clear();
  std::fflush(target);
}

void hybrid_arc_trace_flush_default(void) { hybrid_arc_trace_flush(nullptr); }

void hybrid_arc_dump_leaks(FILE *sink) {
  if (!hybrid_debug_leaks)
    return;
  FILE *target = sink ? sink : stderr;
  std::lock_guard<std::mutex> lock(getLeakRegistryMutex());
  auto &registry = getLeakRegistry();
  if (registry.empty()) {
    std::fprintf(target, "[arc-leak] no outstanding objects\n");
    std::fflush(target);
    return;
  }
  for (const auto &entry : registry) {
    void *ptr = entry.first;
    RefCount counts = RefCount::fromObject(ptr);
    const HybridTypeDescriptor *desc = counts.descriptor();
    const char *typeName =
        desc && desc->typeName ? desc->typeName : "<unknown>";
    std::ostringstream tid;
    tid << entry.second;
    std::fprintf(target,
                 "[arc-leak] ptr=%p type=%s thread=%s strong=%u weak=%u\n",
                 ptr, typeName, tid.str().c_str(),
                 static_cast<unsigned>(counts.strongCount()),
                 static_cast<unsigned>(counts.weakCount()));
  }
  std::fflush(target);
}

void hybrid_arc_dump_leaks_default(void) { hybrid_arc_dump_leaks(nullptr); }

int hybrid_arc_verify_object(void *object) {
  std::string reason;
  bool ok = verifyObjectPointer(object, reason);
  if (!ok && hybrid_debug_verify) {
    std::fprintf(stderr, "[arc-verify] %s (ptr=%p)\n", reason.c_str(),
                 object);
  }
  return ok ? 1 : 0;
}

void hybrid_autorelease_pool_scoped_debug(const char *label) {
  if (!hybrid_debug_pool && !hybrid_debug_reftrace)
    return;
  std::fprintf(stderr, "[arc-pool] depth=%zu label=%s\n",
               AutoreleasePools.size(), label ? label : "-");
}

const HybridTypeDescriptor *
hybrid_register_type_descriptor(const HybridTypeDescriptor *descriptor) {
  if (!descriptorLooksValid(descriptor))
    return descriptor;
  return canonicalizeDescriptor(descriptor);
}

int hybrid_verify_descriptor_layout(void) {
  bool ok = sizeof(hybrid::memory::ARCHeader) == sizeof(hybrid_refcount_t);
  ok = ok && offsetof(hybrid_refcount_t, descriptor) ==
                offsetof(hybrid::memory::ARCHeader, descriptor);
  return ok ? 1 : 0;
}

void *hybrid_alloc_object(std::size_t totalSize,
                          const HybridTypeDescriptor *descriptor) {
  const HybridTypeDescriptor *canonical =
      hybrid_register_type_descriptor(
          descriptor ? descriptor : getFallbackDescriptor());
  if (totalSize == 0)
    return nullptr;
  if (totalSize < hybrid::memory::headerSize())
    return nullptr;
  if (!descriptorLooksValid(canonical))
    return nullptr;

  void *memory = std::calloc(1, totalSize);
  if (!memory)
    return nullptr;

  RefCount counts = RefCount::fromObject(memory);
  counts.initialize(canonical);
  registerLeak(memory, canonical, counts);
  recordTraceEvent("alloc.object", memory, canonical, nullptr,
                   counts.strongCount(), counts.weakCount());
  return memory;
}

void *hybrid_alloc_array(std::size_t elementSize, std::size_t elementCount,
                         const HybridTypeDescriptor *descriptor) {
  const HybridTypeDescriptor *canonical =
      hybrid_register_type_descriptor(
          descriptor ? descriptor : hybrid_array_type_descriptor());
  if (elementSize == 0)
    return nullptr;
  if (elementCount > 0 &&
      elementSize > std::numeric_limits<std::size_t>::max() / elementCount)
    return nullptr;
  if (!descriptorLooksValid(canonical))
    return nullptr;

  const std::size_t payloadSize = elementSize * elementCount;
  const std::size_t totalSize = sizeof(ArrayHeader) + payloadSize;

  void *memory = std::calloc(1, totalSize);
  if (!memory)
    return nullptr;

  auto *header = static_cast<ArrayHeader *>(memory);
  header->length = elementCount;
  header->elementSize = elementSize;
  header->elementRelease = nullptr;

  RefCount counts = RefCount::fromObject(memory);
  counts.initialize(canonical);
  registerLeak(memory, canonical, counts);
  recordTraceEvent("alloc.array", memory, canonical, nullptr,
                   counts.strongCount(), counts.weakCount());
  return memory;
}

void *hybrid_new_object(std::size_t totalSize,
                        const HybridTypeDescriptor *descriptor) {
  return hybrid_alloc_object(totalSize, descriptor);
}

void *hybrid_new_array(std::size_t elementSize, std::size_t elementCount,
                       const HybridTypeDescriptor *descriptor) {
  return hybrid_alloc_array(elementSize, elementCount, descriptor);
}

void hybrid_array_set_release(void *obj,
                              hybrid_array_release_fn releaseFn) {
  if (!obj)
    return;
  auto *header = static_cast<ArrayHeader *>(obj);
  header->elementRelease = releaseFn;
}

void hybrid_array_release_ref_slot(void *slot) {
  if (!slot)
    return;
  void *value = *static_cast<void **>(slot);
  if (value)
    hybrid_release(value);
}

void hybrid_array_release_array_slot(void *slot) {
  if (!slot)
    return;
  void *payloadPtr = *static_cast<void **>(slot);
  if (!payloadPtr)
    return;
  auto *bytePtr = static_cast<std::byte *>(payloadPtr);
  bytePtr -= static_cast<std::ptrdiff_t>(hybrid_array_payload_offset());
  hybrid_release(static_cast<void *>(bytePtr));
}

void hybrid_array_retain_ref_slot(void *slot) {
  if (!slot)
    return;
  void *value = *static_cast<void **>(slot);
  if (value)
    hybrid_retain(value);
}

void hybrid_array_retain_array_slot(void *slot) {
  if (!slot)
    return;
  void *payloadPtr = *static_cast<void **>(slot);
  if (!payloadPtr)
    return;
  auto *bytePtr = static_cast<std::byte *>(payloadPtr);
  bytePtr -= static_cast<std::ptrdiff_t>(hybrid_array_payload_offset());
  hybrid_retain(static_cast<void *>(bytePtr));
}

void *hybrid_array_resize(void *obj, std::size_t elementSize,
                          std::size_t elementCount,
                          hybrid_array_release_fn releaseFn,
                          hybrid_array_retain_fn retainFn) {
  if (elementSize == 0)
    return nullptr;

  void *newObj = hybrid_alloc_array(elementSize, elementCount, nullptr);
  if (!newObj)
    return nullptr;

  if (releaseFn)
    hybrid_array_set_release(newObj, releaseFn);

  if (!obj || elementCount == 0)
    return newObj;

  auto *oldHeader = static_cast<ArrayHeader *>(obj);
  const std::size_t copyCount =
      std::min(oldHeader->length, elementCount);
  if (copyCount == 0)
    return newObj;

  auto *oldPayload = static_cast<std::byte *>(obj) +
                     static_cast<std::ptrdiff_t>(hybrid_array_payload_offset());
  auto *newPayload =
      static_cast<std::byte *>(newObj) +
      static_cast<std::ptrdiff_t>(hybrid_array_payload_offset());
  std::memcpy(newPayload, oldPayload, copyCount * elementSize);

  if (retainFn) {
    for (std::size_t idx = 0; idx < copyCount; ++idx) {
      retainFn(newPayload + idx * elementSize);
    }
  }

  return newObj;
}

void *hybrid_retain(void *obj) {
  if (!obj)
    return nullptr;
  if (hybrid_debug_verify && !hybrid_arc_verify_object(obj)) {
    std::abort();
  }
  RefCount counts = RefCount::fromObject(obj);
  registerLeak(obj, counts.descriptor(), counts);
  counts.retainStrong();
  recordTraceEvent("retain", obj, counts.descriptor(), nullptr,
                   counts.strongCount(), counts.weakCount());
  return obj;
}

void hybrid_release(void *obj) {
  if (!obj)
    return;
  if (hybrid_debug_verify && !hybrid_arc_verify_object(obj)) {
    std::abort();
  }
  RefCount counts = RefCount::fromObject(obj);
  const HybridTypeDescriptor *desc = counts.descriptor();
  const bool shouldDealloc = counts.releaseStrong();
  recordTraceEvent("release", obj, desc, nullptr,
                   counts.strongCount(), counts.weakCount());
  if (shouldDealloc) {
    hybrid_zero_weak_refs(obj);
    if (desc && desc->dealloc)
      desc->dealloc(obj);
    else
      hybrid_dealloc(obj);
  }
}

void hybrid_free(void *obj) { hybrid_release(obj); }

void hybrid_dealloc(void *obj) {
  if (!obj)
    return;
  RefCount counts = RefCount::fromObject(obj);
  unregisterLeak(obj);
  recordTraceEvent("dealloc", obj, counts.descriptor(), nullptr,
                   counts.strongCount(), counts.weakCount());
  if (counts.releaseWeak()) {
    std::free(obj);
  }
}

void hybrid_array_dealloc(void *obj) {
  if (!obj)
    return;
  auto *header = static_cast<ArrayHeader *>(obj);
  hybrid_array_release_fn releaseFn = header->elementRelease;
  if (releaseFn) {
    std::byte *payload = static_cast<std::byte *>(obj) +
                         static_cast<std::ptrdiff_t>(hybrid_array_payload_offset());
    const std::size_t elementSize = header->elementSize;
    const std::size_t length = header->length;
    for (std::size_t idx = 0; idx < length; ++idx) {
      releaseFn(payload + idx * elementSize);
    }
  }
  hybrid_dealloc(obj);
}

void hybrid_autorelease_pool_push(void) {
  AutoreleasePools.emplace_back();
  hybrid_autorelease_pool_scoped_debug("push");
}

void hybrid_autorelease_pool_pop(void) {
  if (AutoreleasePools.empty())
    return;
  AutoreleasePool pool = std::move(AutoreleasePools.back());
  AutoreleasePools.pop_back();
  drainPool(pool);
  hybrid_autorelease_pool_scoped_debug("pop");
}

void hybrid_autorelease_pool_drain(void) {
  if (AutoreleasePools.empty())
    return;
  hybrid_autorelease_pool_scoped_debug("drain");
  drainPool(AutoreleasePools.back());
}

void *hybrid_autorelease(void *obj) {
  if (!obj)
    return nullptr;
  if (hybrid_debug_verify && !hybrid_arc_verify_object(obj)) {
    std::abort();
  }
  RefCount counts = RefCount::fromObject(obj);
  recordTraceEvent("autorelease", obj, counts.descriptor(), nullptr,
                   counts.strongCount(), counts.weakCount());
  if (AutoreleasePools.empty())
    return obj;
  AutoreleasePools.back().objects.push_back(obj);
  return obj;
}

HybridSharedControlBlock *
__hybrid_shared_control_create(void *payload) {
  if (!payload)
    return nullptr;
  auto *block = static_cast<HybridSharedControlBlock *>(
      std::calloc(1, sizeof(HybridSharedControlBlock)));
  if (!block)
    return nullptr;
  block->strongCount.store(1, HYBRID_RELAXED);
  block->weakCount.store(1, HYBRID_RELAXED);
  block->payload = payload;
  return block;
}

void __hybrid_shared_control_retain_strong(
    HybridSharedControlBlock *control) {
  if (!control)
    return;
  control->strongCount.fetch_add(1, HYBRID_RELAXED);
}

void __hybrid_shared_control_release_strong(
    HybridSharedControlBlock *control) {
  if (!control)
    return;
  std::uint32_t previous =
      control->strongCount.fetch_sub(1, HYBRID_ACQ_REL);
  if (previous == 0) {
    control->strongCount.store(0, HYBRID_RELAXED);
    return;
  }
  if (previous == 1) {
    void *payload = control->payload;
    control->payload = nullptr;
    if (payload)
      hybrid_release(payload);
    __hybrid_shared_control_release_weak(control);
  }
}

void __hybrid_shared_control_retain_weak(
    HybridSharedControlBlock *control) {
  if (!control)
    return;
  control->weakCount.fetch_add(1, HYBRID_RELAXED);
}

void __hybrid_shared_control_release_weak(
    HybridSharedControlBlock *control) {
  if (!control)
    return;
  std::uint32_t previous =
      control->weakCount.fetch_sub(1, HYBRID_ACQ_REL);
  if (previous == 0) {
    control->weakCount.store(0, HYBRID_RELAXED);
    return;
  }
  if (previous == 1) {
    std::free(control);
  }
}

void *__hybrid_shared_control_lock(HybridSharedControlBlock *control) {
  if (!control)
    return nullptr;
  if (!control->payload)
    return nullptr;
  std::uint32_t observed =
      control->strongCount.load(HYBRID_ACQUIRE);
  while (observed != 0) {
    if (control->strongCount.compare_exchange_weak(
            observed, observed + 1, HYBRID_ACQ_REL,
            HYBRID_ACQUIRE)) {
      return control->payload;
    }
  }
  return nullptr;
}

std::uint32_t __hybrid_shared_control_use_count(
    HybridSharedControlBlock *control) {
  if (!control)
    return 0;
  return control->strongCount.load(HYBRID_ACQUIRE);
}

void __hybrid_shared_control_debug_dump(HybridSharedControlBlock *control,
                                        FILE *sink) {
  if (!control)
    return;
  FILE *target = sink ? sink : stderr;
  std::fprintf(target,
               "[arc-shared] control=%p strong=%u weak=%u payload=%p\n",
               static_cast<void *>(control),
               static_cast<unsigned>(control->strongCount.load(HYBRID_ACQUIRE)),
               static_cast<unsigned>(control->weakCount.load(HYBRID_ACQUIRE)),
               control->payload);
  std::fflush(target);
}

} // extern "C"
