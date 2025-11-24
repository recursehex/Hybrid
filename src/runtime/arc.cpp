// This file implements ARC runtime routines for allocating objects and managing shared and weak reference counts.

#include "hybrid_runtime.h"
#include "memory/ref_count.h"

#include <atomic>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <limits>
#include <map>
#include <mutex>
#include <string>
#include <utility>
#include <vector>

using hybrid::memory::RefCount;

extern "C" void hybrid_release(void *obj);

static_assert(sizeof(hybrid::memory::ARCHeader) == sizeof(hybrid_refcount_t),
              "ARC header layout drifted from runtime header");
static_assert(
    offsetof(hybrid::memory::ARCHeader, descriptor) ==
        offsetof(hybrid_refcount_t, descriptor),
    "Descriptor pointer offset mismatch");

namespace {

std::mutex DescriptorMutex;
std::map<std::string, const HybridTypeDescriptor *> DescriptorCache;

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

struct AutoreleasePool {
  std::vector<void *> objects;
};

thread_local std::vector<AutoreleasePool> AutoreleasePools;

void drainPool(AutoreleasePool &pool) {
  for (void *obj : pool.objects) {
    hybrid_release(obj);
  }
  pool.objects.clear();
}

} // namespace

struct HybridSharedControlBlock {
  std::atomic<std::uint32_t> strongCount;
  std::atomic<std::uint32_t> weakCount;
  void *payload;
};

extern "C" {

void hybrid_dealloc(void *obj);

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
      hybrid_register_type_descriptor(descriptor);
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
  return memory;
}

void *hybrid_alloc_array(std::size_t elementSize, std::size_t elementCount,
                         const HybridTypeDescriptor *descriptor) {
  const HybridTypeDescriptor *canonical =
      hybrid_register_type_descriptor(descriptor);
  if (elementSize == 0 || elementCount == 0)
    return nullptr;
  if (elementSize > std::numeric_limits<std::size_t>::max() / elementCount)
    return nullptr;
  if (!descriptorLooksValid(canonical))
    return nullptr;

  const std::size_t payloadSize = elementSize * elementCount;
  const std::size_t totalSize = hybrid::memory::headerSize() + payloadSize;

  void *memory = std::calloc(1, totalSize);
  if (!memory)
    return nullptr;

  RefCount counts = RefCount::fromObject(memory);
  counts.initialize(canonical);
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

void *hybrid_retain(void *obj) {
  if (!obj)
    return nullptr;
  RefCount counts = RefCount::fromObject(obj);
  counts.retainStrong();
  return obj;
}

void hybrid_release(void *obj) {
  if (!obj)
    return;
  RefCount counts = RefCount::fromObject(obj);
  if (counts.releaseStrong()) {
    hybrid_zero_weak_refs(obj);
    const HybridTypeDescriptor *desc = counts.descriptor();
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
  if (counts.releaseWeak()) {
    std::free(obj);
  }
}

void hybrid_autorelease_pool_push(void) { AutoreleasePools.emplace_back(); }

void hybrid_autorelease_pool_pop(void) {
  if (AutoreleasePools.empty())
    return;
  AutoreleasePool pool = std::move(AutoreleasePools.back());
  AutoreleasePools.pop_back();
  drainPool(pool);
}

void hybrid_autorelease_pool_drain(void) {
  if (AutoreleasePools.empty())
    return;
  drainPool(AutoreleasePools.back());
}

void *hybrid_autorelease(void *obj) {
  if (!obj)
    return nullptr;
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
      hybrid_retain(control->payload);
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

} // extern "C"
