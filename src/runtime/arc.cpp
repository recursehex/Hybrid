// This file implements ARC runtime routines for allocating objects and managing shared and weak reference counts.

#include "memory/ref_count.h"

#include <atomic>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <limits>

struct HybridInterfaceEntry;

struct HybridTypeDescriptor {
  const char *typeName;
  const HybridTypeDescriptor *baseType;
  const void **vtable;
  std::uint32_t vtableSize;
  const HybridInterfaceEntry *interfaces;
  std::uint32_t interfaceCount;
  void (*dealloc)(void *);
};

using hybrid::memory::RefCount;

struct HybridSharedControlBlock {
  std::atomic<std::uint32_t> strongCount;
  std::atomic<std::uint32_t> weakCount;
  void *payload;
};

extern "C" {

void hybrid_dealloc(void *obj);
HybridSharedControlBlock *
__hybrid_shared_control_create(void *payload);
void __hybrid_shared_control_retain_strong(
    HybridSharedControlBlock *control);
void __hybrid_shared_control_release_strong(
    HybridSharedControlBlock *control);
void __hybrid_shared_control_retain_weak(
    HybridSharedControlBlock *control);
void __hybrid_shared_control_release_weak(
    HybridSharedControlBlock *control);
void *__hybrid_shared_control_lock(HybridSharedControlBlock *control);
std::uint32_t __hybrid_shared_control_use_count(
    HybridSharedControlBlock *control);

void *hybrid_alloc_object(std::size_t totalSize,
                          const HybridTypeDescriptor *descriptor) {
  if (totalSize == 0)
    return nullptr;
  if (totalSize < hybrid::memory::headerSize())
    return nullptr;

  void *memory = std::calloc(1, totalSize);
  if (!memory)
    return nullptr;

  RefCount counts = RefCount::fromObject(memory);
  counts.initialize(descriptor);
  return memory;
}

void *hybrid_alloc_array(std::size_t elementSize, std::size_t elementCount,
                         const HybridTypeDescriptor *descriptor) {
  if (elementSize == 0 || elementCount == 0)
    return nullptr;
  if (elementSize > std::numeric_limits<std::size_t>::max() / elementCount)
    return nullptr;

  const std::size_t payloadSize = elementSize * elementCount;
  const std::size_t totalSize = hybrid::memory::headerSize() + payloadSize;

  void *memory = std::calloc(1, totalSize);
  if (!memory)
    return nullptr;

  RefCount counts = RefCount::fromObject(memory);
  counts.initialize(descriptor);
  return memory;
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
    const HybridTypeDescriptor *desc = counts.descriptor();
    if (desc && desc->dealloc)
      desc->dealloc(obj);
    else
      hybrid_dealloc(obj);
  }
}

void *hybrid_autorelease(void *obj) {
  // Autorelease pools are not available yet; keep value alive for caller.
  return obj;
}

void hybrid_dealloc(void *obj) {
  if (!obj)
    return;
  RefCount counts = RefCount::fromObject(obj);
  if (counts.releaseWeak()) {
    std::free(obj);
  }
}

HybridSharedControlBlock *
__hybrid_shared_control_create(void *payload) {
  if (!payload)
    return nullptr;
  auto *block = static_cast<HybridSharedControlBlock *>(
      std::calloc(1, sizeof(HybridSharedControlBlock)));
  if (!block)
    return nullptr;
  block->strongCount.store(1, std::memory_order_relaxed);
  block->weakCount.store(1, std::memory_order_relaxed);
  block->payload = payload;
  hybrid_retain(payload);
  return block;
}

void __hybrid_shared_control_retain_strong(
    HybridSharedControlBlock *control) {
  if (!control)
    return;
  control->strongCount.fetch_add(1, std::memory_order_relaxed);
}

void __hybrid_shared_control_release_strong(
    HybridSharedControlBlock *control) {
  if (!control)
    return;
  std::uint32_t previous =
      control->strongCount.fetch_sub(1, std::memory_order_acq_rel);
  if (previous == 0) {
    control->strongCount.store(0, std::memory_order_relaxed);
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
  control->weakCount.fetch_add(1, std::memory_order_relaxed);
}

void __hybrid_shared_control_release_weak(
    HybridSharedControlBlock *control) {
  if (!control)
    return;
  std::uint32_t previous =
      control->weakCount.fetch_sub(1, std::memory_order_acq_rel);
  if (previous == 0) {
    control->weakCount.store(0, std::memory_order_relaxed);
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
      control->strongCount.load(std::memory_order_acquire);
  while (observed != 0) {
    if (control->strongCount.compare_exchange_weak(
            observed, observed + 1, std::memory_order_acq_rel,
            std::memory_order_acquire)) {
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
  return control->strongCount.load(std::memory_order_acquire);
}

} // extern "C"
