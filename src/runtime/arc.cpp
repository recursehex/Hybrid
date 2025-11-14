#include "memory/ref_count.h"

#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <limits>

struct HybridTypeDescriptor;

using hybrid::memory::RefCount;

extern "C" {

void hybrid_dealloc(void *obj);

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
  if (counts.releaseStrong())
    hybrid_dealloc(obj);
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

} // extern "C"
