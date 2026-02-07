#ifndef HYBRID_MEMORY_REF_COUNT_H
#define HYBRID_MEMORY_REF_COUNT_H

#include "hybrid_runtime.h"

namespace hybrid::memory {

using ARCHeader = hybrid_refcount_t;

class RefCount {
public:
  RefCount() = default;
  explicit RefCount(ARCHeader *header);

  void reset(ARCHeader *header);
  bool isValid() const;

  ARCHeader *header() const;
  const HybridTypeDescriptor *descriptor() const;
  void setDescriptor(const HybridTypeDescriptor *descriptor);

  void initialize(const HybridTypeDescriptor *descriptor);

  void retainStrong() const;
  bool releaseStrong() const;

  void retainWeak() const;
  bool releaseWeak() const;

  std::uint32_t strongCount() const;
  std::uint32_t weakCount() const;

  static ARCHeader *headerFromObject(void *object);
  static void *objectFromHeader(ARCHeader *header);
  static RefCount fromObject(void *object);

private:
  ARCHeader *header_ = nullptr;
};

std::size_t headerSize();

} // namespace hybrid::memory

#endif // HYBRID_MEMORY_REF_COUNT_H
