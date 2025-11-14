#ifndef HYBRID_MEMORY_REF_COUNT_H
#define HYBRID_MEMORY_REF_COUNT_H

#include <atomic>
#include <cstddef>
#include <cstdint>

struct HybridTypeDescriptor;

namespace hybrid::memory {

struct ARCHeader {
  std::atomic<std::uint32_t> strongCount;
  std::atomic<std::uint32_t> weakCount;
  const HybridTypeDescriptor *typeDescriptor;
};

static_assert(sizeof(std::atomic<std::uint32_t>) == sizeof(std::uint32_t),
              "ARCHeader layout requires lock-free 32-bit atomics");

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
