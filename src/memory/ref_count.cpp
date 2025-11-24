// This file implements reference counting helpers and ARC header utilities used by the runtime.

#include "memory/ref_count.h"

namespace hybrid::memory {

namespace {
constexpr std::memory_order kAcquireRelease = std::memory_order_acq_rel;
constexpr std::memory_order kRelaxed = std::memory_order_relaxed;
} // namespace

RefCount::RefCount(ARCHeader *header) : header_(header) {}

void RefCount::reset(ARCHeader *header) { header_ = header; }

bool RefCount::isValid() const { return header_ != nullptr; }

ARCHeader *RefCount::header() const { return header_; }

const HybridTypeDescriptor *RefCount::descriptor() const {
  return header_ ? header_->typeDescriptor : nullptr;
}

void RefCount::setDescriptor(const HybridTypeDescriptor *descriptor) {
  if (header_)
    header_->typeDescriptor = descriptor;
}

void RefCount::initialize(const HybridTypeDescriptor *descriptor) {
  if (!header_)
    return;
  header_->strongCount.store(1, kRelaxed);
  header_->weakCount.store(1, kRelaxed);
  header_->typeDescriptor = descriptor;
}

void RefCount::retainStrong() const {
  if (header_)
    header_->strongCount.fetch_add(1, kRelaxed);
}

bool RefCount::releaseStrong() const {
  if (!header_)
    return false;
  const std::uint32_t previous =
      header_->strongCount.fetch_sub(1, kAcquireRelease);
  if (previous == 0) {
    header_->strongCount.store(0, kRelaxed);
    return false;
  }
  return previous == 1;
}

void RefCount::retainWeak() const {
  if (header_)
    header_->weakCount.fetch_add(1, kRelaxed);
}

bool RefCount::releaseWeak() const {
  if (!header_)
    return false;
  const std::uint32_t previous =
      header_->weakCount.fetch_sub(1, kAcquireRelease);
  if (previous == 0) {
    header_->weakCount.store(0, kRelaxed);
    return false;
  }
  return previous == 1;
}

std::uint32_t RefCount::strongCount() const {
  if (!header_)
    return 0;
  return header_->strongCount.load(std::memory_order_acquire);
}

std::uint32_t RefCount::weakCount() const {
  if (!header_)
    return 0;
  return header_->weakCount.load(std::memory_order_acquire);
}

ARCHeader *RefCount::headerFromObject(void *object) {
  return static_cast<ARCHeader *>(object);
}

void *RefCount::objectFromHeader(ARCHeader *header) { return header; }

RefCount RefCount::fromObject(void *object) {
  return RefCount(headerFromObject(object));
}

std::size_t headerSize() { return sizeof(ARCHeader); }

} // namespace hybrid::memory
