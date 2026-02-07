// This file implements reference counting helpers and ARC header utilities used by the runtime.

#include "memory/ref_count.h"

namespace hybrid::memory {

RefCount::RefCount(ARCHeader *header) : header_(header) {}

void RefCount::reset(ARCHeader *header) { header_ = header; }

bool RefCount::isValid() const { return header_ != nullptr; }

ARCHeader *RefCount::header() const { return header_; }

const HybridTypeDescriptor *RefCount::descriptor() const {
  return header_ ? header_->descriptor : nullptr;
}

void RefCount::setDescriptor(const HybridTypeDescriptor *descriptor) {
  if (header_)
    header_->descriptor = descriptor;
}

void RefCount::initialize(const HybridTypeDescriptor *descriptor) {
  hybrid_refcount_init(header_, descriptor);
}

void RefCount::retainStrong() const {
  hybrid_refcount_retain_strong(header_);
}

bool RefCount::releaseStrong() const {
  return hybrid_refcount_release_strong(header_);
}

void RefCount::retainWeak() const {
  hybrid_refcount_retain_weak(header_);
}

bool RefCount::releaseWeak() const {
  return hybrid_refcount_release_weak(header_);
}

std::uint32_t RefCount::strongCount() const {
  return hybrid_refcount_strong_count(header_);
}

std::uint32_t RefCount::weakCount() const {
  return hybrid_refcount_weak_count(header_);
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
