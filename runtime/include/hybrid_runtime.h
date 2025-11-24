#ifndef HYBRID_RUNTIME_H
#define HYBRID_RUNTIME_H

#ifdef __cplusplus
#include <atomic>
#include <cstddef>
#include <cstdint>
#define HYBRID_STATIC_ASSERT static_assert
using hybrid_atomic_u32 = std::atomic<std::uint32_t>;
constexpr auto HYBRID_RELAXED = std::memory_order_relaxed;
constexpr auto HYBRID_ACQ_REL = std::memory_order_acq_rel;
constexpr auto HYBRID_ACQUIRE = std::memory_order_acquire;
#else
#include <stdatomic.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#define HYBRID_STATIC_ASSERT _Static_assert
typedef _Atomic(uint32_t) hybrid_atomic_u32;
#define HYBRID_RELAXED memory_order_relaxed
#define HYBRID_ACQ_REL memory_order_acq_rel
#define HYBRID_ACQUIRE memory_order_acquire
#endif

typedef struct HybridTypeDescriptor HybridTypeDescriptor;
typedef struct HybridSharedControlBlock HybridSharedControlBlock;

typedef struct HybridInterfaceEntry {
  const HybridTypeDescriptor *interfaceType;
  const void **methodTable;
} HybridInterfaceEntry;

struct HybridTypeDescriptor {
  const char *typeName;
  const HybridTypeDescriptor *baseType;
  const void **vtable;
  uint32_t vtableSize;
  const HybridInterfaceEntry *interfaces;
  uint32_t interfaceCount;
  void (*dealloc)(void *);
};

typedef struct hybrid_refcount_t {
  hybrid_atomic_u32 strongCount;
  hybrid_atomic_u32 weakCount;
  const HybridTypeDescriptor *descriptor;
} hybrid_refcount_t;

HYBRID_STATIC_ASSERT(sizeof(hybrid_atomic_u32) == sizeof(uint32_t),
                     "Refcount atomics must be lock-free u32");
HYBRID_STATIC_ASSERT(offsetof(hybrid_refcount_t, strongCount) == 0,
                     "ARC header strong count must be first field");
HYBRID_STATIC_ASSERT(
    offsetof(hybrid_refcount_t, weakCount) == sizeof(hybrid_atomic_u32),
    "ARC header weak count offset mismatch");

#ifdef __cplusplus
extern "C" {
#endif

static inline void hybrid_refcount_init(hybrid_refcount_t *header,
                                        const HybridTypeDescriptor *descriptor) {
  if (!header)
    return;
#ifdef __cplusplus
  header->strongCount.store(1, HYBRID_RELAXED);
  header->weakCount.store(1, HYBRID_RELAXED);
#else
  atomic_store_explicit(&header->strongCount, 1, HYBRID_RELAXED);
  atomic_store_explicit(&header->weakCount, 1, HYBRID_RELAXED);
#endif
  header->descriptor = descriptor;
}

static inline void hybrid_refcount_retain_strong(
    hybrid_refcount_t *header) {
  if (!header)
    return;
#ifdef __cplusplus
  header->strongCount.fetch_add(1, HYBRID_RELAXED);
#else
  atomic_fetch_add_explicit(&header->strongCount, 1, HYBRID_RELAXED);
#endif
}

static inline bool hybrid_refcount_release_strong(
    hybrid_refcount_t *header) {
  if (!header)
    return false;
#ifdef __cplusplus
  const std::uint32_t previous =
      header->strongCount.fetch_sub(1, HYBRID_ACQ_REL);
  if (previous == 0) {
    header->strongCount.store(0, HYBRID_RELAXED);
    return false;
  }
  return previous == 1;
#else
  const uint32_t previous =
      atomic_fetch_sub_explicit(&header->strongCount, 1, HYBRID_ACQ_REL);
  if (previous == 0) {
    atomic_store_explicit(&header->strongCount, 0, HYBRID_RELAXED);
    return false;
  }
  return previous == 1;
#endif
}

static inline void hybrid_refcount_retain_weak(
    hybrid_refcount_t *header) {
  if (!header)
    return;
#ifdef __cplusplus
  header->weakCount.fetch_add(1, HYBRID_RELAXED);
#else
  atomic_fetch_add_explicit(&header->weakCount, 1, HYBRID_RELAXED);
#endif
}

static inline bool hybrid_refcount_release_weak(
    hybrid_refcount_t *header) {
  if (!header)
    return false;
#ifdef __cplusplus
  const std::uint32_t previous =
      header->weakCount.fetch_sub(1, HYBRID_ACQ_REL);
  if (previous == 0) {
    header->weakCount.store(0, HYBRID_RELAXED);
    return false;
  }
  return previous == 1;
#else
  const uint32_t previous =
      atomic_fetch_sub_explicit(&header->weakCount, 1, HYBRID_ACQ_REL);
  if (previous == 0) {
    atomic_store_explicit(&header->weakCount, 0, HYBRID_RELAXED);
    return false;
  }
  return previous == 1;
#endif
}

static inline uint32_t
hybrid_refcount_strong_count(hybrid_refcount_t *header) {
  if (!header)
    return 0;
#ifdef __cplusplus
  return header->strongCount.load(HYBRID_ACQUIRE);
#else
  return atomic_load_explicit(&header->strongCount, HYBRID_ACQUIRE);
#endif
}

static inline uint32_t hybrid_refcount_weak_count(
    hybrid_refcount_t *header) {
  if (!header)
    return 0;
#ifdef __cplusplus
  return header->weakCount.load(HYBRID_ACQUIRE);
#else
  return atomic_load_explicit(&header->weakCount, HYBRID_ACQUIRE);
#endif
}

const HybridTypeDescriptor *
hybrid_register_type_descriptor(const HybridTypeDescriptor *descriptor);
int hybrid_verify_descriptor_layout(void);

void hybrid_zero_weak_refs(void *object);
void hybrid_register_weak_slot(void *object, void **slot);
void hybrid_unregister_weak_slot(void *object, void **slot);

void hybrid_autorelease_pool_push(void);
void hybrid_autorelease_pool_pop(void);
void hybrid_autorelease_pool_drain(void);

void *hybrid_alloc_object(size_t totalSize,
                          const HybridTypeDescriptor *descriptor);
void *hybrid_alloc_array(size_t elementSize, size_t elementCount,
                         const HybridTypeDescriptor *descriptor);
void *hybrid_new_object(size_t totalSize,
                        const HybridTypeDescriptor *descriptor);
void *hybrid_new_array(size_t elementSize, size_t elementCount,
                       const HybridTypeDescriptor *descriptor);
void hybrid_free(void *obj);
void *hybrid_retain(void *obj);
void hybrid_release(void *obj);
void *hybrid_autorelease(void *obj);
void hybrid_dealloc(void *obj);

HybridSharedControlBlock *__hybrid_shared_control_create(void *payload);
void __hybrid_shared_control_retain_strong(HybridSharedControlBlock *control);
void __hybrid_shared_control_release_strong(HybridSharedControlBlock *control);
void __hybrid_shared_control_retain_weak(HybridSharedControlBlock *control);
void __hybrid_shared_control_release_weak(HybridSharedControlBlock *control);
void *__hybrid_shared_control_lock(HybridSharedControlBlock *control);
uint32_t __hybrid_shared_control_use_count(HybridSharedControlBlock *control);

#ifdef __cplusplus
} // extern "C"
#endif

#undef HYBRID_STATIC_ASSERT

#ifndef __cplusplus
#undef HYBRID_RELAXED
#undef HYBRID_ACQ_REL
#undef HYBRID_ACQUIRE
#endif

#endif // HYBRID_RUNTIME_H
