#ifndef HYBRID_RUNTIME_H
#define HYBRID_RUNTIME_H

#ifdef __cplusplus
#include <atomic>
#include <cstddef>
#include <cstdint>
#include <cstdio>
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
#include <stdio.h>
#define HYBRID_STATIC_ASSERT _Static_assert
typedef _Atomic(uint32_t) hybrid_atomic_u32;
#define HYBRID_RELAXED memory_order_relaxed
#define HYBRID_ACQ_REL memory_order_acq_rel
#define HYBRID_ACQUIRE memory_order_acquire
#endif

typedef struct HybridTypeDescriptor HybridTypeDescriptor;
typedef struct HybridSharedControlBlock HybridSharedControlBlock;
typedef void (*hybrid_array_release_fn)(void *elementSlot);
typedef void (*hybrid_array_retain_fn)(void *elementSlot);

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

typedef struct hybrid_array_header_t {
  hybrid_refcount_t counts;
  size_t length;
  size_t elementSize;
  hybrid_array_release_fn elementRelease;
} hybrid_array_header_t;

typedef struct hybrid_string_t {
  hybrid_refcount_t counts;
  size_t length;       // UTF-16 code units
  size_t byteLength;   // UTF-8 byte length (without null terminator)
  size_t capacity;     // bytes available for writes (without null terminator)
  struct hybrid_string_t *backing; // retained backing storage for slices/views
  char *data;          // UTF-8 bytes; may alias backing->data for slices
} hybrid_string_t;

typedef struct hybrid_decimal_t {
  uint64_t lo;
  uint64_t hi;
} hybrid_decimal_t;

typedef struct HybridARCDebugConfig {
  int leakDetect;
  int refTrace;
  int verify;
  int poolDebug;
} HybridARCDebugConfig;

HYBRID_STATIC_ASSERT(sizeof(hybrid_atomic_u32) == sizeof(uint32_t),
                     "Refcount atomics must be lock-free u32");
HYBRID_STATIC_ASSERT(offsetof(hybrid_refcount_t, strongCount) == 0,
                     "ARC header strong count must be first field");
HYBRID_STATIC_ASSERT(
    offsetof(hybrid_refcount_t, weakCount) == sizeof(hybrid_atomic_u32),
    "ARC header weak count offset mismatch");
HYBRID_STATIC_ASSERT(
    offsetof(hybrid_array_header_t, counts) == 0,
    "Array header must start with ARC counts");
HYBRID_STATIC_ASSERT(sizeof(hybrid_array_header_t) >= sizeof(hybrid_refcount_t),
                     "Array header must wrap refcount header");
HYBRID_STATIC_ASSERT(offsetof(hybrid_string_t, counts) == 0,
                     "String storage must start with ARC counts");
HYBRID_STATIC_ASSERT(sizeof(hybrid_string_t) >= sizeof(hybrid_refcount_t),
                     "String storage must wrap refcount header");

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
void hybrid_autorelease_pool_scoped_debug(const char *label);

void *hybrid_alloc_object(size_t totalSize,
                          const HybridTypeDescriptor *descriptor);
void *hybrid_alloc_array(size_t elementSize, size_t elementCount,
                         const HybridTypeDescriptor *descriptor);
void *hybrid_new_object(size_t totalSize,
                        const HybridTypeDescriptor *descriptor);
void *hybrid_new_array(size_t elementSize, size_t elementCount,
                       const HybridTypeDescriptor *descriptor);
size_t hybrid_array_payload_offset(void);
const HybridTypeDescriptor *hybrid_array_type_descriptor(void);
void hybrid_array_set_release(void *obj, hybrid_array_release_fn releaseFn);
void hybrid_array_dealloc(void *obj);
void hybrid_array_release_ref_slot(void *slot);
void hybrid_array_release_array_slot(void *slot);
void hybrid_array_retain_ref_slot(void *slot);
void hybrid_array_retain_array_slot(void *slot);
void *hybrid_array_resize(void *obj, size_t elementSize, size_t elementCount,
                          hybrid_array_release_fn releaseFn,
                          hybrid_array_retain_fn retainFn);
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
void __hybrid_shared_control_debug_dump(HybridSharedControlBlock *control,
                                        FILE *sink);

// String runtime
const HybridTypeDescriptor *hybrid_string_type_descriptor(void);
hybrid_string_t *__hybrid_string_from_utf8(const char *utf8, size_t length);
hybrid_string_t *__hybrid_string_from_utf8_literal(const char *utf8,
                                                   size_t length);
hybrid_string_t *__hybrid_concat_strings(hybrid_string_t **segments, int count);
int __hybrid_string_equals(const hybrid_string_t *lhs,
                           const hybrid_string_t *rhs);
hybrid_string_t *__hybrid_string_from_int64(int64_t value, int isUnsigned);
hybrid_string_t *__hybrid_string_from_double(double value, int precision,
                                             int hasPrecision);
hybrid_string_t *__hybrid_decimal_to_string(hybrid_decimal_t value,
                                            int precision, int hasPrecision);
hybrid_string_t *__hybrid_string_from_char32(int32_t codepoint);
hybrid_string_t *__hybrid_string_slice(hybrid_string_t *source, size_t start,
                                       size_t length);
hybrid_string_t *__hybrid_string_append_mut(hybrid_string_t *base,
                                            hybrid_string_t *suffix);
size_t hybrid_string_size(const hybrid_string_t *str);
int hybrid_strlen(const hybrid_string_t *str);
void print_string(hybrid_string_t *str);

hybrid_decimal_t __hybrid_decimal_parse(const char *text, size_t length);
hybrid_decimal_t __hybrid_decimal_add(hybrid_decimal_t lhs,
                                      hybrid_decimal_t rhs);
hybrid_decimal_t __hybrid_decimal_sub(hybrid_decimal_t lhs,
                                      hybrid_decimal_t rhs);
hybrid_decimal_t __hybrid_decimal_mul(hybrid_decimal_t lhs,
                                      hybrid_decimal_t rhs);
hybrid_decimal_t __hybrid_decimal_div(hybrid_decimal_t lhs,
                                      hybrid_decimal_t rhs);
hybrid_decimal_t __hybrid_decimal_rem(hybrid_decimal_t lhs,
                                      hybrid_decimal_t rhs);
hybrid_decimal_t __hybrid_decimal_neg(hybrid_decimal_t value);
int __hybrid_decimal_cmp(hybrid_decimal_t lhs, hybrid_decimal_t rhs);

hybrid_decimal_t __hybrid_decimal_from_i64(int64_t value);
hybrid_decimal_t __hybrid_decimal_from_u64(uint64_t value);
int64_t __hybrid_decimal_to_i64(hybrid_decimal_t value);
uint64_t __hybrid_decimal_to_u64(hybrid_decimal_t value);
hybrid_decimal_t __hybrid_decimal_from_double(double value);
double __hybrid_decimal_to_double(hybrid_decimal_t value);

extern int hybrid_debug_leaks;
extern int hybrid_debug_reftrace;
extern int hybrid_debug_verify;
extern int hybrid_debug_pool;

void hybrid_arc_set_debug_flags(int leakDetect, int refTrace, int verify,
                                int poolDebug);
void hybrid_arc_trace_set_label(const char *label);
void hybrid_arc_trace_flush(FILE *sink);
void hybrid_arc_trace_flush_default(void);
void hybrid_arc_dump_leaks(FILE *sink);
void hybrid_arc_dump_leaks_default(void);
int hybrid_arc_verify_object(void *object);

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
