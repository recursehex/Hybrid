#include "include/hybrid_runtime.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>

static HybridTypeDescriptor FallbackDescriptor = {
    "<anonymous>", NULL, NULL, 0, NULL, 0, NULL};
static HybridTypeDescriptor ArrayDescriptor = {
    "array", NULL, NULL, 0, NULL, 0, hybrid_array_dealloc};
static HybridTypeDescriptor StringDescriptor = {
    "string", NULL, NULL, 0, NULL, 0, NULL};

typedef struct {
  int strong;
  int weak;
  void *payload;
} HybridSharedControlForTests;

static int checked_add_size(size_t lhs, size_t rhs, size_t *out) {
  if (lhs > SIZE_MAX - rhs)
    return 0;
  *out = lhs + rhs;
  return 1;
}

// Descriptor helpers
const HybridTypeDescriptor *
hybrid_register_type_descriptor(const HybridTypeDescriptor *descriptor) {
  return descriptor;
}

int hybrid_verify_descriptor_layout(void) { return 1; }

// ARC debug stubs
int hybrid_debug_leaks = 0;
int hybrid_debug_reftrace = 0;
int hybrid_debug_verify = 0;
int hybrid_debug_pool = 0;

void hybrid_arc_set_debug_flags(int leakDetect, int refTrace, int verify,
                                int poolDebug) {
  (void)leakDetect;
  (void)refTrace;
  (void)verify;
  (void)poolDebug;
}

void hybrid_arc_trace_set_label(const char *label) { (void)label; }
void hybrid_arc_trace_flush(FILE *sink) { (void)sink; }
void hybrid_arc_trace_flush_default(void) {}
void hybrid_arc_dump_leaks(FILE *sink) { (void)sink; }

// Weak table stubs
void hybrid_zero_weak_refs(void *object) { (void)object; }
void hybrid_register_weak_slot(void *object, void **slot) {
  (void)object;
  (void)slot;
}
void hybrid_unregister_weak_slot(void *object, void **slot) {
  (void)object;
  (void)slot;
}

// Autorelease pool stubs
void hybrid_autorelease_pool_push(void) {}
void hybrid_autorelease_pool_pop(void) {}
void hybrid_autorelease_pool_drain(void) {}
void hybrid_autorelease_pool_scoped_debug(const char *label) { (void)label; }

// Array helpers
size_t hybrid_array_payload_offset(void) { return sizeof(hybrid_array_header_t); }

const HybridTypeDescriptor *hybrid_array_type_descriptor(void) {
  return &ArrayDescriptor;
}

void hybrid_array_set_release(void *obj, hybrid_array_release_fn releaseFn) {
  if (!obj)
    return;
  hybrid_array_header_t *header = (hybrid_array_header_t *)obj;
  header->elementRelease = releaseFn;
}

void hybrid_array_release_ref_slot(void *slot) {
  if (!slot)
    return;
  void *value = *(void **)slot;
  if (value)
    hybrid_release(value);
}

void hybrid_array_release_array_slot(void *slot) {
  if (!slot)
    return;
  void *payloadPtr = *(void **)slot;
  if (!payloadPtr)
    return;
  unsigned char *bytePtr = (unsigned char *)payloadPtr;
  bytePtr -= hybrid_array_payload_offset();
  hybrid_release((void *)bytePtr);
}

void hybrid_array_retain_ref_slot(void *slot) {
  if (!slot)
    return;
  void *value = *(void **)slot;
  if (value)
    hybrid_retain(value);
}

void hybrid_array_retain_array_slot(void *slot) {
  if (!slot)
    return;
  void *payloadPtr = *(void **)slot;
  if (!payloadPtr)
    return;
  unsigned char *bytePtr = (unsigned char *)payloadPtr;
  bytePtr -= hybrid_array_payload_offset();
  hybrid_retain((void *)bytePtr);
}

void *hybrid_array_resize(void *obj, size_t elementSize, size_t elementCount,
                          hybrid_array_release_fn releaseFn,
                          hybrid_array_retain_fn retainFn) {
  if (elementSize == 0)
    return NULL;
  if (obj) {
    hybrid_array_header_t *oldHeader = (hybrid_array_header_t *)obj;
    if (oldHeader->elementSize != elementSize)
      return NULL;
  }

  void *newObj = hybrid_alloc_array(elementSize, elementCount, NULL);
  if (!newObj)
    return NULL;

  if (releaseFn)
    hybrid_array_set_release(newObj, releaseFn);

  if (!obj || elementCount == 0)
    return newObj;

  hybrid_array_header_t *oldHeader = (hybrid_array_header_t *)obj;
  size_t copyCount = oldHeader->length < elementCount
                         ? oldHeader->length
                         : elementCount;
  if (copyCount == 0)
    return newObj;

  unsigned char *oldPayload =
      (unsigned char *)obj + hybrid_array_payload_offset();
  unsigned char *newPayload =
      (unsigned char *)newObj + hybrid_array_payload_offset();
  memcpy(newPayload, oldPayload, copyCount * elementSize);

  if (retainFn) {
    for (size_t idx = 0; idx < copyCount; ++idx)
      retainFn(newPayload + idx * elementSize);
  }

  return newObj;
}

// Interface lookup
const void **hybrid_lookup_interface_table(const HybridTypeDescriptor *typeDesc,
                                           const HybridTypeDescriptor *interfaceDesc) {
  const HybridTypeDescriptor *current = typeDesc;
  while (current) {
    if (current->interfaceCount > 0 && current->interfaces) {
      for (uint32_t idx = 0; idx < current->interfaceCount; ++idx) {
        const HybridInterfaceEntry *entry = &current->interfaces[idx];
        if (entry->interfaceType == interfaceDesc)
          return entry->methodTable;
      }
    }
    current = current->baseType;
  }
  return NULL;
}

// Debug helpers
int __hybrid_debug_descriptor_matches(void *object,
                                      const hybrid_string_t *expectedName) {
  if (!object || !expectedName)
    return 0;
  hybrid_refcount_t *header = (hybrid_refcount_t *)object;
  if (!header->descriptor || !header->descriptor->typeName)
    return 0;
  const char *typeName = header->descriptor->typeName;
  const char *expectedUtf8 = expectedName->data;
  size_t expectedLen = expectedName->byteLength;
  if (!expectedUtf8)
    expectedLen = 0;
  if (strlen(typeName) != expectedLen)
    return 0;
  return memcmp(typeName, expectedUtf8, expectedLen) == 0 ? 1 : 0;
}

int __hybrid_debug_strong_count(void *object) {
  if (!object)
    return 0;
  hybrid_refcount_t *header = (hybrid_refcount_t *)object;
  return (int)hybrid_refcount_strong_count(header);
}

// Allocation helpers
void *hybrid_alloc_object(size_t totalSize,
                          const HybridTypeDescriptor *descriptor) {
  if (totalSize == 0 || totalSize < sizeof(hybrid_refcount_t))
    return NULL;
  void *memory = calloc(1, totalSize);
  if (!memory)
    return NULL;
  hybrid_refcount_init((hybrid_refcount_t *)memory,
                       descriptor ? descriptor : &FallbackDescriptor);
  return memory;
}

void *hybrid_alloc_array(size_t elementSize, size_t elementCount,
                         const HybridTypeDescriptor *descriptor) {
  if (elementSize == 0)
    return NULL;
  if (elementCount > 0 && elementSize > SIZE_MAX / elementCount)
    return NULL;

  size_t payloadSize = elementSize * elementCount;
  size_t totalSize = 0;
  if (!checked_add_size(sizeof(hybrid_array_header_t), payloadSize, &totalSize))
    return NULL;
  hybrid_array_header_t *header =
      (hybrid_array_header_t *)calloc(1, totalSize);
  if (!header)
    return NULL;
  hybrid_refcount_init(&header->counts,
                       descriptor ? descriptor : hybrid_array_type_descriptor());
  header->length = elementCount;
  header->elementSize = elementSize;
  header->elementRelease = NULL;
  return (void *)header;
}

void *hybrid_new_object(size_t totalSize,
                        const HybridTypeDescriptor *descriptor) {
  return hybrid_alloc_object(totalSize, descriptor);
}

void *hybrid_new_array(size_t elementSize, size_t elementCount,
                       const HybridTypeDescriptor *descriptor) {
  return hybrid_alloc_array(elementSize, elementCount, descriptor);
}

void *hybrid_retain(void *obj) {
  if (!obj)
    return NULL;
  hybrid_refcount_retain_strong((hybrid_refcount_t *)obj);
  return obj;
}

void hybrid_release(void *obj) {
  if (!obj)
    return;
  hybrid_refcount_t *header = (hybrid_refcount_t *)obj;
  const HybridTypeDescriptor *desc = header->descriptor;
  if (hybrid_refcount_release_strong(header)) {
    hybrid_zero_weak_refs(obj);
    if (desc && desc->dealloc)
      desc->dealloc(obj);
    else
      hybrid_dealloc(obj);
  }
}

void *hybrid_autorelease(void *obj) { return obj; }

void hybrid_dealloc(void *obj) {
  if (!obj)
    return;
  hybrid_refcount_t *header = (hybrid_refcount_t *)obj;
  if (hybrid_refcount_release_weak(header)) {
    free(obj);
  }
}

void hybrid_free(void *obj) { hybrid_release(obj); }

void hybrid_array_dealloc(void *obj) {
  if (!obj)
    return;
  hybrid_array_header_t *header = (hybrid_array_header_t *)obj;
  if (header->elementRelease) {
    unsigned char *payload =
        ((unsigned char *)obj) + hybrid_array_payload_offset();
    for (size_t i = 0; i < header->length; ++i)
      header->elementRelease(payload + i * header->elementSize);
  }
  hybrid_dealloc(obj);
}

// Shared control block stubs for smart pointers
HybridSharedControlBlock *__hybrid_shared_control_create(void *payload) {
  if (!payload)
    return NULL;
  HybridSharedControlForTests *ctrl =
      (HybridSharedControlForTests *)calloc(1, sizeof(HybridSharedControlForTests));
  if (!ctrl)
    return NULL;
  ctrl->strong = 1;
  ctrl->weak = 1;
  ctrl->payload = payload;
  hybrid_retain(payload);
  return (HybridSharedControlBlock *)ctrl;
}

void __hybrid_shared_control_retain_strong(HybridSharedControlBlock *control) {
  HybridSharedControlForTests *ctrl = (HybridSharedControlForTests *)control;
  if (ctrl)
    ctrl->strong += 1;
}

void __hybrid_shared_control_release_strong(HybridSharedControlBlock *control) {
  HybridSharedControlForTests *ctrl = (HybridSharedControlForTests *)control;
  if (!ctrl)
    return;
  int previous = ctrl->strong;
  if (previous > 0)
    ctrl->strong -= 1;
  if (previous == 1) {
    void *payload = ctrl->payload;
    ctrl->payload = NULL;
    if (payload)
      hybrid_release(payload);
    __hybrid_shared_control_release_weak(control);
  }
}

void __hybrid_shared_control_retain_weak(HybridSharedControlBlock *control) {
  HybridSharedControlForTests *ctrl = (HybridSharedControlForTests *)control;
  if (ctrl)
    ctrl->weak += 1;
}

void __hybrid_shared_control_release_weak(HybridSharedControlBlock *control) {
  HybridSharedControlForTests *ctrl = (HybridSharedControlForTests *)control;
  if (!ctrl)
    return;
  if (ctrl->weak > 0)
    ctrl->weak -= 1;
  if (ctrl->weak == 0)
    free(ctrl);
}

void *__hybrid_shared_control_lock(HybridSharedControlBlock *control) {
  HybridSharedControlForTests *ctrl = (HybridSharedControlForTests *)control;
  if (!ctrl || ctrl->strong <= 0)
    return NULL;
  if (!ctrl->payload)
    return NULL;
  ctrl->strong += 1;
  return ctrl->payload;
}

uint32_t __hybrid_shared_control_use_count(HybridSharedControlBlock *control) {
  HybridSharedControlForTests *ctrl = (HybridSharedControlForTests *)control;
  if (!ctrl)
    return 0;
  return (uint32_t)ctrl->strong;
}

void __hybrid_shared_control_debug_dump(HybridSharedControlBlock *control,
                                        FILE *sink) {
  (void)control;
  (void)sink;
}

// String helpers
static void string_dealloc(void *obj);

const HybridTypeDescriptor *hybrid_string_type_descriptor(void) {
  if (!StringDescriptor.dealloc)
    StringDescriptor.dealloc = string_dealloc;
  return &StringDescriptor;
}

static int decode_utf8_codepoint(const unsigned char *input, size_t length,
                                 size_t *index, uint32_t *codepoint) {
  unsigned char first = input[*index];
  if (first < 0x80) {
    *codepoint = first;
    *index += 1;
    return 1;
  }

  size_t width = 0;
  uint32_t minValue = 0;
  if ((first & 0xE0) == 0xC0) {
    width = 2;
    minValue = 0x80;
    *codepoint = first & 0x1F;
  } else if ((first & 0xF0) == 0xE0) {
    width = 3;
    minValue = 0x800;
    *codepoint = first & 0x0F;
  } else if ((first & 0xF8) == 0xF0) {
    width = 4;
    minValue = 0x10000;
    *codepoint = first & 0x07;
  } else {
    return 0;
  }

  if (*index + width > length)
    return 0;

  for (size_t i = 1; i < width; ++i) {
    unsigned char continuation = input[*index + i];
    if ((continuation & 0xC0) != 0x80)
      return 0;
    *codepoint = (*codepoint << 6) | (continuation & 0x3F);
  }

  if (*codepoint < minValue || *codepoint > 0x10FFFF)
    return 0;
  if (*codepoint >= 0xD800 && *codepoint <= 0xDFFF)
    return 0;

  *index += width;
  return 1;
}

static size_t utf16_length_from_utf8(const char *utf8, size_t bytes) {
  if (!utf8)
    return 0;
  size_t idx = 0;
  size_t units = 0;
  const unsigned char *data = (const unsigned char *)utf8;
  while (idx < bytes) {
    uint32_t cp = 0;
    size_t cursor = idx;
    if (!decode_utf8_codepoint(data, bytes, &cursor, &cp))
      return 0;
    units += (cp >= 0x10000) ? 2 : 1;
    idx = cursor;
  }
  return units;
}

static hybrid_string_t *allocate_string_storage(size_t byteLength,
                                                size_t utf16Length,
                                                size_t capacity) {
  size_t inlineBytes = 1;
  if (capacity > 0 && !checked_add_size(capacity, 1, &inlineBytes))
    return NULL;
  size_t totalSize = 0;
  if (!checked_add_size(sizeof(hybrid_string_t), inlineBytes, &totalSize))
    return NULL;
  hybrid_string_t *storage = (hybrid_string_t *)hybrid_new_object(
      totalSize, hybrid_string_type_descriptor());
  if (!storage)
    return NULL;
  storage->length = utf16Length;
  storage->byteLength = byteLength;
  storage->capacity = capacity;
  storage->backing = NULL;
  storage->data = (char *)(storage + 1);
  return storage;
}

static hybrid_string_t *allocate_view_storage(void) {
  return (hybrid_string_t *)hybrid_new_object(
      sizeof(hybrid_string_t), hybrid_string_type_descriptor());
}

static hybrid_string_t *make_string_from_utf8(const char *utf8, size_t length) {
  if (!utf8)
    return allocate_string_storage(0, 0, 0);
  size_t utf16Len = utf16_length_from_utf8(utf8, length);
  hybrid_string_t *storage =
      allocate_string_storage(length, utf16Len, length);
  if (!storage)
    return NULL;
  if (length > 0)
    memcpy(storage->data, utf8, length);
  if (storage->data)
    storage->data[length] = 0;
  return storage;
}

size_t hybrid_string_size(const hybrid_string_t *str) {
  if (!str)
    return 0;
  return str->length;
}

int hybrid_strlen(const hybrid_string_t *str) {
  return (int)hybrid_string_size(str);
}

hybrid_string_t *__hybrid_string_from_utf8(const char *utf8, size_t length) {
  return make_string_from_utf8(utf8, length);
}

hybrid_string_t *__hybrid_string_from_utf8_literal(const char *utf8,
                                                   size_t length) {
  return make_string_from_utf8(utf8, length);
}

hybrid_string_t *__hybrid_concat_strings(hybrid_string_t **segments,
                                         int count) {
  if (!segments || count <= 0)
    return __hybrid_string_from_utf8_literal("", 0);

  size_t totalBytes = 0;
  size_t totalUnits = 0;
  for (int i = 0; i < count; ++i) {
    hybrid_string_t *seg = segments[i];
    if (!seg)
      continue;
    if (seg->byteLength > 0 && !seg->data)
      return NULL;
    if (!checked_add_size(totalBytes, seg->byteLength, &totalBytes))
      return NULL;
    if (!checked_add_size(totalUnits, seg->length, &totalUnits))
      return NULL;
  }

  hybrid_string_t *result =
      allocate_string_storage(totalBytes, totalUnits, totalBytes);
  if (!result)
    return NULL;

  size_t offset = 0;
  for (int i = 0; i < count; ++i) {
    hybrid_string_t *seg = segments[i];
    if (!seg || seg->byteLength == 0)
      continue;
    memcpy(result->data + offset, seg->data, seg->byteLength);
    offset += seg->byteLength;
  }
  if (result->data)
    result->data[result->byteLength] = 0;
  return result;
}

int __hybrid_string_equals(const hybrid_string_t *lhs,
                           const hybrid_string_t *rhs) {
  if (lhs == rhs)
    return 1;
  if (!lhs || !rhs)
    return 0;
  if (lhs->byteLength != rhs->byteLength)
    return 0;
  if (lhs->length != rhs->length)
    return 0;
  if (lhs->byteLength == 0)
    return 1;
  return memcmp(lhs->data, rhs->data, lhs->byteLength) == 0 ? 1 : 0;
}

hybrid_string_t *__hybrid_string_from_int64(int64_t value, int isUnsigned) {
  char buffer[64];
  if (isUnsigned)
    snprintf(buffer, sizeof(buffer), "%llu",
             (unsigned long long)value);
  else
    snprintf(buffer, sizeof(buffer), "%lld", (long long)value);
  size_t len = strlen(buffer);
  return __hybrid_string_from_utf8_literal(buffer, len);
}

hybrid_string_t *__hybrid_string_from_double(double value, int precision,
                                             int hasPrecision) {
  int actual = hasPrecision ? precision : 6;
  if (actual < 0)
    actual = 0;
  else if (actual > 12)
    actual = 12;

  char fmt[16];
  snprintf(fmt, sizeof(fmt), "%%.%df", actual);

  char buffer[128];
  snprintf(buffer, sizeof(buffer), fmt, value);

  char *end = buffer + strlen(buffer) - 1;
  while (end > buffer && *end == '0')
    *end-- = '\0';
  if (end > buffer && *end == '.')
    *end = '\0';

  size_t len = strlen(buffer);
  return __hybrid_string_from_utf8_literal(buffer, len);
}

static hybrid_decimal_t decimal_from_long_double(long double value) {
  hybrid_decimal_t out;
  out.lo = 0;
  out.hi = 0;
  size_t copySize = sizeof(value) < sizeof(out) ? sizeof(value) : sizeof(out);
  memcpy(&out, &value, copySize);
  return out;
}

static long double decimal_to_long_double(hybrid_decimal_t value) {
  long double out = 0.0L;
  size_t copySize = sizeof(out) < sizeof(value) ? sizeof(out) : sizeof(value);
  memcpy(&out, &value, copySize);
  return out;
}

hybrid_decimal_t __hybrid_decimal_parse(const char *text, size_t length) {
  if (!text)
    return decimal_from_long_double(0.0L);

  char *tmp = (char *)malloc(length + 1);
  if (!tmp)
    return decimal_from_long_double(0.0L);
  memcpy(tmp, text, length);
  tmp[length] = '\0';

  errno = 0;
  char *end = NULL;
  long double value = strtold(tmp, &end);
  int valid = (end == tmp + length) && (errno != ERANGE) && isfinite(value);
  free(tmp);
  if (!valid)
    return decimal_from_long_double(0.0L);
  return decimal_from_long_double(value);
}

hybrid_decimal_t __hybrid_decimal_add(hybrid_decimal_t lhs,
                                      hybrid_decimal_t rhs) {
  return decimal_from_long_double(decimal_to_long_double(lhs) +
                                  decimal_to_long_double(rhs));
}

hybrid_decimal_t __hybrid_decimal_sub(hybrid_decimal_t lhs,
                                      hybrid_decimal_t rhs) {
  return decimal_from_long_double(decimal_to_long_double(lhs) -
                                  decimal_to_long_double(rhs));
}

hybrid_decimal_t __hybrid_decimal_mul(hybrid_decimal_t lhs,
                                      hybrid_decimal_t rhs) {
  return decimal_from_long_double(decimal_to_long_double(lhs) *
                                  decimal_to_long_double(rhs));
}

hybrid_decimal_t __hybrid_decimal_div(hybrid_decimal_t lhs,
                                      hybrid_decimal_t rhs) {
  long double divisor = decimal_to_long_double(rhs);
  if (divisor == 0.0L)
    return decimal_from_long_double(0.0L);
  return decimal_from_long_double(decimal_to_long_double(lhs) / divisor);
}

hybrid_decimal_t __hybrid_decimal_rem(hybrid_decimal_t lhs,
                                      hybrid_decimal_t rhs) {
  long double divisor = decimal_to_long_double(rhs);
  if (divisor == 0.0L)
    return decimal_from_long_double(0.0L);
  return decimal_from_long_double(fmodl(decimal_to_long_double(lhs), divisor));
}

hybrid_decimal_t __hybrid_decimal_neg(hybrid_decimal_t value) {
  return decimal_from_long_double(-decimal_to_long_double(value));
}

int __hybrid_decimal_cmp(hybrid_decimal_t lhs, hybrid_decimal_t rhs) {
  long double left = decimal_to_long_double(lhs);
  long double right = decimal_to_long_double(rhs);
  if (left < right)
    return -1;
  if (left > right)
    return 1;
  return 0;
}

hybrid_decimal_t __hybrid_decimal_from_i64(int64_t value) {
  return decimal_from_long_double((long double)value);
}

hybrid_decimal_t __hybrid_decimal_from_u64(uint64_t value) {
  return decimal_from_long_double((long double)value);
}

int64_t __hybrid_decimal_to_i64(hybrid_decimal_t value) {
  long double v = truncl(decimal_to_long_double(value));
  if (!isfinite(v))
    return 0;
  if (v > (long double)INT64_MAX)
    return INT64_MAX;
  if (v < (long double)INT64_MIN)
    return INT64_MIN;
  return (int64_t)v;
}

uint64_t __hybrid_decimal_to_u64(hybrid_decimal_t value) {
  long double v = truncl(decimal_to_long_double(value));
  if (!isfinite(v))
    return 0;
  if (v <= 0.0L)
    return 0;
  if (v > (long double)UINT64_MAX)
    return UINT64_MAX;
  return (uint64_t)v;
}

hybrid_decimal_t __hybrid_decimal_from_double(double value) {
  return decimal_from_long_double((long double)value);
}

double __hybrid_decimal_to_double(hybrid_decimal_t value) {
  return (double)decimal_to_long_double(value);
}

hybrid_string_t *__hybrid_decimal_to_string(hybrid_decimal_t value,
                                            int precision, int hasPrecision) {
  int actual = hasPrecision ? precision : 34;
  if (actual < 0)
    actual = 0;
  else if (actual > 34)
    actual = 34;

  char fmt[16];
  if (hasPrecision)
    snprintf(fmt, sizeof(fmt), "%%.%dLf", actual);
  else
    snprintf(fmt, sizeof(fmt), "%%.%dLg", actual);

  char buffer[256];
  snprintf(buffer, sizeof(buffer), fmt, decimal_to_long_double(value));

  if (!strchr(buffer, 'e') && !strchr(buffer, 'E')) {
    char *end = buffer + strlen(buffer) - 1;
    while (end > buffer && *end == '0')
      *end-- = '\0';
    if (end > buffer && *end == '.')
      *end = '\0';
  }

  size_t len = strlen(buffer);
  return __hybrid_string_from_utf8_literal(buffer, len);
}

hybrid_string_t *__hybrid_string_from_char32(int32_t codepoint) {
  if (codepoint < 0)
    codepoint = 0;

  char buffer[8];
  size_t bytes = 0;
  size_t units = 1;
  uint32_t cp = (uint32_t)codepoint;
  if (cp < 0x80) {
    buffer[bytes++] = (char)cp;
  } else if (cp < 0x800) {
    buffer[bytes++] = (char)(0xC0 | (cp >> 6));
    buffer[bytes++] = (char)(0x80 | (cp & 0x3F));
  } else if (cp < 0x10000) {
    buffer[bytes++] = (char)(0xE0 | (cp >> 12));
    buffer[bytes++] = (char)(0x80 | ((cp >> 6) & 0x3F));
    buffer[bytes++] = (char)(0x80 | (cp & 0x3F));
  } else {
    units = 2;
    buffer[bytes++] = (char)(0xF0 | (cp >> 18));
    buffer[bytes++] = (char)(0x80 | ((cp >> 12) & 0x3F));
    buffer[bytes++] = (char)(0x80 | ((cp >> 6) & 0x3F));
    buffer[bytes++] = (char)(0x80 | (cp & 0x3F));
  }

  hybrid_string_t *storage =
      allocate_string_storage(bytes, units, bytes);
  if (!storage)
    return NULL;
  memcpy(storage->data, buffer, bytes);
  storage->data[bytes] = 0;
  return storage;
}

static int compute_slice_bounds(const hybrid_string_t *source, size_t start,
                                size_t length, size_t *byteOffset,
                                size_t *sliceBytes, size_t *sliceUnits) {
  if (!source) {
    *byteOffset = 0;
    *sliceBytes = 0;
    *sliceUnits = 0;
    return 1;
  }
  if (start > source->length)
    start = source->length;
  size_t maxUnits = source->length;
  if (length > maxUnits - start)
    length = maxUnits - start;

  const unsigned char *data = (const unsigned char *)source->data;
  size_t idx = 0;
  size_t unitsSeen = 0;
  *byteOffset = 0;
  *sliceBytes = 0;
  *sliceUnits = length;

  while (idx < source->byteLength && unitsSeen < start) {
    uint32_t cp = 0;
    size_t cursor = idx;
    if (!decode_utf8_codepoint(data, source->byteLength, &cursor, &cp))
      return 0;
    unitsSeen += (cp >= 0x10000) ? 2 : 1;
    idx = cursor;
  }
  *byteOffset = idx;

  size_t remainingUnits = length;
  while (idx < source->byteLength && remainingUnits > 0) {
    uint32_t cp = 0;
    size_t cursor = idx;
    if (!decode_utf8_codepoint(data, source->byteLength, &cursor, &cp))
      return 0;
    *sliceBytes += (cursor - idx);
    remainingUnits -= 1;
    if (cp >= 0x10000 && remainingUnits > 0)
      remainingUnits -= 1;
    idx = cursor;
  }
  *sliceUnits = length;
  return 1;
}

hybrid_string_t *__hybrid_string_slice(hybrid_string_t *source, size_t start,
                                       size_t length) {
  size_t byteOffset = 0;
  size_t sliceBytes = 0;
  size_t sliceUnits = 0;
  if (!compute_slice_bounds(source, start, length, &byteOffset, &sliceBytes,
                            &sliceUnits))
    return NULL;

  hybrid_string_t *view = allocate_view_storage();
  if (!view)
    return NULL;
  view->length = sliceUnits;
  view->byteLength = sliceBytes;
  view->capacity = sliceBytes;
  view->backing = source;
  view->data = source ? source->data + byteOffset : NULL;
  if (source)
    hybrid_retain(source);
  return view;
}

hybrid_string_t *__hybrid_string_append_mut(hybrid_string_t *base,
                                            hybrid_string_t *suffix) {
  size_t baseBytes = base ? base->byteLength : 0;
  size_t baseUnits = base ? base->length : 0;
  size_t suffixBytes = suffix ? suffix->byteLength : 0;
  size_t suffixUnits = suffix ? suffix->length : 0;
  if (baseBytes > 0 && (!base || !base->data))
    return NULL;
  if (suffixBytes > 0 && (!suffix || !suffix->data))
    return NULL;
  size_t neededBytes = 0;
  size_t neededUnits = 0;
  if (!checked_add_size(baseBytes, suffixBytes, &neededBytes))
    return NULL;
  if (!checked_add_size(baseUnits, suffixUnits, &neededUnits))
    return NULL;

  int baseIsUnique = base && base->backing == NULL &&
                     hybrid_refcount_strong_count(&base->counts) == 1;

  if (baseIsUnique && base->capacity >= neededBytes) {
    if (suffixBytes > 0)
      memcpy(base->data + baseBytes, suffix->data, suffixBytes);
    base->byteLength = neededBytes;
    base->length = neededUnits;
    base->data[neededBytes] = 0;
    return base;
  }

  hybrid_string_t *result =
      allocate_string_storage(neededBytes, neededUnits, neededBytes);
  if (!result)
    return NULL;
  if (baseBytes > 0 && base)
    memcpy(result->data, base->data, baseBytes);
  if (suffixBytes > 0 && suffix)
    memcpy(result->data + baseBytes, suffix->data, suffixBytes);
  result->data[neededBytes] = 0;
  return result;
}

static void string_dealloc(void *obj) {
  if (!obj)
    return;
  hybrid_string_t *str = (hybrid_string_t *)obj;
  if (str->backing) {
    hybrid_release(str->backing);
    str->backing = NULL;
  }
  hybrid_dealloc(obj);
}

// Printing helpers
void print(int x) { printf("%d\n", x); }

void print_string(hybrid_string_t *str) {
  if (!str || !str->data) {
    printf("(null)\n");
    return;
  }
  fwrite(str->data, 1, str->byteLength, stdout);
  printf("\n");
}
