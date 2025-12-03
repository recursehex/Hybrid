// This file implements runtime support helpers for generated Hybrid code,
// including ARC-aware string utilities and interface dispatch.

#include "hybrid_runtime.h"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

extern "C" {

static bool decode_utf8_codepoint(const unsigned char *input, size_t length,
                                  size_t &index, uint32_t &codepoint) {
  const unsigned char first = input[index];
  if (first < 0x80) {
    codepoint = first;
    ++index;
    return true;
  }

  size_t width = 0;
  uint32_t minValue = 0;
  if ((first & 0xE0) == 0xC0) {
    width = 2;
    minValue = 0x80;
    codepoint = first & 0x1F;
  } else if ((first & 0xF0) == 0xE0) {
    width = 3;
    minValue = 0x800;
    codepoint = first & 0x0F;
  } else if ((first & 0xF8) == 0xF0) {
    width = 4;
    minValue = 0x10000;
    codepoint = first & 0x07;
  } else {
    return false;
  }

  if (index + width > length)
    return false;

  for (size_t i = 1; i < width; ++i) {
    unsigned char continuation = input[index + i];
    if ((continuation & 0xC0) != 0x80)
      return false;
    codepoint = static_cast<uint32_t>(codepoint << 6) |
                static_cast<uint32_t>(continuation & 0x3F);
  }

  if (codepoint < minValue || codepoint > 0x10FFFF)
    return false;
  if (codepoint >= 0xD800 && codepoint <= 0xDFFF)
    return false;

  index += width;
  return true;
}

static size_t utf16_length_from_utf8(const char *utf8, size_t bytes) {
  if (!utf8)
    return 0;
  size_t idx = 0;
  size_t codeUnits = 0;
  const unsigned char *data =
      reinterpret_cast<const unsigned char *>(utf8);
  while (idx < bytes) {
    uint32_t codepoint = 0;
    size_t cursor = idx;
    if (!decode_utf8_codepoint(data, bytes, cursor, codepoint))
      return 0;
    codeUnits += (codepoint >= 0x10000) ? 2 : 1;
    idx = cursor;
  }
  return codeUnits;
}

static HybridTypeDescriptor StringDescriptor = {
    "string", nullptr, nullptr, 0, nullptr, 0, nullptr};

static const HybridTypeDescriptor *get_string_descriptor() {
  static const HybridTypeDescriptor *canonical = nullptr;
  if (!canonical) {
    canonical = hybrid_register_type_descriptor(&StringDescriptor);
  }
  return canonical;
}

static void string_dealloc(void *obj) {
  if (!obj)
    return;
  auto *str = static_cast<hybrid_string_t *>(obj);
  if (str->backing) {
    hybrid_release(str->backing);
    str->backing = nullptr;
  }
  hybrid_dealloc(obj);
}

const HybridTypeDescriptor *hybrid_string_type_descriptor(void) {
  if (!StringDescriptor.dealloc)
    StringDescriptor.dealloc = string_dealloc;
  return get_string_descriptor();
}

static hybrid_string_t *allocate_string_storage(size_t byteLength,
                                                size_t utf16Length,
                                                size_t capacity) {
  const size_t inlineBytes = capacity > 0 ? capacity + 1 : 1;
  const size_t totalSize = sizeof(hybrid_string_t) + inlineBytes;
  auto *storage = static_cast<hybrid_string_t *>(
      hybrid_new_object(totalSize, hybrid_string_type_descriptor()));
  if (!storage)
    return nullptr;
  storage->length = utf16Length;
  storage->byteLength = byteLength;
  storage->capacity = capacity;
  storage->backing = nullptr;
  storage->data = reinterpret_cast<char *>(storage + 1);
  return storage;
}

static hybrid_string_t *allocate_view_storage() {
  auto *storage = static_cast<hybrid_string_t *>(
      hybrid_new_object(sizeof(hybrid_string_t),
                        hybrid_string_type_descriptor()));
  return storage;
}

static hybrid_string_t *make_string_from_utf8(const char *utf8, size_t bytes) {
  if (!utf8)
    return allocate_string_storage(0, 0, 0);
  const size_t utf16Len = utf16_length_from_utf8(utf8, bytes);
  auto *storage =
      allocate_string_storage(bytes, utf16Len, std::max<size_t>(bytes, 0));
  if (!storage)
    return nullptr;
  if (bytes > 0)
    std::memcpy(storage->data, utf8, bytes);
  if (storage->data)
    storage->data[bytes] = 0;
  return storage;
}

size_t hybrid_string_size(const hybrid_string_t *str) {
  if (!str)
    return 0;
  return str->length;
}

int hybrid_strlen(const hybrid_string_t *str) {
  return static_cast<int>(hybrid_string_size(str));
}

hybrid_string_t *__hybrid_string_from_utf8(const char *utf8, size_t length) {
  return make_string_from_utf8(utf8, length);
}

hybrid_string_t *__hybrid_string_from_utf8_literal(const char *utf8,
                                                   size_t length) {
  return make_string_from_utf8(utf8, length);
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
  return std::memcmp(lhs->data, rhs->data, lhs->byteLength) == 0 ? 1 : 0;
}

const void **hybrid_lookup_interface_table(const HybridTypeDescriptor *typeDesc,
                                           const HybridTypeDescriptor *interfaceDesc) {
  const HybridTypeDescriptor *current = typeDesc;
  while (current) {
    for (std::uint32_t idx = 0; idx < current->interfaceCount; ++idx) {
      const HybridInterfaceEntry &entry = current->interfaces[idx];
      if (entry.interfaceType == interfaceDesc)
        return entry.methodTable;
    }
    current = current->baseType;
  }
  return nullptr;
}

int __hybrid_debug_descriptor_matches(void *object,
                                      const hybrid_string_t *expectedName) {
  if (!object || !expectedName)
    return 0;
  auto *header = static_cast<hybrid_refcount_t *>(object);
  if (!header || !header->descriptor || !header->descriptor->typeName)
    return 0;
  const char *typeName = header->descriptor->typeName;
  const char *expectedUtf8 = expectedName->data;
  size_t expectedLen = expectedName->byteLength;
  if (!expectedUtf8)
    expectedLen = 0;
  if (std::strlen(typeName) != expectedLen)
    return 0;
  return std::memcmp(typeName, expectedUtf8, expectedLen) == 0 ? 1 : 0;
}

int __hybrid_debug_strong_count(void *object) {
  if (!object)
    return 0;
  auto *header = static_cast<hybrid_refcount_t *>(object);
  return static_cast<int>(hybrid_refcount_strong_count(header));
}

static void append_bytes(char *dest, size_t offset, const char *src,
                         size_t len) {
  if (len == 0)
    return;
  std::memcpy(dest + offset, src, len);
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
    totalBytes += seg->byteLength;
    totalUnits += seg->length;
  }

  auto *result =
      allocate_string_storage(totalBytes, totalUnits, totalBytes);
  if (!result)
    return nullptr;

  size_t offset = 0;
  for (int i = 0; i < count; ++i) {
    hybrid_string_t *seg = segments[i];
    if (!seg || seg->byteLength == 0)
      continue;
    append_bytes(result->data, offset, seg->data, seg->byteLength);
    offset += seg->byteLength;
  }
  if (result->data)
    result->data[result->byteLength] = 0;
  return result;
}

hybrid_string_t *__hybrid_string_from_int64(int64_t value, int isUnsigned) {
  char buffer[64];
  if (isUnsigned) {
    std::snprintf(buffer, sizeof(buffer), "%llu",
                  static_cast<unsigned long long>(value));
  } else {
    std::snprintf(buffer, sizeof(buffer), "%lld",
                  static_cast<long long>(value));
  }
  size_t len = std::strlen(buffer);
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
  std::snprintf(fmt, sizeof(fmt), "%%.%df", actual);

  char buffer[128];
  std::snprintf(buffer, sizeof(buffer), fmt, value);

  char *end = buffer + std::strlen(buffer) - 1;
  while (end > buffer && *end == '0')
    *end-- = '\0';
  if (end > buffer && *end == '.')
    *end = '\0';

  size_t len = std::strlen(buffer);
  return __hybrid_string_from_utf8_literal(buffer, len);
}

hybrid_string_t *__hybrid_string_from_char32(int32_t codepoint) {
  if (codepoint < 0)
    codepoint = 0;

  char buffer[8];
  size_t bytes = 0;
  size_t units = 1;
  uint32_t cp = static_cast<uint32_t>(codepoint);
  if (cp < 0x80) {
    buffer[bytes++] = static_cast<char>(cp);
  } else if (cp < 0x800) {
    buffer[bytes++] = static_cast<char>(0xC0 | (cp >> 6));
    buffer[bytes++] = static_cast<char>(0x80 | (cp & 0x3F));
  } else if (cp < 0x10000) {
    buffer[bytes++] = static_cast<char>(0xE0 | (cp >> 12));
    buffer[bytes++] = static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
    buffer[bytes++] = static_cast<char>(0x80 | (cp & 0x3F));
  } else {
    units = 2;
    buffer[bytes++] = static_cast<char>(0xF0 | (cp >> 18));
    buffer[bytes++] = static_cast<char>(0x80 | ((cp >> 12) & 0x3F));
    buffer[bytes++] = static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
    buffer[bytes++] = static_cast<char>(0x80 | (cp & 0x3F));
  }

  auto *storage = allocate_string_storage(bytes, units, bytes);
  if (!storage)
    return nullptr;
  std::memcpy(storage->data, buffer, bytes);
  storage->data[bytes] = 0;
  return storage;
}

static bool compute_slice_bounds(const hybrid_string_t *source, size_t start,
                                 size_t length, size_t &byteOffset,
                                 size_t &sliceBytes, size_t &sliceUnits) {
  if (!source) {
    byteOffset = 0;
    sliceBytes = 0;
    sliceUnits = 0;
    return true;
  }
  if (start > source->length)
    start = source->length;
  const size_t maxUnits = source->length;
  if (length > maxUnits - start)
    length = maxUnits - start;

  const unsigned char *data =
      reinterpret_cast<const unsigned char *>(source->data);
  size_t idx = 0;
  size_t unitsSeen = 0;
  byteOffset = 0;
  sliceBytes = 0;
  sliceUnits = length;

  while (idx < source->byteLength && unitsSeen < start) {
    uint32_t cp = 0;
    size_t cursor = idx;
    if (!decode_utf8_codepoint(data, source->byteLength, cursor, cp))
      return false;
    unitsSeen += (cp >= 0x10000) ? 2 : 1;
    idx = cursor;
  }
  byteOffset = idx;

  while (idx < source->byteLength && sliceUnits > 0) {
    uint32_t cp = 0;
    size_t cursor = idx;
    if (!decode_utf8_codepoint(data, source->byteLength, cursor, cp))
      return false;
    sliceBytes += (cursor - idx);
    sliceUnits -= 1;
    if (cp >= 0x10000 && sliceUnits > 0)
      sliceUnits -= 1;
    idx = cursor;
  }
  sliceUnits = length;
  return true;
}

hybrid_string_t *__hybrid_string_slice(hybrid_string_t *source, size_t start,
                                       size_t length) {
  size_t byteOffset = 0;
  size_t sliceBytes = 0;
  size_t sliceUnits = 0;
  if (!compute_slice_bounds(source, start, length, byteOffset, sliceBytes,
                            sliceUnits))
    return nullptr;

  auto *view = allocate_view_storage();
  if (!view)
    return nullptr;

  view->length = sliceUnits;
  view->byteLength = sliceBytes;
  view->capacity = sliceBytes;
  view->backing = source;
  view->data = source ? source->data + byteOffset : nullptr;
  if (source)
    hybrid_retain(source);
  return view;
}

hybrid_string_t *__hybrid_string_append_mut(hybrid_string_t *base,
                                            hybrid_string_t *suffix) {
  const size_t baseBytes = base ? base->byteLength : 0;
  const size_t baseUnits = base ? base->length : 0;
  const size_t suffixBytes = suffix ? suffix->byteLength : 0;
  const size_t suffixUnits = suffix ? suffix->length : 0;
  const size_t neededBytes = baseBytes + suffixBytes;
  const size_t neededUnits = baseUnits + suffixUnits;

  const bool baseIsUnique =
      base && base->backing == nullptr &&
      hybrid_refcount_strong_count(&base->counts) == 1;

  if (baseIsUnique && base->capacity >= neededBytes) {
    if (suffixBytes > 0)
      std::memcpy(base->data + baseBytes, suffix->data, suffixBytes);
    base->byteLength = neededBytes;
    base->length = neededUnits;
    base->data[neededBytes] = 0;
    return base;
  }

  auto *result =
      allocate_string_storage(neededBytes, neededUnits, neededBytes);
  if (!result)
    return nullptr;
  if (baseBytes > 0 && base)
    std::memcpy(result->data, base->data, baseBytes);
  if (suffixBytes > 0 && suffix)
    std::memcpy(result->data + baseBytes, suffix->data, suffixBytes);
  result->data[neededBytes] = 0;
  return result;
}

void print(int x) {
  std::printf("%d\n", x);
}

void print_string(hybrid_string_t *str) {
  if (!str || !str->data) {
    std::printf("(null)\n");
    return;
  }
  std::fwrite(str->data, 1, str->byteLength, stdout);
  std::printf("\n");
}

} // extern "C"
