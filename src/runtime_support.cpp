// This file implements runtime support helpers for generated Hybrid code, including string utilities and interface dispatch.

#include "hybrid_runtime.h"

#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

extern "C" {

static uint16_t *dup_utf16_from_utf8(const char *utf8);
static char *dup_utf8_from_utf16(const uint16_t *utf16);

static size_t hybrid_strlen16(const uint16_t *str) {
  if (!str)
    return 0;

  const uint16_t *p = str;
  while (*p != 0)
    ++p;
  return static_cast<size_t>(p - str);
}

int hybrid_strlen(const uint16_t *str) {
  if (!str)
    return 0;
  return static_cast<int>(hybrid_strlen16(str));
}

int __hybrid_string_equals(const uint16_t *lhs, const uint16_t *rhs) {
  if (lhs == rhs)
    return 1;
  if (!lhs || !rhs)
    return 0;

  const uint16_t *left = lhs;
  const uint16_t *right = rhs;
  while (*left != 0 && *right != 0) {
    if (*left != *right)
      return 0;
    ++left;
    ++right;
  }
  return *left == *right;
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
                                      const uint16_t *expectedName) {
  if (!object)
    return 0;
  auto *header = static_cast<hybrid_refcount_t *>(object);
  if (!header || !header->descriptor || !header->descriptor->typeName)
    return 0;
  char *expectedUtf8 = dup_utf8_from_utf16(expectedName);
  if (!expectedUtf8)
    return 0;
  bool matches =
      std::strcmp(header->descriptor->typeName, expectedUtf8) == 0;
  std::free(expectedUtf8);
  return matches ? 1 : 0;
}

int __hybrid_debug_strong_count(void *object) {
  if (!object)
    return 0;
  auto *header = static_cast<hybrid_refcount_t *>(object);
  return static_cast<int>(hybrid_refcount_strong_count(header));
}

static uint16_t *alloc_utf16_buffer(size_t len) {
  auto *buffer = static_cast<uint16_t *>(std::malloc((len + 1) * sizeof(uint16_t)));
  if (!buffer)
    return nullptr;
  buffer[len] = 0;
  return buffer;
}

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

static uint16_t *dup_utf16_from_utf8(const char *utf8) {
  if (!utf8)
    return nullptr;

  const unsigned char *bytes =
      reinterpret_cast<const unsigned char *>(utf8);
  size_t utf8_len = std::strlen(utf8);
  size_t index = 0;
  size_t total_units = 0;

  while (index < utf8_len) {
    uint32_t codepoint = 0;
    size_t cursor = index;
    if (!decode_utf8_codepoint(bytes, utf8_len, cursor, codepoint))
      return nullptr;
    total_units += (codepoint >= 0x10000) ? 2 : 1;
    index = cursor;
  }

  auto *dest = alloc_utf16_buffer(total_units);
  if (!dest)
    return nullptr;

  index = 0;
  size_t out = 0;
  while (index < utf8_len) {
    uint32_t codepoint = 0;
    if (!decode_utf8_codepoint(bytes, utf8_len, index, codepoint)) {
      std::free(dest);
      return nullptr;
    }

    if (codepoint >= 0x10000) {
      uint32_t value = codepoint - 0x10000;
      dest[out++] = static_cast<uint16_t>(0xD800 | ((value >> 10) & 0x3FF));
      dest[out++] = static_cast<uint16_t>(0xDC00 | (value & 0x3FF));
    } else {
      dest[out++] = static_cast<uint16_t>(codepoint);
    }
  }
  return dest;
}

static bool decode_utf16_codepoint(const uint16_t *input, size_t length,
                                   size_t &index, uint32_t &codepoint) {
  uint16_t lead = input[index];
  if (lead >= 0xD800 && lead <= 0xDBFF) {
    if (index + 1 >= length)
      return false;
    uint16_t trail = input[index + 1];
    if (trail < 0xDC00 || trail > 0xDFFF)
      return false;
    uint32_t high = static_cast<uint32_t>(lead - 0xD800);
    uint32_t low = static_cast<uint32_t>(trail - 0xDC00);
    codepoint = (high << 10 | low) + 0x10000;
    index += 2;
    return true;
  }

  if (lead >= 0xDC00 && lead <= 0xDFFF)
    return false;

  codepoint = lead;
  ++index;
  return true;
}

static char *dup_utf8_from_utf16(const uint16_t *utf16) {
  if (!utf16)
    return nullptr;

  size_t len = hybrid_strlen16(utf16);
  size_t index = 0;
  size_t total_bytes = 0;

  while (index < len) {
    uint32_t codepoint = 0;
    if (!decode_utf16_codepoint(utf16, len, index, codepoint))
      return nullptr;

    if (codepoint < 0x80)
      total_bytes += 1;
    else if (codepoint < 0x800)
      total_bytes += 2;
    else if (codepoint < 0x10000)
      total_bytes += 3;
    else
      total_bytes += 4;
  }

  auto *buffer = static_cast<char *>(std::malloc(total_bytes + 1));
  if (!buffer)
    return nullptr;

  char *out = buffer;
  index = 0;

  while (index < len) {
    uint32_t codepoint = 0;
    if (!decode_utf16_codepoint(utf16, len, index, codepoint)) {
      std::free(buffer);
      return nullptr;
    }

    if (codepoint < 0x80) {
      *out++ = static_cast<char>(codepoint);
    } else if (codepoint < 0x800) {
      *out++ = static_cast<char>(0xC0 | (codepoint >> 6));
      *out++ = static_cast<char>(0x80 | (codepoint & 0x3F));
    } else if (codepoint < 0x10000) {
      *out++ = static_cast<char>(0xE0 | (codepoint >> 12));
      *out++ = static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F));
      *out++ = static_cast<char>(0x80 | (codepoint & 0x3F));
    } else {
      *out++ = static_cast<char>(0xF0 | (codepoint >> 18));
      *out++ = static_cast<char>(0x80 | ((codepoint >> 12) & 0x3F));
      *out++ = static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F));
      *out++ = static_cast<char>(0x80 | (codepoint & 0x3F));
    }
  }
  *out = '\0';
  return buffer;
}

void print(int x) {
  std::printf("%d\n", x);
}

void print_string(uint16_t *str) {
  if (!str) {
    std::printf("(null)\n");
    return;
  }
  char *utf8 = dup_utf8_from_utf16(str);
  if (!utf8) {
    std::printf("(null)\n");
    return;
  }
  std::printf("%s\n", utf8);
  std::free(utf8);
}

uint16_t *__hybrid_concat_strings(uint16_t **segments, int count) {
  if (!segments || count <= 0)
    return dup_utf16_from_utf8("");

  size_t total = 0;
  for (int i = 0; i < count; ++i)
    total += hybrid_strlen16(segments[i]);

  uint16_t *result = alloc_utf16_buffer(total);
  if (!result)
    return nullptr;

  size_t offset = 0;
  for (int i = 0; i < count; ++i) {
    const uint16_t *segment = segments[i];
    if (!segment)
      continue;
    size_t len = hybrid_strlen16(segment);
    std::memcpy(result + offset, segment, len * sizeof(uint16_t));
    offset += len;
  }
  return result;
}

uint16_t *__hybrid_string_from_int64(int64_t value, int isUnsigned) {
  char buffer[64];
  if (isUnsigned) {
    std::snprintf(buffer, sizeof(buffer), "%llu",
                  static_cast<unsigned long long>(value));
  } else {
    std::snprintf(buffer, sizeof(buffer), "%lld",
                  static_cast<long long>(value));
  }
  return dup_utf16_from_utf8(buffer);
}

uint16_t *__hybrid_string_from_double(double value, int precision, int hasPrecision) {
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

  return dup_utf16_from_utf8(buffer);
}

uint16_t *__hybrid_string_from_char32(int32_t codepoint) {
  if (codepoint < 0)
    codepoint = 0;
  if (codepoint <= 0xFFFF) {
    uint16_t *result = alloc_utf16_buffer(1);
    if (!result)
      return nullptr;
    result[0] = static_cast<uint16_t>(codepoint);
    return result;
  }

  uint32_t value = static_cast<uint32_t>(codepoint) - 0x10000;
  uint16_t high = static_cast<uint16_t>(0xD800 | ((value >> 10) & 0x3FF));
  uint16_t low = static_cast<uint16_t>(0xDC00 | (value & 0x3FF));
  uint16_t *result = alloc_utf16_buffer(2);
  if (!result)
    return nullptr;
  result[0] = high;
  result[1] = low;
  return result;
}

} // extern "C"
