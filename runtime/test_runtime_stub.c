#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct HybridTypeDescriptor HybridTypeDescriptor;

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

typedef struct {
    uint32_t strongCount;
    uint32_t weakCount;
    const HybridTypeDescriptor *descriptor;
} HybridARCHeader;

static size_t hybrid_strlen16(const uint16_t *str) {
    if (!str) {
        return 0;
    }
    const uint16_t *p = str;
    while (*p != 0) {
        ++p;
    }
    return (size_t)(p - str);
}

int hybrid_strlen(const uint16_t *str) {
    return (int)hybrid_strlen16(str);
}

static uint16_t *alloc_utf16_buffer(size_t len) {
    uint16_t *buffer = (uint16_t *)malloc((len + 1) * sizeof(uint16_t));
    if (!buffer) {
        return NULL;
    }
    buffer[len] = 0;
    return buffer;
}

static uint16_t *dup_utf16_from_utf8(const char *utf8) {
    if (!utf8) {
        return NULL;
    }
    size_t len = strlen(utf8);
    uint16_t *dest = alloc_utf16_buffer(len);
    if (!dest) {
        return NULL;
    }
    for (size_t i = 0; i < len; ++i) {
        dest[i] = (unsigned char)utf8[i];
    }
    return dest;
}

static char *dup_utf8_from_utf16(const uint16_t *utf16) {
    if (!utf16) {
        return NULL;
    }
    size_t len = hybrid_strlen16(utf16);
    size_t max_bytes = len * 3 + 1;
    char *buffer = (char *)malloc(max_bytes);
    if (!buffer) {
        return NULL;
    }
    char *out = buffer;
    for (size_t i = 0; i < len; ++i) {
        uint16_t code = utf16[i];
        if (code < 0x80) {
            *out++ = (char)code;
        } else if (code < 0x800) {
            *out++ = (char)(0xC0 | (code >> 6));
            *out++ = (char)(0x80 | (code & 0x3F));
        } else {
            *out++ = (char)(0xE0 | (code >> 12));
            *out++ = (char)(0x80 | ((code >> 6) & 0x3F));
            *out++ = (char)(0x80 | (code & 0x3F));
        }
    }
    *out = '\0';
    return buffer;
}

void print(int x) {
    printf("%d\n", x);
}

void print_string(uint16_t *str) {
    if (!str) {
        printf("(null)\n");
        return;
    }
    char *utf8 = dup_utf8_from_utf16(str);
    if (!utf8) {
        printf("(null)\n");
        return;
    }
    printf("%s\n", utf8);
    free(utf8);
}

uint16_t *__hybrid_concat_strings(uint16_t **segments, int count) {
    if (!segments || count <= 0) {
        return dup_utf16_from_utf8("");
    }

    size_t total = 0;
    for (int i = 0; i < count; ++i) {
        total += hybrid_strlen16(segments[i]);
    }

    uint16_t *result = alloc_utf16_buffer(total);
    if (!result) {
        return NULL;
    }

    size_t offset = 0;
    for (int i = 0; i < count; ++i) {
        const uint16_t *segment = segments[i];
        if (!segment) {
            continue;
        }
        size_t len = hybrid_strlen16(segment);
        memcpy(result + offset, segment, len * sizeof(uint16_t));
        offset += len;
    }
    return result;
}

int __hybrid_string_equals(const uint16_t *lhs, const uint16_t *rhs) {
    if (lhs == rhs) {
        return 1;
    }
    if (!lhs || !rhs) {
        return 0;
    }

    while (*lhs != 0 && *rhs != 0) {
        if (*lhs != *rhs) {
            return 0;
        }
        ++lhs;
        ++rhs;
    }
    return *lhs == *rhs;
}

uint16_t *__hybrid_string_from_int64(int64_t value, int isUnsigned) {
    char buffer[64];
    if (isUnsigned) {
        unsigned long long v = (unsigned long long)value;
        snprintf(buffer, sizeof(buffer), "%llu", v);
    } else {
        long long v = (long long)value;
        snprintf(buffer, sizeof(buffer), "%lld", v);
    }
    return dup_utf16_from_utf8(buffer);
}

uint16_t *__hybrid_string_from_double(double value, int precision, int hasPrecision) {
    int actual = hasPrecision ? precision : 6;
    if (actual < 0) {
        actual = 0;
    } else if (actual > 12) {
        actual = 12;
    }

    char fmt[16];
    snprintf(fmt, sizeof(fmt), "%%.%df", actual);

    char buffer[128];
    snprintf(buffer, sizeof(buffer), fmt, value);

    // Trim trailing zeros and optional decimal point for cleaner output
    char *end = buffer + strlen(buffer) - 1;
    while (end > buffer && *end == '0') {
        *end-- = '\0';
    }
    if (end > buffer && *end == '.') {
        *end = '\0';
    }

    return dup_utf16_from_utf8(buffer);
}

uint16_t *__hybrid_string_from_char32(int32_t codepoint) {
    if (codepoint < 0) {
        codepoint = 0;
    }
    if (codepoint <= 0xFFFF) {
        uint16_t *result = alloc_utf16_buffer(1);
        if (!result) {
            return NULL;
        }
        result[0] = (uint16_t)codepoint;
        return result;
    }

    uint32_t value = (uint32_t)codepoint - 0x10000;
    uint16_t high = 0xD800 | ((value >> 10) & 0x3FF);
    uint16_t low = 0xDC00 | (value & 0x3FF);
    uint16_t *result = alloc_utf16_buffer(2);
    if (!result) {
        return NULL;
    }
    result[0] = high;
    result[1] = low;
    return result;
}

const void **hybrid_lookup_interface_table(const HybridTypeDescriptor *typeDesc,
                                           const HybridTypeDescriptor *interfaceDesc) {
    const HybridTypeDescriptor *current = typeDesc;
    while (current) {
        if (current->interfaceCount > 0 && current->interfaces) {
            for (uint32_t idx = 0; idx < current->interfaceCount; ++idx) {
                const HybridInterfaceEntry *entry = &current->interfaces[idx];
                if (entry->interfaceType == interfaceDesc) {
                    return entry->methodTable;
                }
            }
        }
        current = current->baseType;
    }
    return NULL;
}

int __hybrid_debug_descriptor_matches(void *object,
                                      const uint16_t *expectedName) {
    if (!object)
        return 0;
    HybridARCHeader *header = (HybridARCHeader *)object;
    if (!header || !header->descriptor || !header->descriptor->typeName)
        return 0;
    size_t len = hybrid_strlen16(expectedName);
    char *buffer = (char *)malloc(len + 1);
    if (!buffer)
        return 0;
    for (size_t i = 0; i < len; ++i)
        buffer[i] = (char)(expectedName[i] & 0xFF);
    buffer[len] = '\0';
    int matches = strcmp(header->descriptor->typeName, buffer) == 0;
    free(buffer);
    return matches;
}

int __hybrid_debug_strong_count(void *object) {
    if (!object)
        return 0;
    HybridARCHeader *header = (HybridARCHeader *)object;
    return header ? (int)header->strongCount : 0;
}

void hybrid_dealloc(void *obj);

void *hybrid_retain(void *obj) {
    if (!obj)
        return NULL;
    HybridARCHeader *header = (HybridARCHeader *)obj;
    header->strongCount += 1;
    return obj;
}

void hybrid_release(void *obj) {
    if (!obj)
        return;
    HybridARCHeader *header = (HybridARCHeader *)obj;
    if (header->strongCount > 0)
        header->strongCount -= 1;
    if (header->strongCount == 0) {
        const HybridTypeDescriptor *desc = header->descriptor;
        if (desc && desc->dealloc) {
            desc->dealloc(obj);
        } else {
            hybrid_dealloc(obj);
        }
    }
}

void *hybrid_autorelease(void *obj) { return obj; }

void hybrid_dealloc(void *obj) {
    if (!obj)
        return;
    HybridARCHeader *header = (HybridARCHeader *)obj;
    if (header->weakCount > 0)
        header->weakCount -= 1;
    if (header->weakCount == 0)
        free(obj);
}

typedef struct {
    int strong;
    int weak;
    void *payload;
} HybridSharedControlForTests;

void *__hybrid_shared_control_create(void *payload) {
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
    return ctrl;
}

void __hybrid_shared_control_retain_strong(void *control) {
    HybridSharedControlForTests *ctrl = (HybridSharedControlForTests *)control;
    if (ctrl)
        ctrl->strong += 1;
}

void __hybrid_shared_control_release_weak(void *control);

void __hybrid_shared_control_release_strong(void *control) {
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

void __hybrid_shared_control_retain_weak(void *control) {
    HybridSharedControlForTests *ctrl = (HybridSharedControlForTests *)control;
    if (ctrl)
        ctrl->weak += 1;
}

void __hybrid_shared_control_release_weak(void *control) {
    HybridSharedControlForTests *ctrl = (HybridSharedControlForTests *)control;
    if (!ctrl)
        return;
    if (ctrl->weak > 0)
        ctrl->weak -= 1;
    if (ctrl->weak == 0)
        free(ctrl);
}

void *__hybrid_shared_control_lock(void *control) {
    HybridSharedControlForTests *ctrl = (HybridSharedControlForTests *)control;
    if (!ctrl || ctrl->strong <= 0)
        return NULL;
    if (!ctrl->payload)
        return NULL;
    ctrl->strong += 1;
    hybrid_retain(ctrl->payload);
    return ctrl->payload;
}

int __hybrid_shared_control_use_count(void *control) {
    HybridSharedControlForTests *ctrl = (HybridSharedControlForTests *)control;
    if (!ctrl)
        return 0;
    return ctrl->strong;
}
