#!/bin/bash

# Hybrid Compiler Test Runner
# Automatically discovers and runs all test files in the test/ directory

set -e  # Exit on any error

START_TIME=$(date +%s)
TIMED_DURATION=0
TIMED_TESTS=0

START_TIME=$(date +%s)

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Progress tracking (used for failures-only mode)
PROGRESS_BAR_WIDTH=40
PROGRESS_COUNT=0
PROGRESS_TOTAL=0
PROGRESS_VISIBLE=0

ensure_progress_newline() {
    if [ $PROGRESS_VISIBLE -eq 1 ]; then
        printf "\n"
        PROGRESS_VISIBLE=0
    fi
}

update_progress_display() {
    if [ $FAILURES_ONLY -ne 1 ]; then
        return
    fi
    if [ $PROGRESS_TOTAL -le 0 ]; then
        return
    fi

    if [ $PROGRESS_COUNT -gt $PROGRESS_TOTAL ]; then
        PROGRESS_COUNT=$PROGRESS_TOTAL
    fi

    local completed_chars=$((PROGRESS_COUNT * PROGRESS_BAR_WIDTH / PROGRESS_TOTAL))
    local remaining_chars=$((PROGRESS_BAR_WIDTH - completed_chars))

    printf -v hashes '%*s' "$completed_chars" ''
    hashes=${hashes// /#}
    printf -v spaces '%*s' "$remaining_chars" ''
    spaces=${spaces// /.}

    printf "\rProgress: [%s%s] %d/%d" "$hashes" "$spaces" "$PROGRESS_COUNT" "$PROGRESS_TOTAL"
    PROGRESS_VISIBLE=1
}

mark_test_completed() {
    if [ $FAILURES_ONLY -ne 1 ]; then
        TOTAL_TESTS=$((TOTAL_TESTS + 1))
        return
    fi

    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    PROGRESS_COUNT=$((PROGRESS_COUNT + 1))
    update_progress_display
}

# Initialize command line option variables early to avoid unset variable errors
VERBOSE_MODE=0
FAILURES_ONLY=1
TEST_PATTERN=""
RUN_MULTI_UNIT_TESTS=1
MULTI_UNIT_FILTER=""
EXTRA_COMPILER_ARGS=()
FORCE_VERBOSE_FROM_ENV=0
VERBOSE_FROM_ARGS=0
ARC_RUNTIME_REQUIRED=0

echo -e "${BLUE}Hybrid Compiler Test Suite${NC}"
echo "==============================="

# Check for hybrid executable - try multiple locations
HYBRID_EXEC=""
if [ -f "./build/hybrid" ]; then
    HYBRID_EXEC="./build/hybrid"
elif [ -f "./hybrid" ]; then
    HYBRID_EXEC="./hybrid"
elif [ -f "./cmake-build-debug/hybrid" ]; then
    HYBRID_EXEC="./cmake-build-debug/hybrid"
elif [ -f "./cmake-build-release/hybrid" ]; then
    HYBRID_EXEC="./cmake-build-release/hybrid"
else
    echo -e "${RED}Error: hybrid executable not found.${NC}"
    echo -e "${RED}Build the project first using:${NC}"
    echo -e "${RED}  cmake -B build && cmake --build build${NC}"
    echo -e "${RED}Or use the build script:${NC}"
    echo -e "${RED}  ./build.sh${NC}"
    exit 1
fi

echo "Using executable: $HYBRID_EXEC"

if [[ -n "${HYBRID_SHOW_GENERIC_METRICS+x}" ]]; then
    value=$(printf "%s" "$HYBRID_SHOW_GENERIC_METRICS" | tr -d '[:space:]')
    lower=$(printf "%s" "$value" | tr '[:upper:]' '[:lower:]')
    if [[ -z "$lower" || ( "$lower" != "0" && "$lower" != "false" && "$lower" != "off" ) ]]; then
        EXTRA_COMPILER_ARGS+=("--diagnostics" "generics")
        FORCE_VERBOSE_FROM_ENV=1
    fi
fi

if [[ -n "${HYBRID_ARC_DEBUG+x}" ]]; then
    value=$(printf "%s" "$HYBRID_ARC_DEBUG" | tr -d '[:space:]')
    lower=$(printf "%s" "$value" | tr '[:upper:]' '[:lower:]')
    if [[ -z "$lower" || ( "$lower" != "0" && "$lower" != "false" && "$lower" != "off" ) ]]; then
        EXTRA_COMPILER_ARGS+=("--arc-debug")
        ARC_RUNTIME_REQUIRED=1
        FORCE_VERBOSE_FROM_ENV=1
    fi
fi

if [[ -n "${HYBRID_ARC_TRACE_RUNTIME+x}" ]]; then
    value=$(printf "%s" "$HYBRID_ARC_TRACE_RUNTIME" | tr -d '[:space:]')
    lower=$(printf "%s" "$value" | tr '[:upper:]' '[:lower:]')
    if [[ -z "$lower" || ( "$lower" != "0" && "$lower" != "false" && "$lower" != "off" ) ]]; then
        EXTRA_COMPILER_ARGS+=("--arc-trace-retains")
        ARC_RUNTIME_REQUIRED=1
    fi
fi

if [[ -n "${HYBRID_ARC_LEAK_DETECT+x}" ]]; then
    value=$(printf "%s" "$HYBRID_ARC_LEAK_DETECT" | tr -d '[:space:]')
    lower=$(printf "%s" "$value" | tr '[:upper:]' '[:lower:]')
    if [[ -z "$lower" || ( "$lower" != "0" && "$lower" != "false" && "$lower" != "off" ) ]]; then
        EXTRA_COMPILER_ARGS+=("--arc-leak-detect")
        ARC_RUNTIME_REQUIRED=1
    fi
fi

if [[ -n "${HYBRID_ARC_VERIFY_RUNTIME+x}" ]]; then
    value=$(printf "%s" "$HYBRID_ARC_VERIFY_RUNTIME" | tr -d '[:space:]')
    lower=$(printf "%s" "$value" | tr '[:upper:]' '[:lower:]')
    if [[ -z "$lower" || ( "$lower" != "0" && "$lower" != "false" && "$lower" != "off" ) ]]; then
        EXTRA_COMPILER_ARGS+=("--arc-verify-runtime")
        ARC_RUNTIME_REQUIRED=1
    fi
fi

# Create runtime library for test execution if clang is available
RUNTIME_LIB=""
if command -v clang &> /dev/null; then
    RUNTIME_LIB=$(mktemp /tmp/hybrid_runtime.XXXXXX)
    mv "$RUNTIME_LIB" "${RUNTIME_LIB}.o"
    RUNTIME_LIB="${RUNTIME_LIB}.o"
    cat > "${RUNTIME_LIB%.o}.c" << 'EOF'
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

EOF
    clang -c "${RUNTIME_LIB%.o}.c" -o "$RUNTIME_LIB" 2>/dev/null
fi

# Cleanup runtime library on exit
cleanup_runtime() {
    if [ -n "$RUNTIME_LIB" ]; then
        rm -f "$RUNTIME_LIB" "${RUNTIME_LIB%.o}.c"
    fi
}
trap cleanup_runtime EXIT

recount_multi_unit() {
    local filter="$1"
    local count=0
    if [ -d "test/multi_unit" ]; then
        while IFS= read -r -d '' multi_dir; do
            if [ -n "$filter" ] && [ "$(basename "$multi_dir")" != "$filter" ]; then
                continue
            fi
            if find "$multi_dir" -maxdepth 1 -name "*.hy" -type f | grep -q .; then
                count=$((count + 1))
            fi
        done < <(find test/multi_unit -mindepth 1 -maxdepth 1 -type d -print0)
    fi
    echo "$count"
}

# Find all test files in test/ directory and subdirectories
TEST_FILES=$(find test/ -path 'test/multi_unit' -prune -o -name "*.hy" -type f -print | sort)

if [ -z "$TEST_FILES" ]; then
    echo -e "${RED}No test files found in test/ directory${NC}"
    exit 1
fi

MULTI_UNIT_TEST_COUNT=$(recount_multi_unit "$MULTI_UNIT_FILTER")

# Count tests by category
echo "Test categories:"
for dir in test/*/; do
    if [ -d "$dir" ]; then
        category=$(basename "$dir")
        if [ "$category" = "multi_unit" ]; then
            count=$MULTI_UNIT_TEST_COUNT
        else
            count=$(find "$dir" -name "*.hy" -type f | wc -l | tr -d '[:space:]')
        fi
        if [ $count -gt 0 ]; then
            echo "  - $category: $count tests"
        fi
    fi
done
echo

# Function to run a single test
run_test() {
    local test_file="$1"
    local test_name=$(basename "$test_file" .hy)
    local test_wall_start=$(date +%s)
    local counts_for_timing=1
    if [[ "$test_name" == *"fail"* ]] || [[ "$test_name" == *"error"* ]]; then
        counts_for_timing=0
    fi
    local run_opts=()
    while IFS= read -r line; do
        line=${line#"// RUN_OPTS:"}
        line=$(echo "$line" | sed -e 's/^[[:space:]]*//')
        if [ -n "$line" ]; then
            read -r -a parts <<< "$line"
            run_opts+=("${parts[@]}")
        fi
    done < <(grep -E "^// RUN_OPTS:" "$test_file" || true)
    
    # Run the test and capture both output and exit code
    local output
    local exit_code
    output=$("$HYBRID_EXEC" "${EXTRA_COMPILER_ARGS[@]}" "${run_opts[@]}" < "$test_file" 2>&1)
    exit_code=$?
    
    # Check for error patterns in output
    local has_errors=0
    if grep -q "Error" <<< "$output" 2>/dev/null; then
        has_errors=1
    fi
    if grep -q "Failed to generate" <<< "$output" 2>/dev/null; then
        has_errors=1
    fi
    if grep -q "Unknown function" <<< "$output" 2>/dev/null; then
        has_errors=1
    fi
    if grep -q "Unknown variable" <<< "$output" 2>/dev/null; then
        has_errors=1
    fi
    if grep -q "invalid binary operator" <<< "$output" 2>/dev/null; then
        has_errors=1
    fi
    if grep -q "Binary operator" <<< "$output" 2>/dev/null; then
        has_errors=1
    fi
    if grep -q "Expected.*after" <<< "$output" 2>/dev/null; then
        has_errors=1
    fi

    local expected_output=()
    while IFS= read -r line; do
        line=${line#"// EXPECT_OUTPUT:"}
        line=$(echo "$line" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
        if [ -n "$line" ]; then
            expected_output+=("$line")
        fi
    done < <(grep -E "^// EXPECT_OUTPUT:" "$test_file" || true)

    local expected_runtime=()
    while IFS= read -r line; do
        line=${line#"// EXPECT_RUNTIME:"}
        line=$(echo "$line" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
        if [ -n "$line" ]; then
            expected_runtime+=("$line")
        fi
    done < <(grep -E "^// EXPECT_RUNTIME:" "$test_file" || true)

    for expected in "${expected_output[@]}"; do
        if ! grep -F -q "$expected" <<< "$output"; then
            has_errors=1
            output="${output}"$'\n'"[test-expectation] missing: ${expected}"
        fi
    done

    # If compilation succeeded and test is not expected to fail, compile and run with clang
    local runtime_exit_code=0
    local runtime_output=""
    if [ $has_errors -eq 0 ] && [ $exit_code -eq 0 ] && [[ "$test_name" != *"fail"* ]] && [[ "$test_name" != *"error"* ]]; then
        # Check if clang is available and we have runtime library
        if command -v clang &> /dev/null && [ -n "$RUNTIME_LIB" ] && [ -f "$RUNTIME_LIB" ]; then
            # Extract the final complete LLVM module (after "=== Final Generated LLVM IR ===")
            local module_marker=$(echo "$output" | grep -n "^=== Final Generated" | tail -1 | cut -d: -f1)
            local module_start=""
            if [ -n "$module_marker" ]; then
                module_start=$((module_marker + 1))
            fi

            if [ -n "$module_start" ]; then
                local clean_ir=$(echo "$output" | tail -n +$module_start | grep -v "^ready>" | grep -v "^Parsed" | grep -v "^Generated" | grep -v "^\\[generics" | grep -v "^\\[arc-trace\\]" | grep -v "^\\[arc-escape\\]")

                local temp_ir=$(mktemp /tmp/hybrid_test.XXXXXX)
                mv "$temp_ir" "${temp_ir}.ll"
                temp_ir="${temp_ir}.ll"
                local temp_bin=$(mktemp /tmp/hybrid_bin.XXXXXX)
                echo "$clean_ir" > "$temp_ir"

                if grep -q "define i32 @main" "$temp_ir"; then
                    # Check if smart pointer runtime is needed
                    local needs_smart_ptr_runtime=$ARC_RUNTIME_REQUIRED
                    for opt in "${run_opts[@]}"; do
                        case "$opt" in
                            --arc-debug|--arc-leak-detect|--arc-verify-runtime|--arc-trace-retains)
                                needs_smart_ptr_runtime=1
                                ;;
                        esac
                    done
                    if grep -q "__hybrid_shared_control\|__hybrid_smart_" "$temp_ir"; then
                        needs_smart_ptr_runtime=1
                    elif grep -q "hybrid_release\|hybrid_retain\|hybrid_autorelease\|hybrid_alloc_array\|hybrid_alloc_object" "$temp_ir"; then
                        needs_smart_ptr_runtime=1
                    fi

                    local clang_success=0
                    if [ $needs_smart_ptr_runtime -eq 1 ]; then
                        # Smart pointer tests use C++ runtime - don't include stub runtime library
                        if clang++ "$temp_ir" src/runtime_support.cpp src/runtime/arc.cpp src/runtime/weak_table.cpp src/memory/ref_count.cpp -Isrc -Iruntime/include -std=c++17 -o "$temp_bin" 2>/dev/null; then
                            clang_success=1
                        fi
                    else
                        if clang++ "$temp_ir" "$RUNTIME_LIB" -o "$temp_bin" &> /dev/null; then
                            clang_success=1
                        fi
                    fi

                    if [ $clang_success -eq 1 ]; then
                        set +e
                        runtime_output=$("$temp_bin" 2>&1)
                        runtime_exit_code=$?
                        set -e

                        if [ $runtime_exit_code -ne 0 ]; then
                            has_errors=1
                        fi
                    else
                        has_errors=1
                    fi
                fi

                rm -f "$temp_ir" "$temp_bin"
            fi
        fi
    fi

    # For fail/error tests that compiled cleanly, run the generated program to
    # detect failure modes like non-zero exit codes.
    if [[ "$test_name" == *"fail"* ]] || [[ "$test_name" == *"error"* ]]; then
        if [ $has_errors -eq 0 ] && [ $exit_code -eq 0 ]; then
            if command -v clang &> /dev/null && [ -n "$RUNTIME_LIB" ] && [ -f "$RUNTIME_LIB" ]; then
                local module_marker=$(echo "$output" | grep -n "^=== Final Generated" | tail -1 | cut -d: -f1)
                local module_start=""
                if [ -n "$module_marker" ]; then
                    module_start=$((module_marker + 1))
                fi

                if [ -n "$module_start" ]; then
                    local clean_ir=$(echo "$output" | tail -n +$module_start | grep -v "^ready>" | grep -v "^Parsed" | grep -v "^Generated" | grep -v "^\\[generics" | grep -v "^\\[arc-trace\\]" | grep -v "^\\[arc-escape\\]")

                    local temp_ir=$(mktemp /tmp/hybrid_test_fail.XXXXXX)
                    mv "$temp_ir" "${temp_ir}.ll"
                    temp_ir="${temp_ir}.ll"
                    local temp_bin=$(mktemp /tmp/hybrid_bin_fail.XXXXXX)
                    echo "$clean_ir" > "$temp_ir"

                    if grep -q "define i32 @main" "$temp_ir"; then
                        # Check if smart pointer runtime is needed
                        local needs_smart_ptr_runtime=$ARC_RUNTIME_REQUIRED
                        for opt in "${run_opts[@]}"; do
                            case "$opt" in
                                --arc-debug|--arc-leak-detect|--arc-verify-runtime|--arc-trace-retains)
                                    needs_smart_ptr_runtime=1
                                    ;;
                            esac
                        done
                        if grep -q "__hybrid_shared_control\|__hybrid_smart_" "$temp_ir"; then
                            needs_smart_ptr_runtime=1
                        elif grep -q "hybrid_release\|hybrid_retain\|hybrid_autorelease\|hybrid_alloc_array\|hybrid_alloc_object" "$temp_ir"; then
                            needs_smart_ptr_runtime=1
                        fi

                        local clang_success=0
                        if [ $needs_smart_ptr_runtime -eq 1 ]; then
                            # Smart pointer tests use C++ runtime - don't include stub runtime library
                            if clang++ "$temp_ir" src/runtime_support.cpp src/runtime/arc.cpp src/runtime/weak_table.cpp src/memory/ref_count.cpp -Isrc -Iruntime/include -std=c++17 -o "$temp_bin" 2>/dev/null; then
                                clang_success=1
                            fi
                        else
                            if clang++ "$temp_ir" "$RUNTIME_LIB" -o "$temp_bin" &> /dev/null; then
                                clang_success=1
                            fi
                        fi

                        if [ $clang_success -eq 1 ]; then
                            set +e
                            runtime_output=$("$temp_bin" 2>&1)
                            runtime_exit_code=$?
                            set -e

                            if [ $runtime_exit_code -ne 0 ]; then
                                has_errors=1
                            fi
                        else
                            has_errors=1
                        fi
                    fi

                    rm -f "$temp_ir" "$temp_bin"
                fi
            fi
        fi
    fi

    if [ ${#expected_runtime[@]} -gt 0 ]; then
        if [ -z "$runtime_output" ]; then
            has_errors=1
            output="${output}"$'\n'"[runtime-expectation] missing runtime output"
        else
            for expected in "${expected_runtime[@]}"; do
                if ! grep -F -q "$expected" <<< "$runtime_output"; then
                    has_errors=1
                    output="${output}"$'\n'"[runtime-expectation] missing: ${expected}"
                fi
            done
        fi
    fi

    # Determine if test passed or failed
    local test_passed=0

    # Special case: tests that should fail (have "fail" in name)
    if [[ "$test_name" == *"fail"* ]] || [[ "$test_name" == *"error"* ]]; then
        if [ $has_errors -eq 1 ] || [ $exit_code -ne 0 ]; then
            test_passed=1
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            test_passed=0
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    else
        # Normal tests should not have errors (including runtime errors)
        if [ $has_errors -eq 0 ] && [ $exit_code -eq 0 ]; then
            test_passed=1
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            test_passed=0
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    fi
    
    local should_show_output=0
    if [ $FAILURES_ONLY -eq 0 ] || [ $test_passed -eq 0 ]; then
        should_show_output=1
    fi

    if [ $VERBOSE_MODE -eq 1 ] && [ $should_show_output -eq 1 ]; then
        ensure_progress_newline
        echo -e "${BLUE}=== Test: $test_name ===${NC}"
        echo "File: $test_file"
        echo "Content:"
        awk '{printf(" %4d %s\n", NR, $0)}' "$test_file"
        echo
        echo "Output:"
        printf "%s\n" "$output"
        echo
        if [[ "$test_name" == *"fail"* ]] || [[ "$test_name" == *"error"* ]]; then
            if [ $test_passed -eq 1 ]; then
                echo -e "${GREEN}✓ PASSED: $test_name (correctly failed as expected)${NC}"
            else
                echo -e "${RED}✗ FAILED: $test_name (should have failed but didn't)${NC}"
            fi
        else
            if [ $test_passed -eq 1 ]; then
                echo -e "${GREEN}✓ PASSED: $test_name${NC}"
            else
                echo -e "${RED}✗ FAILED: $test_name${NC}"
                if [ $exit_code -ne 0 ]; then
                    echo -e "${RED}  Compilation exit code: $exit_code${NC}"
                fi
                if [ $runtime_exit_code -ne 0 ]; then
                    echo -e "${RED}  Runtime exit code: $runtime_exit_code (possible assert failure or abort)${NC}"
                fi
                if [ $has_errors -eq 1 ]; then
                    echo -e "${RED}  Errors found in output:${NC}"
                    echo "$output" | grep -E "(Error|Failed to generate|Unknown function|Unknown variable|Binary operator|Expected.*after)" | head -3
                fi
            fi
        fi
        if [ -n "$runtime_output" ]; then
            echo
            echo "Runtime output:"
            printf "%s\n" "$runtime_output"
        fi
        echo
        echo "========================================"
        echo
    elif [ $should_show_output -eq 1 ]; then
        ensure_progress_newline
        echo -e "${YELLOW}Running test: $test_name${NC}"
        echo "----------------------------------------"
        
        if [[ "$test_name" == *"fail"* ]] || [[ "$test_name" == *"error"* ]]; then
            if [ $test_passed -eq 1 ]; then
                echo -e "${GREEN}✓ PASSED: $test_name (correctly failed as expected)${NC}"
            else
                echo -e "${RED}✗ FAILED: $test_name (should have failed but didn't)${NC}"
            fi
        else
            if [ $test_passed -eq 1 ]; then
                echo -e "${GREEN}✓ PASSED: $test_name${NC}"
            else
                echo -e "${RED}✗ FAILED: $test_name${NC}"
                if [ $exit_code -ne 0 ]; then
                    echo -e "${RED}  Compilation exit code: $exit_code${NC}"
                fi
                if [ $runtime_exit_code -ne 0 ]; then
                    echo -e "${RED}  Runtime exit code: $runtime_exit_code (possible assert failure or abort)${NC}"
                fi
                if [ $has_errors -eq 1 ]; then
                    echo -e "${RED}  Errors found in output:${NC}"
                    echo "$output" | grep -E "(Error|Failed to generate|Unknown function|Unknown variable|Binary operator|Expected.*after)" | head -3
                fi
            fi
        fi
        echo
    fi
    
    mark_test_completed

    local test_wall_end=$(date +%s)
    local test_elapsed=$((test_wall_end - test_wall_start))
    if [ $counts_for_timing -eq 1 ]; then
        TIMED_DURATION=$((TIMED_DURATION + test_elapsed))
        TIMED_TESTS=$((TIMED_TESTS + 1))
    fi
}

run_multi_unit_tests() {
    local base_dir="test/multi_unit"
    if [ ! -d "$base_dir" ]; then
        return
    fi

    ensure_progress_newline
    echo
    echo -e "${BLUE}Multi-unit Compilation Tests${NC}"
    echo "-------------------------------"

    for dir in "$base_dir"/*/; do
        [ -d "$dir" ] || continue

        local test_name=$(basename "$dir")
        if [ -n "$MULTI_UNIT_FILTER" ] && [ "$test_name" != "$MULTI_UNIT_FILTER" ]; then
            continue
        fi
        local suite_start=$(date +%s)
        local expect="pass"
        if [ -f "${dir}/EXPECT_FAIL" ] || [[ "$test_name" == *_fail ]]; then
            expect="fail"
        fi
        local counts_for_timing=0
        if [ "$expect" = "pass" ]; then
            counts_for_timing=1
        fi

        local cmd_files=()
        while IFS= read -r -d '' file; do
            cmd_files+=("$file")
        done < <(find "$dir" -maxdepth 1 -name "*.hy" -type f -print0 | sort -z)

        if [ ${#cmd_files[@]} -eq 0 ]; then
            echo -e "${YELLOW}Skipping $test_name (no .hy files)${NC}"
            continue
        fi

        local temp_output
        temp_output=$(mktemp /tmp/hybrid_multi.XXXXXX.out)
        local output_args=("-o" "$temp_output")

        set +e
        local output
        output=$("$HYBRID_EXEC" "${EXTRA_COMPILER_ARGS[@]}" "${cmd_files[@]}" "${output_args[@]}" 2>&1)
        local status=$?
        set -e

        local runtime_output=""
        local runtime_status=0
        if [ "$expect" = "pass" ] && [ $status -eq 0 ]; then
            if [ -x "$temp_output" ]; then
                set +e
                runtime_output=$("$temp_output" 2>&1)
                runtime_status=$?
                set -e
            else
                runtime_status=1
                runtime_output="(binary '$temp_output' is not executable)"
            fi
        fi

        TOTAL_TESTS=$((TOTAL_TESTS + 1))

        local passed=0
        if [ "$expect" = "pass" ]; then
            if [ $status -eq 0 ] && [ $runtime_status -eq 0 ]; then
                passed=1
            fi
        else
            if [ $status -ne 0 ]; then
                passed=1
            fi
        fi

        if [ $passed -eq 1 ]; then
            PASSED_TESTS=$((PASSED_TESTS + 1))
            if [ $FAILURES_ONLY -eq 0 ]; then
                echo -e "${GREEN}✓ PASSED: $test_name${NC}"
            fi
        else
            FAILED_TESTS=$((FAILED_TESTS + 1))
            ensure_progress_newline
            echo -e "${RED}✗ FAILED: $test_name${NC}"
            if [ "$expect" = "pass" ]; then
                if [ $status -ne 0 ]; then
                    echo -e "${RED}  Expected success but compilation failed with status $status${NC}"
                elif [ $runtime_status -ne 0 ]; then
                    echo -e "${RED}  Runtime exited with status $runtime_status${NC}"
                fi
            else
                echo -e "${RED}  Expected failure but command succeeded${NC}"
            fi
            echo "--- Compiler Output ---"
            echo "$output"
            if [ "$expect" = "pass" ] && [ -n "$runtime_output" ]; then
                echo "--- Runtime Output ---"
                echo "$runtime_output"
            fi
            echo "--------------"
        fi

        rm -f "$temp_output"

        if [ $FAILURES_ONLY -eq 1 ]; then
            PROGRESS_COUNT=$((PROGRESS_COUNT + 1))
            update_progress_display
        fi

        local suite_end=$(date +%s)
        local suite_elapsed=$((suite_end - suite_start))
        if [ $counts_for_timing -eq 1 ]; then
            TIMED_DURATION=$((TIMED_DURATION + suite_elapsed))
            TIMED_TESTS=$((TIMED_TESTS + 1))
        fi
    done
}

# Parse command line arguments
# (Variables already initialized at the top of the script)

# Parse flags and patterns
while [[ $# -gt 0 ]]; do
    case $1 in
        -v|--verbose)
            VERBOSE_MODE=1
            VERBOSE_FROM_ARGS=1
            shift
            ;;
        -f|--full)
            FAILURES_ONLY=0
            shift
            ;;
        --failures-only)
            FAILURES_ONLY=1
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS] [TEST_PATTERN]"
            echo
            echo "Options:"
            echo "  -v, --verbose       Show detailed output for each test"
            echo "  -f, --full          Show full output for each test"
            echo "      --failures-only Show only failing tests (default)"
            echo "  -h, --help          Show this help message"
            echo
            echo "Test Pattern:"
            echo "  Can be a category name, file path, or pattern to match"
            echo
            echo "Examples:"
            echo "  $0                  # Run all tests (compact mode)"
            echo "  $0 -v               # Run all tests with verbose output"
            echo "  $0 -f               # Run all tests with full output"
            echo "  $0 -f -v            # Full output with verbose details"
            echo "  $0 --failures-only  # Explicitly run in compact mode (default)"
            echo "  $0 structs          # Run all tests in structs category"
            echo "  $0 operators        # Run all tests in operators category"
            echo "  $0 test_bool        # Run tests matching 'test_bool'"
            echo "  $0 -v test_bool     # Run tests matching 'test_bool' with verbose output"
            echo "  $0 test/types/test_bool.hy  # Run specific test file"
            echo
            echo "Available categories:"
            for dir in test/*/; do
                if [ -d "$dir" ]; then
                    echo "  - $(basename "$dir")"
                fi
            done
            exit 0
            ;;
        *)
        TEST_PATTERN="$1"
        shift
        ;;
    esac
done

if [[ "$TEST_PATTERN" == multi_unit/* ]]; then
    MULTI_UNIT_FILTER="${TEST_PATTERN#multi_unit/}"
    TEST_PATTERN="multi_unit"
fi

if [ $FORCE_VERBOSE_FROM_ENV -eq 1 ] && [ $VERBOSE_FROM_ARGS -eq 0 ]; then
    VERBOSE_MODE=1
fi

# Filter tests if pattern provided
if [ -n "$TEST_PATTERN" ]; then
    RUN_MULTI_UNIT_TESTS=0
    # Check if it's a category name
    if [ -d "test/$TEST_PATTERN" ]; then
        if [ "$TEST_PATTERN" = "multi_unit" ]; then
            TEST_FILES=""
            if [ -n "$MULTI_UNIT_FILTER" ]; then
                echo "Running multi-unit manifest tests (filtered to '$MULTI_UNIT_FILTER')"
            else
                echo "Running multi-unit manifest tests"
            fi
            RUN_MULTI_UNIT_TESTS=1
        else
            # Run all tests in that category
            TEST_FILES=$(find "test/$TEST_PATTERN" -name "*.hy" -type f | sort)
            echo "Running all tests in category: $TEST_PATTERN"
        fi
    elif [ -f "$TEST_PATTERN" ]; then
        # Specific file with path
        TEST_FILES="$TEST_PATTERN"
        echo "Running specific test file: $TEST_PATTERN"
    elif [ -f "test/$TEST_PATTERN" ]; then
        # Specific file in test root
        TEST_FILES="test/$TEST_PATTERN"
        echo "Running specific test file: test/$TEST_PATTERN"
    else
        # Pattern matching across all subdirectories
        TEST_FILES=$(find test/ -path 'test/multi_unit' -prune -o -name "*$TEST_PATTERN*.hy" -type f -print | sort)
        if [ -z "$TEST_FILES" ]; then
            echo -e "${RED}No test files found matching pattern: $TEST_PATTERN${NC}"
            exit 1
        fi
        echo "Running tests matching pattern: $TEST_PATTERN"
    fi
    
    if [ $VERBOSE_MODE -eq 1 ]; then
        echo "(verbose mode enabled)"
    fi
    echo
fi

MULTI_UNIT_TEST_COUNT=$(recount_multi_unit "$MULTI_UNIT_FILTER")

SINGLE_TEST_COUNT=0
if [ -n "$TEST_FILES" ]; then
    SINGLE_TEST_COUNT=$(printf "%s\n" "$TEST_FILES" | sed '/^$/d' | wc -l | tr -d '[:space:]')
fi
EFFECTIVE_MULTI_UNIT_TEST_COUNT=$MULTI_UNIT_TEST_COUNT
if [ $RUN_MULTI_UNIT_TESTS -eq 0 ]; then
    EFFECTIVE_MULTI_UNIT_TEST_COUNT=0
fi
TOTAL_DISCOVERED_TESTS=$((SINGLE_TEST_COUNT + EFFECTIVE_MULTI_UNIT_TEST_COUNT))
echo "Found $TOTAL_DISCOVERED_TESTS total tests to run"
echo

PROGRESS_TOTAL=$TOTAL_DISCOVERED_TESTS

# Run tests
if [ $VERBOSE_MODE -eq 1 ]; then
    echo "Running tests in verbose mode..."
    echo
fi

for test_file in $TEST_FILES; do
    run_test "$test_file"
done

if [ $RUN_MULTI_UNIT_TESTS -eq 1 ]; then
    run_multi_unit_tests
fi

ensure_progress_newline

# Print summary
echo "==============================="
echo -e "${BLUE}Test Summary${NC}"
echo "Total tests:  $TOTAL_TESTS"
echo -e "Passed:       ${GREEN}$PASSED_TESTS${NC}"
echo -e "Failed:       ${RED}$FAILED_TESTS${NC}"

# Add note about failures-only mode
if [ $FAILURES_ONLY -eq 1 ] && [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
fi

if [ $TIMED_TESTS -gt 0 ]; then
    AVG_TIME=$(awk "BEGIN { printf \"%.2f\", $TIMED_DURATION / $TIMED_TESTS }")
else
    AVG_TIME="n/a"
fi

echo
echo "Timing (runtime tests only):"
if [ $TIMED_TESTS -gt 0 ]; then
    printf "  Counted tests: %d\n" "$TIMED_TESTS"
    printf "  Total elapsed: %ds\n" "$TIMED_DURATION"
    printf "  Avg per test: %ss\n" "$AVG_TIME"
else
    echo "  No runtime tests were timed."
fi

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed!${NC}"
    echo
    echo "To debug failing tests, run:"
    echo "  $0 -v [test_pattern]"
    exit 1
fi
