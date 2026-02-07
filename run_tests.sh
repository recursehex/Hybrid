#!/bin/bash

# Hybrid Compiler Test Runner
# Automatically discovers and runs all test files in the test/ directory

set -e  # Exit on any error

TIMED_DURATION=0.0
TIMED_TESTS=0

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

now_time() {
    if command -v python3 >/dev/null 2>&1; then
        python3 - <<'PY'
import time, sys
sys.stdout.write(f"{time.time():.6f}")
PY
    else
        date +%s
    fi
}

sum_durations() {
    awk "BEGIN { printf \"%.6f\", $1 + $2 }"
}

diff_durations() {
    awk "BEGIN { printf \"%.6f\", $2 - $1 }"
}

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
JOBS=1
TEST_PATTERN=""
RUN_MULTI_UNIT_TESTS=1
MULTI_UNIT_FILTER=""
EXTRA_COMPILER_ARGS=()
FORCE_VERBOSE_FROM_ENV=0
VERBOSE_FROM_ARGS=0
ARC_RUNTIME_REQUIRED=0
ARC_ENABLED_OVERRIDE=""
SANITIZER_MODE=""
SANITIZER_FLAGS=()
HYBRID_EXEC_FROM_ENV="${HYBRID_EXEC_OVERRIDE:-${HYBRID_EXEC:-}}"
VALGRIND_MODE=0
VALGRIND_ARGS=(
    "--leak-check=full"
    "--show-leak-kinds=all"
    "--errors-for-leak-kinds=none"
    "--error-exitcode=101"
)

if [[ -n "${HYBRID_TEST_SANITIZER+x}" ]]; then
    value=$(printf "%s" "$HYBRID_TEST_SANITIZER" | tr -d '[:space:]')
    lower=$(printf "%s" "$value" | tr '[:upper:]' '[:lower:]')
    case "$lower" in
        ""|address|asan)
            SANITIZER_MODE="address"
            ;;
        0|false|off|none)
            SANITIZER_MODE=""
            ;;
        *)
            echo -e "${RED}Error: unsupported HYBRID_TEST_SANITIZER value '$HYBRID_TEST_SANITIZER' (use 'address' or 'off')${NC}"
            exit 1
            ;;
    esac
fi

if [ "$SANITIZER_MODE" = "address" ]; then
    SANITIZER_FLAGS=("-fsanitize=address" "-fno-omit-frame-pointer")
fi

if [[ -n "${HYBRID_TEST_VALGRIND+x}" ]]; then
    value=$(printf "%s" "$HYBRID_TEST_VALGRIND" | tr -d '[:space:]')
    lower=$(printf "%s" "$value" | tr '[:upper:]' '[:lower:]')
    case "$lower" in
        ""|1|true|on|yes)
            VALGRIND_MODE=1
            ;;
        0|false|off|none|no)
            VALGRIND_MODE=0
            ;;
        *)
            echo -e "${RED}Error: unsupported HYBRID_TEST_VALGRIND value '$HYBRID_TEST_VALGRIND' (use on/off)${NC}"
            exit 1
            ;;
    esac
fi

echo -e "${BLUE}Hybrid Compiler Test Suite${NC}"
echo "==============================="

# Check for hybrid executable - try multiple locations
HYBRID_EXEC=""
HYBRID_EXEC_OVERRIDE="$HYBRID_EXEC_FROM_ENV"
if [ -n "$HYBRID_EXEC_OVERRIDE" ]; then
    if [ ! -x "$HYBRID_EXEC_OVERRIDE" ]; then
        echo -e "${RED}Error: HYBRID_EXEC override is not executable: $HYBRID_EXEC_OVERRIDE${NC}"
        exit 1
    fi
    HYBRID_EXEC="$HYBRID_EXEC_OVERRIDE"
elif [ -f "./build/hybrid" ]; then
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
HYBRID_BIN_DIR=$(dirname "$HYBRID_EXEC")
PREBUILT_RUNTIME_STUB_LIB="${HYBRID_BIN_DIR}/libhybrid_test_stub.a"
PREBUILT_ARC_RUNTIME_LIB="${HYBRID_BIN_DIR}/libhybrid_runtime_test.a"

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
RUNTIME_LIB_IS_TEMP=0
if [ -f "$PREBUILT_RUNTIME_STUB_LIB" ]; then
    RUNTIME_LIB="$PREBUILT_RUNTIME_STUB_LIB"
elif command -v clang &> /dev/null; then
    RUNTIME_LIB=$(mktemp /tmp/hybrid_runtime.XXXXXX)
    mv "$RUNTIME_LIB" "${RUNTIME_LIB}.o"
    RUNTIME_LIB="${RUNTIME_LIB}.o"
    if ! clang "${SANITIZER_FLAGS[@]}" -c runtime/test_runtime_stub.c -o "$RUNTIME_LIB" 2>/dev/null; then
        RUNTIME_LIB=""
    else
        RUNTIME_LIB_IS_TEMP=1
    fi
fi

ARC_RUNTIME_DIR=""
ARC_RUNTIME_OBJS=()
ARC_RUNTIME_PREPARED=0

ensure_arc_runtime_objects() {
    if [ $ARC_RUNTIME_PREPARED -eq 1 ]; then
        return
    fi
    ARC_RUNTIME_PREPARED=1

    if ! command -v clang++ &> /dev/null; then
        return
    fi
    if [ -f "$PREBUILT_ARC_RUNTIME_LIB" ]; then
        ARC_RUNTIME_OBJS=("$PREBUILT_ARC_RUNTIME_LIB")
        return
    fi

    ARC_RUNTIME_DIR=$(mktemp -d /tmp/hybrid_arc_runtime.XXXXXX)
    runtime_sources=(
        "src/runtime_support.cpp"
        "src/runtime/arc.cpp"
        "src/runtime/weak_table.cpp"
        "src/memory/ref_count.cpp"
    )
    runtime_objects=(
        "${ARC_RUNTIME_DIR}/runtime_support.o"
        "${ARC_RUNTIME_DIR}/arc.o"
        "${ARC_RUNTIME_DIR}/weak_table.o"
        "${ARC_RUNTIME_DIR}/ref_count.o"
    )

    for i in "${!runtime_sources[@]}"; do
        if ! clang++ "${SANITIZER_FLAGS[@]}" -c "${runtime_sources[$i]}" -Isrc -Iruntime/include -std=c++17 -o "${runtime_objects[$i]}" 2>/dev/null; then
            rm -rf "$ARC_RUNTIME_DIR"
            ARC_RUNTIME_DIR=""
            return
        fi
    done

    ARC_RUNTIME_OBJS=("${runtime_objects[@]}")
}

# Cleanup runtime library on exit
cleanup_runtime() {
    if [ $RUNTIME_LIB_IS_TEMP -eq 1 ] && [ -n "$RUNTIME_LIB" ]; then
        rm -f "$RUNTIME_LIB"
    fi
    if [ -n "$ARC_RUNTIME_DIR" ]; then
        rm -rf "$ARC_RUNTIME_DIR"
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
    local machine_mode=0
    if [ -n "${RESULT_FILE:-}" ]; then
        machine_mode=1
    fi
    local test_wall_start=""
    local counts_for_timing=1
    local expected_mode="pass"
    local expected_fail_kind=""
    local expected_exit=""
    local expect_pass=0

    if grep -qE "^// EXPECT_PASS" "$test_file"; then
        expect_pass=1
    fi

    while IFS= read -r line; do
        line=${line#"// EXPECT_FAIL:"}
        line=$(echo "$line" | sed -e 's/^[[:space:]]*//')
        if [ -z "$line" ]; then
            expected_fail_kind="compile"
        else
            local lower
            lower=$(printf "%s" "$line" | tr '[:upper:]' '[:lower:]')
            case "$lower" in
                compile|compiler)
                    expected_fail_kind="compile"
                    ;;
                runtime|run)
                    expected_fail_kind="runtime"
                    ;;
                any|either)
                    expected_fail_kind="any"
                    ;;
                *)
                    expected_fail_kind="compile"
                    ;;
            esac
        fi
    done < <(grep -E "^// EXPECT_FAIL:" "$test_file" || true)

    if [ $expect_pass -eq 1 ]; then
        expected_mode="pass"
    elif [ -n "$expected_fail_kind" ]; then
        expected_mode="fail"
    elif [[ "$test_name" == *"fail"* ]] || [[ "$test_name" == *"error"* ]]; then
        expected_mode="fail"
        expected_fail_kind="compile"
    fi

    if [ "$expected_mode" = "fail" ] && [ -z "$expected_fail_kind" ]; then
        expected_fail_kind="compile"
    fi

    if [ "$expected_mode" = "fail" ]; then
        counts_for_timing=0
    fi
    if [ $counts_for_timing -eq 1 ]; then
        test_wall_start=$(now_time)
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

    local expected_output=()
    while IFS= read -r line; do
        line=${line#"// EXPECT_OUTPUT:"}
        line=$(echo "$line" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
        if [ -n "$line" ]; then
            expected_output+=("$line")
        fi
    done < <(grep -E "^// EXPECT_OUTPUT:" "$test_file" || true)

    local expected_compile_diagnostics=()
    while IFS= read -r line; do
        line=${line#"// EXPECT_DIAGNOSTIC:"}
        line=$(echo "$line" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
        if [ -n "$line" ]; then
            expected_compile_diagnostics+=("$line")
        fi
    done < <(grep -E "^// EXPECT_DIAGNOSTIC:" "$test_file" || true)

    while IFS= read -r line; do
        line=${line#"// EXPECT_ERROR:"}
        line=$(echo "$line" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
        if [ -n "$line" ]; then
            expected_compile_diagnostics+=("$line")
        fi
    done < <(grep -E "^// EXPECT_ERROR:" "$test_file" || true)

    local expected_runtime=()
    while IFS= read -r line; do
        line=${line#"// EXPECT_RUNTIME:"}
        line=$(echo "$line" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
        if [ -n "$line" ]; then
            expected_runtime+=("$line")
        fi
    done < <(grep -E "^// EXPECT_RUNTIME:" "$test_file" || true)

    while IFS= read -r line; do
        line=${line#"// EXPECT_EXIT:"}
        line=$(echo "$line" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
        if [ -n "$line" ]; then
            expected_exit="$line"
        fi
    done < <(grep -E "^// EXPECT_EXIT:" "$test_file" || true)
    
    # Run the test and capture both output and exit code
    local output
    local exit_code
    local emitted_ir_file=""
    local used_direct_ir_file=0
    local may_need_runtime=0
    local run_opts_override_output=0
    if [ "$expected_mode" = "pass" ]; then
        may_need_runtime=1
    elif [ "$expected_fail_kind" != "compile" ]; then
        may_need_runtime=1
    elif [ ${#expected_runtime[@]} -gt 0 ] || [ -n "$expected_exit" ]; then
        may_need_runtime=1
    fi
    for opt in "${run_opts[@]}"; do
        if [ "$opt" = "-o" ] || [ "$opt" = "--emit-llvm" ] || [[ "$opt" == -o=* ]]; then
            run_opts_override_output=1
            break
        fi
    done

    if [ $may_need_runtime -eq 1 ] && [ $run_opts_override_output -eq 0 ]; then
        emitted_ir_file=$(mktemp /tmp/hybrid_test.XXXXXX)
        mv "$emitted_ir_file" "${emitted_ir_file}.ll"
        emitted_ir_file="${emitted_ir_file}.ll"
        output=$("$HYBRID_EXEC" "${EXTRA_COMPILER_ARGS[@]}" "${run_opts[@]}" "$test_file" -o "$emitted_ir_file" 2>&1)
        exit_code=$?
        used_direct_ir_file=1
    else
        output=$("$HYBRID_EXEC" "${EXTRA_COMPILER_ARGS[@]}" "${run_opts[@]}" < "$test_file" 2>&1)
        exit_code=$?
    fi
    
    # Check for error patterns in output
    local compile_failed=0
    if [ $exit_code -ne 0 ]; then
        compile_failed=1
    fi
    if grep -E -q "Error at line|Error:|Failed to generate|Unknown function|Unknown variable|invalid binary operator|Binary operator|Expected.*after" <<< "$output" 2>/dev/null; then
        compile_failed=1
    fi

    local compile_diagnostics=()
    while IFS= read -r line; do
        if [[ "$line" =~ ^Error\ at\ line ]] || [[ "$line" =~ ^Error: ]] || [[ "$line" =~ ^Warning\ at\ line ]]; then
            compile_diagnostics+=("$line")
        fi
    done < <(printf "%s\n" "$output" | sed -e 's/Error at line /\nError at line /g' -e 's/Warning at line /\nWarning at line /g' -e 's/Error: /\nError: /g')

    local compiler_expectation_failed=0
    for expected in "${expected_output[@]}"; do
        if ! grep -F -q "$expected" <<< "$output"; then
            compiler_expectation_failed=1
            output="${output}"$'\n'"[test-expectation] missing: ${expected}"
        fi
    done

    if [ "$expected_mode" = "fail" ] && [ "$expected_fail_kind" != "runtime" ]; then
        local strict_diagnostics=("${expected_compile_diagnostics[@]}")
        if [ ${#strict_diagnostics[@]} -eq 0 ]; then
            compiler_expectation_failed=1
            output="${output}"$'\n'"[test-expectation] missing compile diagnostic expectations for ${test_file}"
        else
            local strict_diag_mismatch=0
            local -a strict_diag_matched=()
            local -a unexpected_diagnostics=()
            local -a missing_diagnostics=()

            for _ in "${strict_diagnostics[@]}"; do
                strict_diag_matched+=(0)
            done

            for actual_diag in "${compile_diagnostics[@]}"; do
                local matched_current=0
                for i in "${!strict_diagnostics[@]}"; do
                    if [ "${strict_diag_matched[$i]}" -eq 1 ]; then
                        continue
                    fi
                    if [[ "$actual_diag" == *"${strict_diagnostics[$i]}"* ]]; then
                        strict_diag_matched[$i]=1
                        matched_current=1
                        break
                    fi
                done
                if [ $matched_current -eq 0 ]; then
                    strict_diag_mismatch=1
                    unexpected_diagnostics+=("$actual_diag")
                fi
            done

            for i in "${!strict_diagnostics[@]}"; do
                if [ "${strict_diag_matched[$i]}" -eq 0 ]; then
                    strict_diag_mismatch=1
                    missing_diagnostics+=("${strict_diagnostics[$i]}")
                fi
            done

            if [ $strict_diag_mismatch -eq 1 ]; then
                compiler_expectation_failed=1
                for missing_diag in "${missing_diagnostics[@]}"; do
                    output="${output}"$'\n'"[diagnostic-expectation] missing: ${missing_diag}"
                done
                for unexpected_diag in "${unexpected_diagnostics[@]}"; do
                    output="${output}"$'\n'"[diagnostic-expectation] unexpected: ${unexpected_diag}"
                done
            fi
        fi
    fi

    local runtime_exit_code=0
    local runtime_output=""
    local runtime_ran=0
    local runtime_failed=0
    local runtime_failure_kind=""
    local should_run_runtime=0
    local runtime_ir_file="$emitted_ir_file"

    if [ $compile_failed -eq 0 ] && [ $exit_code -eq 0 ]; then
        if [ "$expected_mode" = "pass" ]; then
            should_run_runtime=1
        else
            if [ "$expected_fail_kind" != "compile" ]; then
                should_run_runtime=1
            elif [ ${#expected_runtime[@]} -gt 0 ] || [ -n "$expected_exit" ]; then
                should_run_runtime=1
            fi
        fi
    fi

    if [ $should_run_runtime -eq 1 ]; then
        if command -v clang &> /dev/null && [ -n "$RUNTIME_LIB" ] && [ -f "$RUNTIME_LIB" ]; then
            if [ -z "$runtime_ir_file" ] || [ ! -f "$runtime_ir_file" ]; then
                # Fallback path: extract the final complete LLVM module from stdin mode output.
                local module_marker=$(echo "$output" | grep -n "^=== Final Generated" | tail -1 | cut -d: -f1)
                local module_start=""
                if [ -n "$module_marker" ]; then
                    module_start=$((module_marker + 1))
                fi

                if [ -n "$module_start" ]; then
                    local clean_ir=$(echo "$output" | tail -n +$module_start | grep -v "^ready>" | grep -v "^Parsed" | grep -v "^Generated" | grep -v "^\\[generics" | grep -v "^\\[arc-trace\\]" | grep -v "^\\[arc-escape\\]")

                    runtime_ir_file=$(mktemp /tmp/hybrid_test.XXXXXX)
                    mv "$runtime_ir_file" "${runtime_ir_file}.ll"
                    runtime_ir_file="${runtime_ir_file}.ll"
                    echo "$clean_ir" > "$runtime_ir_file"
                fi
            fi

            if [ -n "$runtime_ir_file" ] && [ -f "$runtime_ir_file" ]; then
                local temp_bin=$(mktemp /tmp/hybrid_bin.XXXXXX)

                if grep -q "define i32 @main" "$runtime_ir_file"; then
                    # Check if smart pointer runtime is needed
                    local needs_smart_ptr_runtime=$ARC_RUNTIME_REQUIRED
                    for opt in "${run_opts[@]}"; do
                        case "$opt" in
                            --arc-debug|--arc-leak-detect|--arc-verify-runtime|--arc-trace-retains)
                                needs_smart_ptr_runtime=1
                                ;;
                        esac
                    done
                    if grep -q "__hybrid_shared_control\|__hybrid_smart_" "$runtime_ir_file"; then
                        needs_smart_ptr_runtime=1
                    elif grep -q "hybrid_release\|hybrid_retain\|hybrid_autorelease\|hybrid_alloc_array\|hybrid_alloc_object" "$runtime_ir_file"; then
                        needs_smart_ptr_runtime=1
                    fi

                    local clang_success=0
                    if [ $needs_smart_ptr_runtime -eq 1 ]; then
                        ensure_arc_runtime_objects
                        if [ ${#ARC_RUNTIME_OBJS[@]} -gt 0 ]; then
                            if clang++ "$runtime_ir_file" "${ARC_RUNTIME_OBJS[@]}" "${SANITIZER_FLAGS[@]}" -o "$temp_bin" 2>/dev/null; then
                                clang_success=1
                            fi
                        else
                            # Fallback when precompiled runtime objects are unavailable.
                            if clang++ "${SANITIZER_FLAGS[@]}" "$runtime_ir_file" src/runtime_support.cpp src/runtime/arc.cpp src/runtime/weak_table.cpp src/memory/ref_count.cpp -Isrc -Iruntime/include -std=c++17 -o "$temp_bin" 2>/dev/null; then
                                clang_success=1
                            fi
                        fi
                    else
                        if clang++ "$runtime_ir_file" "$RUNTIME_LIB" "${SANITIZER_FLAGS[@]}" -o "$temp_bin" &> /dev/null; then
                            clang_success=1
                        fi
                    fi

                    if [ $clang_success -eq 1 ]; then
                        set +e
                        if [ $VALGRIND_MODE -eq 1 ]; then
                            runtime_output=$(valgrind "${VALGRIND_ARGS[@]}" "$temp_bin" 2>&1)
                            runtime_exit_code=$?
                        else
                            runtime_output=$("$temp_bin" 2>&1)
                            runtime_exit_code=$?
                        fi
                        set -e
                        runtime_ran=1

                        if [ $runtime_exit_code -ne 0 ]; then
                            runtime_failed=1
                            runtime_failure_kind="exit"
                        fi
                    else
                        runtime_failed=1
                        runtime_failure_kind="clang"
                    fi
                fi

                if [ $runtime_failed -ne 0 ] && [ -n "$temp_bin" ]; then
                    cp "$runtime_ir_file" /tmp/hybrid_fail.ll 2>/dev/null || true
                    cp "$temp_bin" /tmp/hybrid_fail.bin 2>/dev/null || true
                fi
                rm -f "$temp_bin"
            fi
        fi
    fi

    local runtime_expectation_failed=0
    if [ ${#expected_runtime[@]} -gt 0 ]; then
        if [ $runtime_ran -eq 0 ]; then
            runtime_expectation_failed=1
            output="${output}"$'\n'"[runtime-expectation] runtime did not run"
        elif [ -z "$runtime_output" ]; then
            runtime_expectation_failed=1
            output="${output}"$'\n'"[runtime-expectation] missing runtime output"
        else
            for expected in "${expected_runtime[@]}"; do
                if ! grep -F -q "$expected" <<< "$runtime_output"; then
                    runtime_expectation_failed=1
                    output="${output}"$'\n'"[runtime-expectation] missing: ${expected}"
                fi
            done
        fi
    fi

    if [ -n "$expected_exit" ]; then
        if [ $runtime_ran -eq 0 ]; then
            runtime_expectation_failed=1
            output="${output}"$'\n'"[runtime-expectation] missing runtime exit"
        else
            local exit_expectation_failed=0
            local lower_exit
            lower_exit=$(printf "%s" "$expected_exit" | tr '[:upper:]' '[:lower:]')
            case "$lower_exit" in
                nonzero)
                    if [ $runtime_exit_code -eq 0 ]; then
                        exit_expectation_failed=1
                    fi
                    ;;
                zero)
                    if [ $runtime_exit_code -ne 0 ]; then
                        exit_expectation_failed=1
                    fi
                    ;;
                abort|sigabrt)
                    if [ $runtime_exit_code -ne 134 ]; then
                        exit_expectation_failed=1
                    fi
                    ;;
                *)
                    if [ "$runtime_exit_code" -ne "$expected_exit" ]; then
                        exit_expectation_failed=1
                    fi
                    ;;
            esac
            if [ $exit_expectation_failed -eq 1 ]; then
                runtime_expectation_failed=1
                output="${output}"$'\n'"[runtime-expectation] expected exit ${expected_exit}, got ${runtime_exit_code}"
            fi
        fi
    fi

    # Determine if test passed or failed
    local test_passed=0
    if [ "$expected_mode" = "fail" ]; then
        case "$expected_fail_kind" in
            compile)
                if [ $compile_failed -eq 1 ] && [ $compiler_expectation_failed -eq 0 ] && [ $runtime_expectation_failed -eq 0 ]; then
                    test_passed=1
                fi
                ;;
            runtime)
                if [ $compile_failed -eq 0 ] && [ $compiler_expectation_failed -eq 0 ] && [ $runtime_failed -eq 1 ] && [ $runtime_expectation_failed -eq 0 ]; then
                    test_passed=1
                fi
                ;;
            any)
                if { [ $compile_failed -eq 1 ] || [ $runtime_failed -eq 1 ]; } && [ $compiler_expectation_failed -eq 0 ] && [ $runtime_expectation_failed -eq 0 ]; then
                    test_passed=1
                fi
                ;;
            *)
                if [ $compile_failed -eq 1 ] && [ $compiler_expectation_failed -eq 0 ] && [ $runtime_expectation_failed -eq 0 ]; then
                    test_passed=1
                fi
                ;;
        esac
    else
        if [ $compile_failed -eq 0 ] && [ $compiler_expectation_failed -eq 0 ] && [ $runtime_failed -eq 0 ] && [ $runtime_expectation_failed -eq 0 ]; then
            test_passed=1
        fi
    fi

    if [ $machine_mode -eq 0 ]; then
        if [ $test_passed -eq 1 ]; then
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    fi
    
    local expected_label="expected to pass"
    if [ "$expected_mode" = "fail" ]; then
        if [ "$expected_fail_kind" = "any" ]; then
            expected_label="expected failure"
        else
            expected_label="expected ${expected_fail_kind} failure"
        fi
    fi

    local should_show_output=0
    if [ $FAILURES_ONLY -eq 0 ] || [ $test_passed -eq 0 ]; then
        should_show_output=1
    fi
    if [ $machine_mode -eq 1 ] && [ $test_passed -eq 1 ]; then
        should_show_output=0
    fi

    if [ $VERBOSE_MODE -eq 1 ] && [ $should_show_output -eq 1 ]; then
        if [ $machine_mode -eq 0 ]; then
            ensure_progress_newline
        fi
        echo -e "${BLUE}=== Test: $test_name ===${NC}"
        echo "File: $test_file"
        echo "Content:"
        awk '{printf(" %4d %s\n", NR, $0)}' "$test_file"
        echo
        echo "Output:"
        printf "%s\n" "$output"
        echo
        if [ "$expected_mode" = "fail" ]; then
            if [ $test_passed -eq 1 ]; then
                echo -e "${GREEN}✓ PASSED: $test_name (${expected_label})${NC}"
            else
                echo -e "${RED}✗ FAILED: $test_name (${expected_label})${NC}"
            fi
        else
            if [ $test_passed -eq 1 ]; then
                echo -e "${GREEN}✓ PASSED: $test_name${NC}"
            else
                echo -e "${RED}✗ FAILED: $test_name${NC}"
            fi
        fi
        if [ $test_passed -eq 0 ]; then
            if [ $exit_code -ne 0 ]; then
                echo -e "${RED}  Compilation exit code: $exit_code${NC}"
            elif [ $compile_failed -ne 0 ]; then
                echo -e "${RED}  Compiler diagnostics detected${NC}"
            fi
            if [ $runtime_ran -eq 1 ]; then
                if [ $runtime_exit_code -ne 0 ]; then
                    echo -e "${RED}  Runtime exit code: $runtime_exit_code${NC}"
                fi
            elif [ "$expected_mode" = "fail" ] && [ "$expected_fail_kind" = "runtime" ]; then
                echo -e "${RED}  Runtime did not run${NC}"
            fi
            if [ $compiler_expectation_failed -ne 0 ] || [ $runtime_expectation_failed -ne 0 ]; then
                echo -e "${RED}  Expectation mismatch detected${NC}"
            fi
            if [ $compile_failed -ne 0 ] || [ $compiler_expectation_failed -ne 0 ]; then
                local error_lines
                error_lines=$(echo "$output" | grep -E "Error at line|Error:|Warning at line" || true)
                echo -e "${RED}  Compiler output:${NC}"
                if [ -n "$error_lines" ]; then
                    echo "$error_lines" | head -5
                else
                    echo "$output" | head -5
                fi
            fi
            if [ $runtime_expectation_failed -ne 0 ] || [ $runtime_failed -ne 0 ]; then
                if [ -n "$runtime_output" ]; then
                    echo -e "${RED}  Runtime output:${NC}"
                    echo "$runtime_output" | head -5
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
        if [ $machine_mode -eq 0 ]; then
            ensure_progress_newline
        fi
        echo -e "${YELLOW}Running test: $test_name${NC}"
        echo "----------------------------------------"
        
        if [ "$expected_mode" = "fail" ]; then
            if [ $test_passed -eq 1 ]; then
                echo -e "${GREEN}✓ PASSED: $test_name (${expected_label})${NC}"
            else
                echo -e "${RED}✗ FAILED: $test_name (${expected_label})${NC}"
            fi
        else
            if [ $test_passed -eq 1 ]; then
                echo -e "${GREEN}✓ PASSED: $test_name${NC}"
            else
                echo -e "${RED}✗ FAILED: $test_name${NC}"
            fi
        fi
        if [ $test_passed -eq 0 ]; then
            if [ $exit_code -ne 0 ]; then
                echo -e "${RED}  Compilation exit code: $exit_code${NC}"
            elif [ $compile_failed -ne 0 ]; then
                echo -e "${RED}  Compiler diagnostics detected${NC}"
            fi
            if [ $runtime_ran -eq 1 ]; then
                if [ $runtime_exit_code -ne 0 ]; then
                    echo -e "${RED}  Runtime exit code: $runtime_exit_code${NC}"
                fi
            elif [ "$expected_mode" = "fail" ] && [ "$expected_fail_kind" = "runtime" ]; then
                echo -e "${RED}  Runtime did not run${NC}"
            fi
            if [ $compiler_expectation_failed -ne 0 ] || [ $runtime_expectation_failed -ne 0 ]; then
                echo -e "${RED}  Expectation mismatch detected${NC}"
            fi
            if [ $compile_failed -ne 0 ] || [ $compiler_expectation_failed -ne 0 ]; then
                local error_lines
                error_lines=$(echo "$output" | grep -E "Error at line|Error:|Warning at line" || true)
                echo -e "${RED}  Compiler output:${NC}"
                if [ -n "$error_lines" ]; then
                    echo "$error_lines" | head -5
                else
                    echo "$output" | head -5
                fi
            fi
            if [ $runtime_expectation_failed -ne 0 ] || [ $runtime_failed -ne 0 ]; then
                if [ -n "$runtime_output" ]; then
                    echo -e "${RED}  Runtime output:${NC}"
                    echo "$runtime_output" | head -5
                fi
            fi
        fi
        echo
    fi
    
    if [ $machine_mode -eq 0 ]; then
        mark_test_completed
    fi

    local test_elapsed=""
    if [ $counts_for_timing -eq 1 ]; then
        local test_wall_end=$(now_time)
        test_elapsed=$(diff_durations "$test_wall_start" "$test_wall_end")
        if [ $machine_mode -eq 0 ]; then
            TIMED_DURATION=$(sum_durations "$TIMED_DURATION" "$test_elapsed")
            TIMED_TESTS=$((TIMED_TESTS + 1))
        fi
    fi

    if [ $machine_mode -eq 1 ] && [ -n "${RESULT_FILE:-}" ]; then
        {
            printf "test_file=%s\n" "$test_file"
            printf "test_name=%s\n" "$test_name"
            printf "test_passed=%d\n" "$test_passed"
            printf "counts_for_timing=%d\n" "$counts_for_timing"
            printf "test_elapsed=%s\n" "$test_elapsed"
        } > "$RESULT_FILE"
    fi

    if [ -n "$runtime_ir_file" ] && [ -f "$runtime_ir_file" ] && [ $used_direct_ir_file -eq 0 ]; then
        rm -f "$runtime_ir_file"
    fi
    if [ -n "$emitted_ir_file" ] && [ -f "$emitted_ir_file" ]; then
        rm -f "$emitted_ir_file"
    fi
}

process_parallel_result() {
    local result_file="$1"
    local output_file="$2"

    local result_test_name=""
    local result_passed="0"
    local result_counts_for_timing="0"
    local result_elapsed=""

    if [ -f "$result_file" ]; then
        while IFS='=' read -r key value; do
            case "$key" in
                test_name) result_test_name="$value" ;;
                test_passed) result_passed="$value" ;;
                counts_for_timing) result_counts_for_timing="$value" ;;
                test_elapsed) result_elapsed="$value" ;;
            esac
        done < "$result_file"
    fi

    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    if [ "$result_passed" = "1" ]; then
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        FAILED_TESTS=$((FAILED_TESTS + 1))
        ensure_progress_newline
        if [ -s "$output_file" ]; then
            cat "$output_file"
        else
            if [ -n "$result_test_name" ]; then
                echo -e "${RED}✗ FAILED: $result_test_name${NC}"
            else
                echo -e "${RED}✗ FAILED: test worker did not return results${NC}"
            fi
        fi
    fi

    if [ "$result_counts_for_timing" = "1" ] && [ -n "$result_elapsed" ]; then
        TIMED_DURATION=$(sum_durations "$TIMED_DURATION" "$result_elapsed")
        TIMED_TESTS=$((TIMED_TESTS + 1))
    fi

    if [ $FAILURES_ONLY -eq 1 ]; then
        PROGRESS_COUNT=$((PROGRESS_COUNT + 1))
        update_progress_display
    fi
}

run_single_tests_parallel() {
    local temp_dir
    temp_dir=$(mktemp -d /tmp/hybrid_parallel.XXXXXX)
    local batch_size=0
    local batch_test_files=()
    local batch_result_files=()
    local batch_output_files=()
    local batch_pids=()
    local sequence=0

    while IFS= read -r test_file; do
        if [ -z "$test_file" ]; then
            continue
        fi

        sequence=$((sequence + 1))
        local result_file="${temp_dir}/result.${sequence}"
        local output_file="${temp_dir}/output.${sequence}"

        (
            RESULT_FILE="$result_file" run_test "$test_file"
        ) >"$output_file" 2>&1 &

        batch_test_files+=("$test_file")
        batch_result_files+=("$result_file")
        batch_output_files+=("$output_file")
        batch_pids+=("$!")
        batch_size=$((batch_size + 1))

        if [ $batch_size -lt $JOBS ]; then
            continue
        fi

        for pid in "${batch_pids[@]}"; do
            wait "$pid" || true
        done
        for i in "${!batch_test_files[@]}"; do
            process_parallel_result "${batch_result_files[$i]}" "${batch_output_files[$i]}"
        done

        batch_size=0
        batch_test_files=()
        batch_result_files=()
        batch_output_files=()
        batch_pids=()
    done < <(printf "%s\n" "$TEST_FILES")

    if [ $batch_size -gt 0 ]; then
        for pid in "${batch_pids[@]}"; do
            wait "$pid" || true
        done
        for i in "${!batch_test_files[@]}"; do
            process_parallel_result "${batch_result_files[$i]}" "${batch_output_files[$i]}"
        done
    fi

    rm -rf "$temp_dir"
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
        local suite_start=$(now_time)
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

        local suite_end=$(now_time)
        local suite_elapsed=$(diff_durations "$suite_start" "$suite_end")
        if [ $counts_for_timing -eq 1 ]; then
            TIMED_DURATION=$(sum_durations "$TIMED_DURATION" "$suite_elapsed")
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
        -j|--jobs)
            if [ $# -lt 2 ]; then
                echo -e "${RED}Error: -j requires a positive integer${NC}"
                exit 1
            fi
            if ! [[ "$2" =~ ^[0-9]+$ ]] || [ "$2" -le 0 ]; then
                echo -e "${RED}Error: -j requires a positive integer${NC}"
                exit 1
            fi
            JOBS="$2"
            shift 2
            ;;
        --jobs=*)
            value="${1#--jobs=}"
            if ! [[ "$value" =~ ^[0-9]+$ ]] || [ "$value" -le 0 ]; then
                echo -e "${RED}Error: --jobs requires a positive integer${NC}"
                exit 1
            fi
            JOBS="$value"
            shift
            ;;
        -a)
            if [ $# -lt 2 ]; then
                echo -e "${RED}Error: -a requires a value (on/off)${NC}"
                exit 1
            fi
            lower=$(printf "%s" "$2" | tr '[:upper:]' '[:lower:]')
            case "$lower" in
                on|true|1)
                    ARC_ENABLED_OVERRIDE="true"
                    ;;
                off|false|0)
                    ARC_ENABLED_OVERRIDE="false"
                    ;;
                *)
                    echo -e "${RED}Error: invalid value for -a (use on/off)${NC}"
                    exit 1
                    ;;
            esac
            shift 2
            ;;
        --asan)
            SANITIZER_MODE="address"
            SANITIZER_FLAGS=("-fsanitize=address" "-fno-omit-frame-pointer")
            shift
            ;;
        --valgrind)
            VALGRIND_MODE=1
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
            echo "  -j, --jobs N        Run up to N single-file tests in parallel"
            echo "  -a on|off           Toggle ARC lowering (--arc-enabled) for this run"
            echo "      --asan          Link runtime test binaries with AddressSanitizer"
            echo "      --valgrind      Run runtime tests under valgrind"
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
            echo "  $0 -j 8             # Run tests with 8 workers"
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

if [ -n "$ARC_ENABLED_OVERRIDE" ]; then
    EXTRA_COMPILER_ARGS+=("--arc-enabled=$ARC_ENABLED_OVERRIDE")
fi

if [ "$SANITIZER_MODE" = "address" ]; then
    echo "Sanitizer mode: address"
fi

if [ $VALGRIND_MODE -eq 1 ]; then
    if ! command -v valgrind >/dev/null 2>&1; then
        echo -e "${RED}Error: valgrind requested but not found in PATH${NC}"
        exit 1
    fi
    echo "Runtime wrapper: valgrind"
fi

if [ "$SANITIZER_MODE" = "address" ] && [ $RUNTIME_LIB_IS_TEMP -eq 1 ] && [ -n "$RUNTIME_LIB" ]; then
    if ! clang "${SANITIZER_FLAGS[@]}" -c runtime/test_runtime_stub.c -o "$RUNTIME_LIB" 2>/dev/null; then
        echo -e "${YELLOW}Warning: failed to rebuild temporary stub runtime with ASan instrumentation${NC}"
    fi
fi

if [ $JOBS -gt 1 ] && { [ $VERBOSE_MODE -eq 1 ] || [ $FAILURES_ONLY -eq 0 ]; }; then
    echo -e "${YELLOW}Note: parallel mode currently supports failures-only compact output; using serial mode.${NC}"
    JOBS=1
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

if [ $JOBS -gt 1 ] && [ -n "$TEST_FILES" ]; then
    run_single_tests_parallel
else
    for test_file in $TEST_FILES; do
        run_test "$test_file"
    done
fi

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
    printf "  Total elapsed: %.2fs\n" "$TIMED_DURATION"
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
