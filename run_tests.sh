#!/bin/bash

# Hybrid Compiler Test Runner
# Automatically discovers and runs all test files in the test/ directory

set -e  # Exit on any error

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
FAILURES_ONLY=0
TEST_PATTERN=""
RUN_MULTI_UNIT_TESTS=1

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

# Create runtime library for test execution if clang is available
RUNTIME_LIB=""
if command -v clang &> /dev/null; then
    RUNTIME_LIB=$(mktemp /tmp/hybrid_runtime.XXXXXX)
    mv "$RUNTIME_LIB" "${RUNTIME_LIB}.o"
    RUNTIME_LIB="${RUNTIME_LIB}.o"
    cat > "${RUNTIME_LIB%.o}.c" << 'EOF'
#include <stdio.h>
#include <stdlib.h>

void print(int x) {
    printf("%d\n", x);
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

# Find all test files in test/ directory and subdirectories
TEST_FILES=$(find test/ -path 'test/multi_unit' -prune -o -name "*.hy" -type f -print | sort)

if [ -z "$TEST_FILES" ]; then
    echo -e "${RED}No test files found in test/ directory${NC}"
    exit 1
fi

MULTI_UNIT_TEST_COUNT=0
if [ -d "test/multi_unit" ]; then
    while IFS= read -r -d '' multi_dir; do
        if find "$multi_dir" -maxdepth 1 -name "*.hy" -type f | grep -q .; then
            MULTI_UNIT_TEST_COUNT=$((MULTI_UNIT_TEST_COUNT + 1))
        fi
    done < <(find test/multi_unit -mindepth 1 -maxdepth 1 -type d -print0)
fi

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
    
    # Run the test and capture both output and exit code
    local output
    local exit_code
    output=$($HYBRID_EXEC < "$test_file" 2>&1)
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

    # If compilation succeeded and test is not expected to fail, compile and run with clang
    local runtime_exit_code=0
    local runtime_output=""
    if [ $has_errors -eq 0 ] && [ $exit_code -eq 0 ] && [[ "$test_name" != *"fail"* ]] && [[ "$test_name" != *"error"* ]]; then
        # Check if clang is available and we have runtime library
        if command -v clang &> /dev/null && [ -n "$RUNTIME_LIB" ] && [ -f "$RUNTIME_LIB" ]; then
            # Extract the final complete LLVM module (after "=== Final Generated LLVM IR ===")
            local module_start=$(echo "$output" | grep -n "^=== Final Generated" | tail -1 | cut -d: -f1)
            if [ -n "$module_start" ]; then
                # Start from the line after "=== Final Generated..."
                module_start=$((module_start + 1))
                local clean_ir=$(echo "$output" | tail -n +$module_start | grep -v "^ready>" | grep -v "^Parsed" | grep -v "^Generated")

                # Create temporary files
                local temp_ir=$(mktemp /tmp/hybrid_test.XXXXXX)
                mv "$temp_ir" "${temp_ir}.ll"
                temp_ir="${temp_ir}.ll"
                local temp_bin=$(mktemp /tmp/hybrid_bin.XXXXXX)
                echo "$clean_ir" > "$temp_ir"

                # Compile IR to binary with clang, linking with runtime library
                if clang "$temp_ir" "$RUNTIME_LIB" -o "$temp_bin" &> /dev/null; then
                    # Execute the binary and capture exit code properly
                    set +e  # Temporarily disable exit on error
                    runtime_output=$("$temp_bin" 2>&1)
                    runtime_exit_code=$?
                    set -e  # Re-enable exit on error

                    # Check for runtime abort (assert failures): exit code 134 = SIGABRT
                    if [ $runtime_exit_code -eq 134 ]; then
                        has_errors=1
                    fi
                fi

                # Clean up temporary files
                rm -f "$temp_ir" "$temp_bin"
            fi
        fi
    fi

    # For fail/error tests that compiled cleanly, run the generated program to
    # detect failure modes like non-zero exit codes.
    if [[ "$test_name" == *"fail"* ]] || [[ "$test_name" == *"error"* ]]; then
        if [ $has_errors -eq 0 ] && [ $exit_code -eq 0 ]; then
            if command -v clang &> /dev/null && [ -n "$RUNTIME_LIB" ] && [ -f "$RUNTIME_LIB" ]; then
                local module_start=$(echo "$output" | grep -n "^=== Final Generated" | tail -1 | cut -d: -f1)
                if [ -n "$module_start" ]; then
                    module_start=$((module_start + 1))
                    local clean_ir=$(echo "$output" | tail -n +$module_start | grep -v "^ready>" | grep -v "^Parsed" | grep -v "^Generated")

                    local temp_ir=$(mktemp /tmp/hybrid_test_fail.XXXXXX)
                    mv "$temp_ir" "${temp_ir}.ll"
                    temp_ir="${temp_ir}.ll"
                    local temp_bin=$(mktemp /tmp/hybrid_bin_fail.XXXXXX)
                    echo "$clean_ir" > "$temp_ir"

                    if clang "$temp_ir" "$RUNTIME_LIB" -o "$temp_bin" &> /dev/null; then
                        set +e
                        runtime_output=$("$temp_bin" 2>&1)
                        runtime_exit_code=$?
                        set -e

                        if [ $runtime_exit_code -ne 0 ]; then
                            has_errors=1
                        fi
                    fi

                    rm -f "$temp_ir" "$temp_bin"
                fi
            fi
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
        local expect="pass"
        if [ -f "${dir}/EXPECT_FAIL" ] || [[ "$test_name" == *_fail ]]; then
            expect="fail"
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
        output=$("$HYBRID_EXEC" "${cmd_files[@]}" "${output_args[@]}" 2>&1)
        local status=$?
        set -e

        TOTAL_TESTS=$((TOTAL_TESTS + 1))

        if { [ "$expect" = "pass" ] && [ $status -eq 0 ]; } || { [ "$expect" = "fail" ] && [ $status -ne 0 ]; }; then
            PASSED_TESTS=$((PASSED_TESTS + 1))
            if [ $FAILURES_ONLY -eq 0 ]; then
                echo -e "${GREEN}✓ PASSED: $test_name${NC}"
            fi
        else
            FAILED_TESTS=$((FAILED_TESTS + 1))
            ensure_progress_newline
            echo -e "${RED}✗ FAILED: $test_name${NC}"
            if [ "$expect" = "pass" ]; then
                echo -e "${RED}  Expected success but command failed with status $status${NC}"
            else
                echo -e "${RED}  Expected failure but command succeeded${NC}"
            fi
            echo "--- Output ---"
            echo "$output"
            echo "--------------"
        fi

        rm -f "$temp_output"

        if [ $FAILURES_ONLY -eq 1 ]; then
            PROGRESS_COUNT=$((PROGRESS_COUNT + 1))
            update_progress_display
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
            shift
            ;;
        -f|--failures-only)
            FAILURES_ONLY=1
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS] [TEST_PATTERN]"
            echo
            echo "Options:"
            echo "  -v, --verbose       Show detailed output for each test"
            echo "  -f, --failures-only Only show failing tests (hide passing tests)"
            echo "  -h, --help          Show this help message"
            echo
            echo "Test Pattern:"
            echo "  Can be a category name, file path, or pattern to match"
            echo
            echo "Examples:"
            echo "  $0                  # Run all tests"
            echo "  $0 -v               # Run all tests with verbose output"
            echo "  $0 -f               # Run all tests but only show failures"
            echo "  $0 -f -v            # Show verbose output only for failing tests"
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

# Filter tests if pattern provided
if [ -n "$TEST_PATTERN" ]; then
    RUN_MULTI_UNIT_TESTS=0
    # Check if it's a category name
    if [ -d "test/$TEST_PATTERN" ]; then
        if [ "$TEST_PATTERN" = "multi_unit" ]; then
            TEST_FILES=""
            echo "Running multi-unit manifest tests"
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
    echo -e "${GREEN}(No failures shown - all tests passed)${NC}"
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
