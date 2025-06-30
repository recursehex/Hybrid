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

echo -e "${BLUE}Hybrid Compiler Test Suite${NC}"
echo "==============================="

# Check if hybrid executable exists
if [ ! -f "./hybrid" ]; then
    echo -e "${RED}Error: hybrid executable not found. Run 'make' first.${NC}"
    exit 1
fi

# Find all test files in test/ directory
TEST_FILES=$(find test/ -name "*.hy" -type f | sort)

if [ -z "$TEST_FILES" ]; then
    echo -e "${RED}No test files found in test/ directory${NC}"
    exit 1
fi

echo "Found test files:"
for test_file in $TEST_FILES; do
    echo "  - $test_file"
done
echo

# Function to run a single test
run_test() {
    local test_file="$1"
    local test_name=$(basename "$test_file" .hy)
    
    echo -e "${YELLOW}Running test: $test_name${NC}"
    echo "----------------------------------------"
    
    # Run the test and capture both output and exit code
    local output
    local exit_code
    output=$(./hybrid < "$test_file" 2>&1)
    exit_code=$?
    
    # Check for error patterns in output
    local has_errors=0
    if echo "$output" | grep -q "Error:"; then
        has_errors=1
    elif echo "$output" | grep -q "Failed to generate"; then
        has_errors=1
    elif echo "$output" | grep -q "Unknown function"; then
        has_errors=1
    elif echo "$output" | grep -q "Unknown variable"; then
        has_errors=1
    elif echo "$output" | grep -q "invalid binary operator"; then
        has_errors=1
    elif echo "$output" | grep -q "Expected.*after"; then
        has_errors=1
    fi
    
    # Special case: tests that should fail (have "fail" in name)
    if [[ "$test_name" == *"fail"* ]] || [[ "$test_name" == *"error"* ]]; then
        if [ $has_errors -eq 1 ] || [ $exit_code -ne 0 ]; then
            echo -e "${GREEN}✓ PASSED: $test_name (correctly failed as expected)${NC}"
            ((PASSED_TESTS++))
        else
            echo -e "${RED}✗ FAILED: $test_name (should have failed but didn't)${NC}"
            ((FAILED_TESTS++))
        fi
    else
        # Normal tests should not have errors
        if [ $has_errors -eq 0 ] && [ $exit_code -eq 0 ]; then
            echo -e "${GREEN}✓ PASSED: $test_name${NC}"
            ((PASSED_TESTS++))
        else
            echo -e "${RED}✗ FAILED: $test_name${NC}"
            if [ $exit_code -ne 0 ]; then
                echo -e "${RED}  Exit code: $exit_code${NC}"
            fi
            if [ $has_errors -eq 1 ]; then
                echo -e "${RED}  Errors found in output:${NC}"
                echo "$output" | grep -E "(Error:|Failed to generate|Unknown function|Unknown variable|invalid binary operator|Expected.*after)" | head -3
            fi
            ((FAILED_TESTS++))
        fi
    fi
    ((TOTAL_TESTS++))
    echo
}

# Function to run a test with visible output (for debugging)
run_test_verbose() {
    local test_file="$1"
    local test_name=$(basename "$test_file" .hy)
    
    echo -e "${BLUE}=== Test: $test_name ===${NC}"
    echo "File: $test_file"
    echo "Content:"
    cat "$test_file"
    echo
    echo "Output:"
    if ./hybrid < "$test_file"; then
        echo -e "${GREEN}✓ Test completed${NC}"
    else
        local exit_code=$?
        echo -e "${RED}✗ Test failed with exit code: $exit_code${NC}"
    fi
    echo
    echo "========================================"
    echo
}

# Check command line arguments
if [ "$1" = "-v" ] || [ "$1" = "--verbose" ]; then
    echo "Running tests in verbose mode..."
    echo
    for test_file in $TEST_FILES; do
        run_test_verbose "$test_file"
    done
elif [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    echo "Usage: $0 [OPTIONS] [TEST_PATTERN]"
    echo
    echo "Options:"
    echo "  -v, --verbose       Show detailed output for each test"
    echo "  -h, --help          Show this help message"
    echo
    echo "Examples:"
    echo "  $0                  # Run all tests"
    echo "  $0 -v               # Run all tests with verbose output"
    echo "  $0 null             # Run only tests matching 'null'"
    echo "  $0 test_expr.hy     # Run specific test file"
    exit 0
elif [ -n "$1" ]; then
    # Filter tests by pattern
    if [ -f "test/$1" ]; then
        # Specific file
        TEST_FILES="test/$1"
    else
        # Pattern matching
        TEST_FILES=$(find test/ -name "*$1*.hy" -type f | sort)
        if [ -z "$TEST_FILES" ]; then
            echo -e "${RED}No test files found matching pattern: $1${NC}"
            exit 1
        fi
    fi
    echo "Running tests matching pattern: $1"
    echo
fi

# Run all tests
for test_file in $TEST_FILES; do
    run_test "$test_file"
done

# Print summary
echo "==============================="
echo -e "${BLUE}Test Summary${NC}"
echo "Total tests:  $TOTAL_TESTS"
echo -e "Passed:       ${GREEN}$PASSED_TESTS${NC}"
echo -e "Failed:       ${RED}$FAILED_TESTS${NC}"

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