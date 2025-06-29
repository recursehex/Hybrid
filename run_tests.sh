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
TEST_FILES=$(find test/ -name "*.txt" -type f | sort)

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
    local test_name=$(basename "$test_file" .txt)
    
    echo -e "${YELLOW}Running test: $test_name${NC}"
    echo "----------------------------------------"
    
    # Run the test and capture output
    if ./hybrid < "$test_file" > /dev/null 2>&1; then
        echo -e "${GREEN}✓ PASSED: $test_name${NC}"
        ((PASSED_TESTS++))
    else
        local exit_code=$?
        echo -e "${RED}✗ FAILED: $test_name (exit code: $exit_code)${NC}"
        ((FAILED_TESTS++))
    fi
    ((TOTAL_TESTS++))
    echo
}

# Function to run a test with visible output (for debugging)
run_test_verbose() {
    local test_file="$1"
    local test_name=$(basename "$test_file" .txt)
    
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
    echo "  -v, --verbose    Show detailed output for each test"
    echo "  -h, --help       Show this help message"
    echo
    echo "Examples:"
    echo "  $0                    # Run all tests"
    echo "  $0 -v                 # Run all tests with verbose output"
    echo "  $0 null               # Run only tests matching 'null'"
    echo "  $0 test_expr.txt      # Run specific test file"
    exit 0
elif [ -n "$1" ]; then
    # Filter tests by pattern
    if [ -f "test/$1" ]; then
        # Specific file
        TEST_FILES="test/$1"
    else
        # Pattern matching
        TEST_FILES=$(find test/ -name "*$1*.txt" -type f | sort)
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