# Hybrid Language Test Suite

This directory contains the comprehensive test suite for the Hybrid programming language compiler.

## Test Organization

Tests are organized into the following categories:

### `arrays/`
Tests for array functionality including:
- Array declarations and initialization
- Array indexing and access
- Global arrays

### `basic/`
Basic language features:
- Expressions and evaluation
- Comments
- Brackets and grouping
- Variable initialization
- Module usage (use statements)

### `control_flow/`
Control flow constructs:
- If-else statements
- While loops
- For-each loops
- Break statements
- Skip (continue) statements

### `errors/`
Tests that are expected to fail (negative tests):
- Uninitialized variable errors
- Type errors
- Other compilation failures

### `functions/`
Function-related tests:
- Function definitions
- Parameters and arguments
- Code generation

### `operators/`
Operator tests:
- Arithmetic operators
- Bitwise operators
- Boolean operators
- Compound assignments
- Increment/decrement operators
- Modulo operator

### `structs/`
Struct (user-defined type) tests:
- Basic struct definitions
- Advanced struct features (nested structs, member access)

### `types/`
Type system tests:
- Boolean types
- Character types
- String types
- Type casting
- Sized integer types
- Null values
- Type tracking and inference

## Running Tests

Use the `run_tests.sh` script in the project root:

```bash
# Run all tests
./run_tests.sh

# Run all tests in a specific category
./run_tests.sh structs
./run_tests.sh operators

# Run tests matching a pattern
./run_tests.sh test_bool
./run_tests.sh cast

# Run a specific test file
./run_tests.sh test/types/test_bool.hy

# Run with verbose output (shows test content and output)
./run_tests.sh -v
./run_tests.sh -v structs
```

## Writing New Tests

1. Create a `.hy` file in the appropriate category directory
2. Name it descriptively (e.g., `test_feature_name.hy`)
3. Tests that should fail should include "fail" or "error" in the filename
4. Add comments explaining what the test is verifying

## Test Conventions

- Tests should be self-contained
- Use comments to explain what is being tested
- Expected failures should have "fail" or "error" in the filename
- Keep tests focused on a single feature or aspect