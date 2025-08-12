# Testing

## Overview

The Hybrid compiler includes a comprehensive test suite with automated test discovery and intelligent error detection. Tests are written in `.hy` files and can be run individually or as a complete suite.

## Running Tests

### Test Runner Script

The primary way to run tests is using the `run_tests.sh` script:

```bash
# Run all tests
./run_tests.sh

# Run tests with verbose output
./run_tests.sh -v

# Run specific test or pattern
./run_tests.sh test_expr      # Runs test_expr.hy
./run_tests.sh array         # Runs all tests containing "array"

# Show help
./run_tests.sh -h
```

### Running Individual Tests

Tests can also be run directly with the compiler:

```bash
# Run a specific test file
./hybrid < test/test_codegen.hy

# Run with output redirection
./hybrid < test/test_expr.hy > output.txt

# Run with error output
./hybrid < test/test_fail.hy 2>&1
```

## Test Suite Features

### Automatic Test Discovery

The test runner automatically finds all `.hy` files in the `test/` directory:
- No need to register tests manually
- New tests are discovered automatically
- Tests are run in alphabetical order

### Error Detection

The test suite uses pattern matching to detect compilation errors:

| Error Pattern | Description |
|--------------|-------------|
| `"Error:"` | General compilation errors |
| `"Failed to generate"` | Code generation failures |
| `"Unknown function"` | Undefined function calls |
| `"Unknown variable"` | Undefined variable references |
| `"invalid binary operator"` | Unsupported operations |
| `"Expected.*after"` | Syntax errors |

### Test Classification

Tests are classified by their expected behavior:
- **Passing Tests**: Should compile without errors
- **Failing Tests**: Names containing "fail" are expected to produce errors
- **Feature Tests**: Demonstrate specific language features

### Output Format

#### Default Mode

```
Hybrid Compiler Test Suite
===============================
Found test files:
  - test/test_bool.hy
  - test/test_expr.hy
  ...

Running test: test_bool
✓ PASSED: test_bool

Running test: test_expr
✓ PASSED: test_expr

===============================
Test Summary
Total tests:  14
Passed:       14
Failed:       0
All tests passed!
```

#### Verbose Mode (`-v`)

Shows complete compiler output for each test, including:
- Generated LLVM IR
- Any compilation errors
- Parser and code generation messages

## Test Organization

### Test Categories

Tests are organized by feature area:

| Test File | Description |
|-----------|-------------|
| `test_expr.hy` | Arithmetic expressions and operations |
| `test_bool.hy` | Boolean literals and expressions |
| `test_codegen.hy` | Complete function code generation |
| `test_if_else.hy` | If-else statements and comparisons |
| `test_boolean_ops.hy` | Boolean operators (&&, \|\|, !) |
| `test_while.hy` | While loops and nested loops |
| `test_arrays.hy` | Comprehensive array operations |
| `test_null.hy` | String and null initialization |
| `test.hy` | General integration tests |

### Test File Structure

A typical test file contains:

```c
// test/test_feature.hy
// Test description comment

// Test case 1: Basic functionality
int basicTest()
{
    return 42
}

// Test case 2: Edge cases
int edgeCase(int x)
{
    if x < 0 { return -1 }
    return x
}

// Top-level expressions for REPL testing
basicTest()
edgeCase(-5)
edgeCase(10)
```

## Writing Tests

### Guidelines

1. **One feature per file**: Focus each test file on a specific language feature
2. **Clear naming**: Use descriptive test file names (e.g., `test_array_indexing.hy`)
3. **Comments**: Add comments explaining what each test case validates
4. **Coverage**: Include both typical usage and edge cases
5. **Expected failures**: Name files with "fail" for tests that should error

### Example Test

```c
// test/test_arithmetic.hy
// Tests arithmetic operations and precedence

// Test basic operations
int testBasic()
{
    int a = 10 + 5    // 15
    int b = 20 - 8    // 12
    int c = 3 * 4     // 12
    int d = 15 / 3    // 5
    return a + b + c + d  // 44
}

// Test operator precedence
int testPrecedence()
{
    return 2 + 3 * 4  // Should be 14, not 20
}

// Test mixed types
double testMixed()
{
    int x = 5
    double y = 2.5
    return x + y  // 7.5
}

// Run the tests
testBasic()       // Expected: 44
testPrecedence()  // Expected: 14
testMixed()       // Expected: 7.5
```

### Negative Tests

For tests that should fail, include "fail" in the filename:

```c
// test/test_undefined_var_fail.hy
// This test should fail with "Unknown variable" error

int useUndefined()
{
    return x  // Error: x is not defined
}
```

## Test Coverage

### Current Coverage

The test suite covers:
- ✓ Arithmetic expressions
- ✓ Boolean operations and literals
- ✓ Function definitions and calls
- ✓ Variable declarations and assignment
- ✓ If-else statements
- ✓ While loops
- ✓ Arrays (declaration, indexing, assignment)
- ✓ Type promotion and casting
- ✓ String and null handling
- ✓ Global variables
- ✓ External function declarations

### Pending Coverage

- Foreach loop code generation
- Complex nested structures
- Error recovery testing
- Performance benchmarks

## Continuous Integration

### Future Enhancements

Planned CI improvements:
- GitHub Actions integration
- Automated testing on pull requests
- Coverage reporting
- Performance regression testing
- Multi-platform testing (Linux, macOS, Windows)

## Debugging Failed Tests

### Common Issues

1. **Syntax Errors**: Check for missing braces or incorrect keywords
2. **Type Errors**: Ensure types match in operations and function calls
3. **Undefined References**: Verify all variables and functions are defined
4. **LLVM Errors**: Check LLVM installation and version compatibility

### Debug Steps

1. Run the failing test with verbose mode: `./run_tests.sh -v test_name`
2. Check the exact error message in the output
3. Run the test file directly: `./hybrid < test/test_name.hy`
4. Add print statements or simplify the test to isolate the issue
5. Check recent changes that might have affected the feature

## Test Utilities

### Future Test Helpers

Planned testing utilities:
- Assert functions for validating results
- Test fixtures for common setup
- Performance timing utilities
- Memory usage tracking
- Differential testing against reference implementation