# Testing

## Overview

The Hybrid compiler includes a comprehensive test suite with automated test discovery and intelligent error detection. Tests are written in `.hy` files and can be run individually or as a complete suite.

## Running Tests

### Test Runner Script

The primary way to run tests is using the platform-specific test runner:

#### Unix/macOS

```bash
# Run all tests
./run_tests.sh

# Run tests with verbose output
./run_tests.sh -v

# Run all tests with ARC lowering disabled
./run_tests.sh -a off

# Run specific test or pattern
./run_tests.sh expr             # Runs expr.hy
./run_tests.sh array            # Runs all tests containing "array"
./run_tests.sh multi_unit       # Runs only multi-file directory tests

# Show help
./run_tests.sh -h
```

#### Windows

```cmd
# Run all tests
run_tests.bat

# Run tests with verbose output
run_tests.bat -v

# Run specific test or pattern
run_tests.bat expr          # Runs expr.hy
run_tests.bat array         # Runs all tests containing "array"

# Show help
run_tests.bat -h
```

### Running Individual Tests

Tests can also be run directly with the compiler:

#### Unix/macOS
```bash
# Run a specific test file
./build/hybrid < test/codegen.hy

# Run with output redirection
./build/hybrid < test/expr.hy > output.txt

# Run with error output
./build/hybrid < test/fail.hy 2>&1
```

#### Windows
```cmd
# Run a specific test file
build\hybrid.exe < test\codegen.hy

# Run with output redirection
build\hybrid.exe < test\expr.hy > output.txt

# Run with error output
build\hybrid.exe < test\fail.hy 2>&1
```

## ARC runtime and diagnostics

- When `clang` is available the harness builds a lightweight stub runtime (`runtime/test_runtime_stub.c`) for tests that do not emit ARC/smart-pointer symbols, keeping quick suites fast.
- If the generated IR references ARC helpers (`hybrid_retain`, `__hybrid_shared_control`, smart pointer shims) or any of `HYBRID_ARC_DEBUG`, `HYBRID_ARC_TRACE_RUNTIME`, `HYBRID_ARC_LEAK_DETECT`, or `HYBRID_ARC_VERIFY_RUNTIME` are set, the runner links the real runtime (`src/runtime_support.cpp`, `src/runtime/arc.cpp`, `src/runtime/weak_table.cpp`, `src/memory/ref_count.cpp`) instead. Set one of those env vars in CI to force the full runtime for ARC suites.
- Toggle ARC lowering with `./run_tests.sh -a on|off` or `// RUN_OPTS: --arc-enabled=false`; with ARC disabled the compiler omits retain/release insertion and ARC-specific diagnostics so fixtures can demonstrate the difference between ARC-on and ARC-off behavior.

## Test Suite Features

### Automatic Test Discovery

The test runner automatically finds all `.hy` files in the `test/` directory:
- No need to register tests manually
- New tests are discovered automatically
- Tests are run in alphabetical order
- Tests organized by subdirectories (types/, errors/, structs/, etc.)

**Multi-file directories**

- Any subdirectory under `test/multi_unit` is treated as a single compilation unit.
- All `.hy` files in that directory are passed to the driver together using `hybrid file1.hy file2.hy -o a.out` semantics.
- Directories whose name ends with `_fail` (or that contain an `EXPECT_FAIL` file) are expected to fail.
- These directory-based tests can be run in isolation via `./run_tests.sh multi_unit`.

> [!IMPORTANT]
> When adding a multi-file test that should fail, be sure the directory name ends with `_fail` or `_error` (or includes `EXPECT_FAIL`). Otherwise, the harness will expect the suite to pass and will flag the run as a regression.

### Binary Execution

The test suite compiles and executes generated LLVM IR:

- **Automatic compilation**: If `clang` is available, tests are compiled to native binaries
- **Runtime library**: A temporary runtime library is created with support functions (e.g. `print()`)
- **Execution verification**: Tests are run and exit codes are checked. A Hybrid program that returns a non-zero status from `int main()` is reported as a failing test.
- **Assert detection**: Runtime aborts (exit code 134/SIGABRT) are detected as test failures
- **Cleanup**: All temporary files are automatically removed

This ensures that generated code not only compiles but also executes correctly.

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

Additionally, runtime errors are detected:

| Exit Code | Description |
|-----------|-------------|
| `134` (SIGABRT) | Runtime abort (e.g. from `assert` statements) |
| Non-zero | Program returned a non-zero status from `int main()` or raised another runtime error |

### Diagnostic Quality

Error reporting is centralized through `reportCompilerError()` in `compiler_session.cpp`. Whenever new parser or code generation features are added:
- Capture precise token locations via the lexer's `SourceLocation` plumbing and pass them through the parser.
- Emit diagnostics with contextual hints by calling `LogError`, `LogErrorS`, `LogErrorP`, or `LogErrorV` so messages stay consistent.
- Add regression coverage in `test/errors/` whenever a new diagnostic string is introduced to ensure the test runner continues to recognize expected failures.

> [!IMPORTANT]
> Every new diagnostic message needs a matching `test/errors/` fixture. Without it, the wording can drift silently, breaking IDE/CLI consumers that parse Hybrid's standardized error format.

Treat descriptive errors as a first-class requirement. Pipe new failure paths through the shared helpers instead of printing ad-hoc messages.

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
  - test/bool.hy
  - test/expr.hy
  ...

Running test: bool
✓ PASSED: bool

Running test: expr
✓ PASSED: expr

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
| `expr.hy` | Arithmetic expressions and operations |
| `testbool.hy` | Boolean literals and expressions |
| `codegen.hy` | Complete function code generation |
| `if_else.hy` | If-else statements and comparisons |
| `boolean_ops.hy` | Boolean operators (&&, \|\|, !) |
| `while.hy` | While loops and nested loops |
| `arrays.hy` | Comprehensive array operations |
| `null.hy` | String and null initialization |
| `test.hy` | General integration tests |

### Test File Structure

A typical test file contains:

```c
// test/feature.hy
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
2. **Clear naming**: Use descriptive test file names (e.g. `array_indexing.hy`)
3. **Comments**: Add comments explaining what each test case validates
4. **Coverage**: Include both typical usage and edge cases
5. **Expected failures**: Name files with "fail" for tests that should error

### Example Test

```c
// test/arithmetic.hy
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
// test/undefined_var_fail.hy
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

1. Run the failing test with verbose mode: `./run_tests.sh -v name`
2. Check the exact error message in the output
3. Run the test file directly: `./hybrid < test/name.hy`
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
