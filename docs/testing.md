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

# Run single-file tests in parallel (compact failures-only mode)
./run_tests.sh -j 4

# Run tests with verbose output
./run_tests.sh -v

# Run all tests with ARC lowering disabled
./run_tests.sh -a off

# Link runtime test binaries with AddressSanitizer
# (use with an ASan-built compiler binary)
./run_tests.sh --asan arc

# Run specific test or pattern
./run_tests.sh expr             # Runs expr.hy
./run_tests.sh array            # Runs all tests containing "array"
./run_tests.sh multi_unit       # Runs only multi-file directory tests

# Show help
./run_tests.sh -h
```

> [!NOTE]
> `-j/--jobs` parallelizes single-file tests and is currently intended for compact failures-only runs. If you also pass `-v` or `-f`, the runner falls back to serial execution to preserve readable output ordering.

#### Windows

```cmd
# Run all tests
run_tests.bat

# Run tests with verbose output
run_tests.bat -v

# Run single-file tests in parallel (compact failures-only mode)
run_tests.bat -j 4

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

- The CMake build now emits reusable runtime archives in `build/`: `libhybrid_test_stub.a` and `libhybrid_runtime_test.a`.
- The test harness prefers those archives for runtime linking. If they are missing, it falls back to on-the-fly runtime compilation so local workflows still work.
- For non-ARC runtime tests, the harness links the lightweight stub runtime. If generated IR references ARC/smart-pointer helpers (`hybrid_retain`, `__hybrid_shared_control`, smart pointer shims) or any of `HYBRID_ARC_DEBUG`, `HYBRID_ARC_TRACE_RUNTIME`, `HYBRID_ARC_LEAK_DETECT`, or `HYBRID_ARC_VERIFY_RUNTIME` are set, it links the full ARC runtime archive instead.
- Toggle ARC lowering with `./run_tests.sh -a on|off` or `// RUN_OPTS: --arc-enabled=false`; with ARC disabled the compiler omits retain/release insertion and ARC-specific diagnostics so fixtures can demonstrate the difference between ARC-on and ARC-off behavior.
- Set `HYBRID_EXEC=<path/to/hybrid>` to run tests against a non-default compiler binary (for example an ASan build under `build-asan/`).
- Pass `--asan` (or set `HYBRID_TEST_SANITIZER=address`) to link runtime test binaries with AddressSanitizer flags.

## ARC ASan lane

Use the helper script to configure an ASan build and run ARC suites against it:

```bash
# Defaults to running: arc/memory, arc/arc_off, arc/debug, and errors/arc
./scripts/run_arc_asan.sh

# Run a specific category or pattern
./scripts/run_arc_asan.sh arc/memory
```

Manual setup is also available:

```bash
./build.sh --asan -d
HYBRID_EXEC=./build/hybrid ./run_tests.sh --asan arc
```

Notes:
- The script configures `build-asan/` with `-DHYBRID_ENABLE_ASAN=ON`.
- It runs tests with `HYBRID_EXEC=./build-asan/hybrid` and `--asan` so ARC runtime paths are exercised with sanitizer instrumentation.

## ARC Valgrind lane (Linux)

Run ARC suites under valgrind on Linux:

```bash
# Defaults to running: arc/memory, arc/arc_off, arc/debug, and errors/arc
./scripts/run_arc_valgrind.sh

# Run a specific category or pattern
./scripts/run_arc_valgrind.sh arc/debug
```

Notes:
- This path is Linux-only and requires `valgrind` in `PATH`.
- `run_tests.sh --valgrind` wraps runtime binaries in valgrind with:
  - `--leak-check=full`
  - `--show-leak-kinds=all`
  - `--errors-for-leak-kinds=none`
  - `--error-exitcode=101`
- Leak-only fixtures remain valid under this mode because leak kinds are reported but not counted as hard errors.

## Running ARC Benchmarks

Use the benchmark driver to compare ARC-on vs ARC-off runtime timing on dedicated ARC workloads.

```bash
# Build, run all ARC benchmarks 5x per mode, and write artifacts
./scripts/arc_bench.sh

# Include compiler stage pass timing capture
./scripts/arc_bench.sh --pass-timing

# Run a subset with custom thresholds
./scripts/arc_bench.sh --runs 7 --warn-threshold 6 --fail-threshold 12 object_churn
```

What it does:
- Runs each benchmark in `test/bench/arc/*.hy` under both `-a on` and `-a off`.
- Repeats each mode N times and records median elapsed time.
- Writes machine-readable artifacts to `build/arc-bench/`:
  - `arc_bench_raw_<timestamp>.csv`
  - `arc_bench_summary_<timestamp>.csv`
  - `arc_bench_<timestamp>.json`
- Optional (`--pass-timing`): captures compiler stage timings from `--pass-timing` / `HYBRID_PASS_TIMING=1` into:
  - `arc_bench_pass_timing_<timestamp>.csv`
- Applies threshold policy:
  - warn if ARC-on delta > `--warn-threshold` (default `8%`)
  - fail if ARC-on delta > `--fail-threshold` (default `15%`, or `off` to disable)

Notes:
- Keep baselines architecture-specific (for example, Apple Silicon vs Linux x86_64).
- Prefer running benchmarks on an idle machine and compare medians, not single runs.
- CI runs a lightweight Linux smoke lane (`arc-perf-smoke`) with `--warn-threshold 8 --fail-threshold 12` and uploads trend artifacts:
  - `arc_bench_summary_<timestamp>.csv`
  - `arc_bench_pass_timing_<timestamp>.csv`
- CI also includes a scheduled trend lane (`ARC Perf Trend`, weekly + manual dispatch) that runs with `--runs 7 --fail-threshold off`, publishes a step summary table, and retains trend artifacts for 90 days.

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

- **Direct IR emission for runtime tests**: For tests that require runtime execution, the harness invokes `hybrid <test.hy> -o <temp>.ll` and links the emitted IR directly. This avoids scraping IR from interactive stderr output on the fast path.
- **Automatic compilation**: If `clang` is available, tests are compiled to native binaries
- **Runtime library selection**: The harness links either the stub archive or the full ARC runtime archive depending on referenced symbols/options
- **Execution verification**: Tests are run and exit codes are checked. A Hybrid program that returns a non-zero status from `int main()` is reported as a failing test.
- **Assert detection**: Runtime aborts (exit code 134/SIGABRT) are detected as test failures
- **Cleanup**: All temporary files are automatically removed

This ensures that generated code not only compiles but also executes correctly.

### Performance Notes

- `./run_tests.sh -j N` can reduce wall-clock time significantly by running single-file tests concurrently.
- The `Timing (runtime tests only)` summary is aggregated per-test runtime duration, not wall-clock elapsed for the whole suite. In parallel mode this number can be higher than actual end-to-end time because multiple tests run at once.

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
- **Failing Tests**: Names containing "fail" or "error" are expected to produce compile-time failures by default
- **Feature Tests**: Demonstrate specific language features

You can override the default classification with inline annotations:
- `// EXPECT_FAIL: compile|runtime|any` to require a specific failure kind (defaults to `compile` when omitted)
- `// EXPECT_PASS` to force a test to be treated as passing even if the filename contains `fail`/`error`
- `// EXPECT_EXIT: <code|nonzero|zero|abort>` to assert the runtime exit status when a program is executed
- `// EXPECT_DIAGNOSTIC: <text>` (or `// EXPECT_ERROR: <text>`) to declare an expected compile diagnostic

For compile-failure tests, diagnostic matching is strict:
- Every expected diagnostic must appear.
- Any additional compile diagnostic marks the test as failed.
- There is no fallback source; compile-failure tests must declare diagnostics inline.
- Prefer stable message fragments (for example, `Unknown variable name: x`) and avoid location suffixes like `(near ...)`.

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

For precise failure assertions, prefer explicit diagnostic expectations:

```c
// EXPECT_DIAGNOSTIC: Unknown variable name: x
```

If the failure should occur at runtime (for example, a non-zero `main` return),
add an explicit expectation:

```c
// EXPECT_FAIL: runtime
// EXPECT_EXIT: 1
```

If a file name contains `fail`/`error` for non-failure reasons (for example, ARC
escape analysis fixtures), add `// EXPECT_PASS` to force a passing expectation.

Compile-failure fixtures should always include explicit `EXPECT_DIAGNOSTIC` lines
in the file itself so expectation updates travel with the test.

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
