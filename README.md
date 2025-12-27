<h1 align="center">
 <img src="docs/img/hybrid_logo.svg" alt="Hybrid logo"/>
</h1>

# Hybrid Programming Language

Hybrid is a performant, statically-typed programming language that combines the power of C/C++ with the flexibility of Python. It has a modern, expressive syntax, interoperating with C/C++ while being memory-safe.

## Contents
- [Quick Start](#quick-start)
- [Documentation](#documentation)
- [Build System](#build-system)
- [Project Structure](#project-structure)
- [Current Status](#current-status)
- [Contributing](#contributing)
- [License](#license)

## Quick Start

### Requirements

#### macOS

Install LLVM and CMake via [Homebrew](https://brew.sh):

```bash
brew install llvm cmake
```

Ensure Homebrew's LLVM clang is first on PATH by adding the following to your `~/.zprofile` or `~/.zshrc`:

```bash
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
```

> [!NOTE]
> Hybrid's macOS builds use the Homebrew-provided LLVM clang toolchain. If you prefer not to modify your shell profile, make sure `/opt/homebrew/opt/llvm/bin` is on your PATH before configuring the project.

#### Ubuntu/Debian

Install LLVM, CMake, and build tools:

```bash
apt install llvm-dev cmake build-essential
```

#### Windows
Install LLVM, CMake, and Ninja using [winget](https://learn.microsoft.com/en-us/windows/package-manager/winget/) or [Chocolatey](https://chocolatey.org/):

```cmd
winget install LLVM.LLVM
choco install llvm

winget install Kitware.CMake
choco install cmake

winget install Ninja-build.Ninja
choco install ninja
```

Ensure LLVM's bin directory is in your PATH:
```bash
set PATH=C:\Program Files\LLVM\bin;%PATH%
```

### Building

#### Unix/macOS

Configure and build:
```bash
./build.sh              # Release build
./build.sh -d           # Debug build
./build.sh -c -t        # Clean build and run tests
```

Or use CMake presets:
```bash
cmake --preset=release
cmake --build --preset=release
```

Or manual CMake:
```bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
```

#### Windows

Using build script:

```cmd
build.bat
build.bat -d
build.bat -c -t
```

Or use CMake presets:

```cmd
cmake --preset=release
cmake --build --preset=release
```

Or manual CMake:

```cmd
cmake -B build -G "Ninja" -DCMAKE_BUILD_TYPE=Release
cmake --build build
```

### Running

#### Unix/macOS

Run interactive mode:
```bash
./build/hybrid          # or ./build/release/hybrid with presets
```

Run a program:
```bash
# Interpret a single file from stdin
./build/hybrid < program.hy

# Compile one or more files (clang-style)
./build/hybrid module_a.hy module_b.hy -o my_program    # produces a native binary
./build/hybrid --emit-llvm module.hy -o module.ll       # emit LLVM IR instead

# --emit-llvm is implied when -o ends with .ll/.bc or when you pass -o -
# Without -o, the driver defaults to writing a native a.out next to the CLI

# Run tests
./run_tests.sh
./run_tests.sh multi_unit   # Only the multi-file directory tests
./run_tests.sh -v           # Verbose mode
```

#### Windows

```cmd
# Run interactive mode
build\hybrid.exe        # or build\release\hybrid.exe with presets

# Run a program
build\hybrid.exe < program.hy

# Run tests
run_tests.bat
run_tests.bat -v
```

## Documentation

Comprehensive documentation is available in the `docs/` directory:

- [Language Reference](docs/language-reference.md) - Syntax and language basics
- [Type System](docs/type-system.md) - Types and type safety
- [Functions](docs/functions.md) - Function definitions and calls
- [Control Flow](docs/control-flow.md) - Statements and loops
- [Arrays](docs/arrays.md) - Array types and operations
- [Expressions](docs/expressions.md) - Operators and expressions
- [Examples](docs/examples.md) - Complete example programs
- [Architecture](docs/architecture.md) - Compiler design and internals
- [LLVM Code Generation](docs/llvm-codegen.md) - How code is generated
- [Testing](docs/testing.md) - Test suite and writing tests
- [Multi-Unit Compilation](docs/multi_unit_compilation.md) - Building multiple files together

## Build System

The project uses CMake for cross-platform builds with full support for Windows, Linux, and macOS.

### Prerequisites
- LLVM 20+ with development headers
- C++20 compatible compiler (Clang, GCC, or MSVC)
- CMake 3.20+
- Windows: Ninja build system (recommended) or Visual Studio

## Project Structure

```
Hybrid/
├── src/                # Compiler source code
│   └── CMakeLists.txt  # Source build configuration
├── test/               # Test suite (.hy files)
├── docs/               # Documentation
│   ├── control_flow/   # Control flow (if-else, foreach, switch, etc.)
│   ├── type_system/    # Type system (variables, functions, casting, etc.)
├── CMakeLists.txt      # Main build configuration
├── CMakePresets.json   # Build presets for IDEs
├── build.sh            # Unix/macOS build script
├── build.bat           # Windows build script
├── run_tests.sh        # Unix/macOS test runner
└── run_tests.bat       # Windows test runner
```

## Contributing

Contributions are welcome! To keep reviews fast and regressions low, please follow the workflow below.

### Workflow
- Create a focused branch and keep changes scoped to one feature or fix.
- Build after any source change: `./build.sh` (use `./build.sh -c` for a clean configure) or `build.bat` (`build.bat -c` for clean).
- Run tests: `./run_tests.sh` (add `-v` for verbose output) or `run_tests.bat` (`run_tests.bat -v`).
- Run a single test with `./run_tests.sh test_name` (name without `.hy`) or `run_tests.bat test_name`.
- Add or update documentation in `docs/` when behavior changes.

### Tests and Diagnostics
- Add new tests under `test/` using `.hy` files; prefer minimal programs.
- For expected failures, use files with `fail`/`error` suffixes and include the diagnostic text.
- When adding new diagnostics, route them through `reportCompilerError()` / `LogError*` and ensure tokens have useful names in `describeTokenForDiagnostics()`.

### Pull Request Checklist
- Tests pass locally (`./run_tests.sh` or `run_tests.bat`).
- New behavior is covered by tests in `test/`.
- Docs updated in `docs/` where relevant.

## License

MIT License - see [LICENSE](LICENSE) file for details.
