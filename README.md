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

## Current Status

**Implemented**
- Complete lexer and parser
- AST construction
- LLVM code generation
- Function definitions and calls
- All primitive types and arrays
- Mulitidimensional and jagged arrays
- Multiline literals (arrays, function arguments, constructor calls)
- Rich numeric literal formats (`0b`, `0o`, `0x`, scientific notation)
- If-else statements
- While loops
- For loops with advanced features:
  - Basic `for int i = 1 to 10` syntax with automatic increment/decrement
  - Anonymous loops `for 0 to 10` without variable declarations
  - Custom steps with `by` keyword: `by 2`, `by -3`, `by * 2`, `by / 2`
  - Exclusive bounds: `for int i = 0 to i < size`
  - Full float/double support: `for float f = 0.0 to 1.0 by 0.1`
  - Complex conditions and nested loops
- Foreach loops
- Expression evaluation
- Global and local variables
- External function declarations
- Bitwise operators and compound assignments (`&`, `|`, `^`, `<<`, `>>`, `&=`, `|=`, `^=`, `<<=`, `>>=`)
- Increment/decrement operators (`++`, `--`) - both prefix and postfix
- Multiple sizes for integers and characters
    - `byte` - 8 bit unsigned integer
    - `short` - 16 bit signed integer
    - `long` - 64 bit signed integer
    - `schar` - 8 bit character
    - `lchar` - 32 bit Unicode character
- Unsigned versions of integer types
    - `sbyte` - 8 bit signed (`byte` is unsigned)
    - `ushort` - 16 bit unsigned
    - `uint` - 32 bit unsigned
    - `ulong` - 64 bit unsigned
- Structures
    - `struct` - supports fields and constructors
    - `class` - supports methods and inheritance
- Inheritance
    - `inherits`
    - `abstract`
    - `interface`
    - `base`
    - `virtual`
    - `override`
- Access modifiers
    - `public`
    - `private`
    - `protected`
    - `static`
    - `const`
- Switch statements and expressions
    - Block-style switch statements: `switch num { case 1 { ... } default { ... } }`
    - Arrow-syntax switch expressions: `switch letter { 'a' => "Alpha" default => "Unknown" }`
    - Multiple case values: `case 1, 2 => 10`
    - Full LLVM optimization with native switch instructions
- Ternary operator with Python-style syntax
    - Conditional expressions: `value_if_true if condition else value_if_false`
    - Examples: `int max = a if a > b else b`, `return n if n >= 0 else -n`
    - Automatic type promotion: `double mixed = 3.14 if false else 42`
    - Right-associative with proper precedence handling
- Pointer operators
    - `@` - pointer operator and dereference operator
    - `#` - address operator
    - `->` - member access through pointer
    - `unsafe` - pointers can only be used within these blocks
- Reference types
    - `ref type`
- Assert statement
    - `assert num == 42`
- String interpolation
    - ```$"Hello, `name`!"```
- Nullable types
    - `type?`
- Nullity operators
    - `?.`
    - `?[]`
    - `??`
    - `??=`
- Generics
    - `class Box<T> { ... }` - generic classes
    - `void Assign<T>(ref T target, T value)` - generic functions
    - Explicit invocations such as `Box<int>` and `Assign<int>(ref target, 7)`

**Planned Features**
- Stuctures
    - `enum`
    - `namespace`
- Heap allocation keywords
    - `new`
    - `free`
- 128 bit base 10 floating point type
    - `decimal`
- Smart pointers
    - `unique`
    - `shared`
    - `weak`
- Tuples
    - `(type1, type2) var`
- Type checking
    - `is`
    - `not`
- Reflection keywords
    - `typeof`
    - `nameof`
    - `sizeof`
- Exception statements
    - `try`
    - `catch`
    - `throw`
    - `finally`
- Properties
    - `type var {}`
    - `get`
    - `set`
    - `value`
- Delegates
    - `delegate` - function pointer type


## Contributing

Contributions are welcome! Please ensure all tests pass and add tests for new features.

## License

MIT License - see [LICENSE](LICENSE) file for details.
