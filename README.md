# Hybrid Programming Language

Hybrid is a modern, statically-typed programming language that interops with C++ while being memory-safe. It combines the familiar syntax of C/C++ with the high-level features and feel of Python. It will feature automatic reference counting (ARC) and support AOT and JIT compilation.

## Features

- **C-style syntax** with explicit type declarations
- **LLVM backend** for optimized native code generation
- **Static typing** with automatic type inference for literals
- **Control flow** including if-else, while loops, and foreach
- **Arrays** with literals and indexing
- **Structs** with constructors and member access
- **Interactive REPL** with live code compilation
- **Cross-platform** support (macOS, Linux)

## Quick Start

### Prerequisites

```bash
# Install LLVM
brew install llvm        # macOS
apt install llvm-dev     # Ubuntu/Debian

# Install CMake (if not already installed)
brew install cmake       # macOS
apt install cmake        # Ubuntu/Debian
```

### Building

```bash
# Configure and build
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build

# Or use the convenient build script
./build.sh              # Release build
./build.sh -d           # Debug build
./build.sh -c -t        # Clean build and run tests

# Or use CMake presets
cmake --preset=release
cmake --build --preset=release

# Run interactive mode
./build/hybrid          # or ./build/release/hybrid with presets

# Run a program
./build/hybrid < program.hy

# Run tests
./run_tests.sh
```

## Examples

```c
// Function definition
int fibonacci(int n) {
    if n <= 1 { return n }
    return fibonacci(n - 1) + fibonacci(n - 2)
}

// Variable declaration
int[] numbers = [1, 2, 3, 4, 5]
int counter = 0

// Control flow with new operators
int findMax(int[] arr, int len) {
    int max = arr[0]
    int i = 1
    while i < len {
        if arr[i] > max {
            max = arr[i]
        }
        i++       // Postfix increment  
        counter++ // Could also use: ++counter
    }
    return max
}

// Modulo operator example
int checkEven(int n) {
    return n % 2
}

// Foreach loop with skip
int sumOddNumbers(int[] nums) {
    int sum = 0
    for int n in nums {
        if checkEven(n) == 0 {
            skip
        }
        sum += n
    }
    return sum
}

// Struct definition
struct Point {
    int x
    int y
    
    Point(int x, int y) {
        this.x = x
        this.y = y
    }
}

// Using structs
Point p = Point(10, 20)
int distance = p.x + p.y  // 30

// Call the functions
findMax(numbers, 5)    // Returns 5
checkEven(17)          // Returns 1
sumOddNumbers(numbers) // Returns 9 (1+3+5)
```

## Documentation

Comprehensive documentation is available in the `docs/` directory:

- [Language Reference](docs/language-reference.md) - Syntax and language basics
- [Type System](docs/type-system.md) - Types and type safety
- [Functions](docs/functions.md) - Function definitions and calls
- [Control Flow](docs/control-flow.md) - If-else, while, and foreach loops
- [Arrays](docs/arrays.md) - Array types and operations
- [Expressions](docs/expressions.md) - Operators and expressions
- [Examples](docs/examples.md) - Complete example programs
- [Architecture](docs/architecture.md) - Compiler design and internals
- [LLVM Code Generation](docs/llvm-codegen.md) - How code is generated
- [Testing](docs/testing.md) - Test suite and writing tests

## Language Overview

### Types
- Basic primitives: `int` (32-bit), `float` (32-bit), `double` (64-bit), `char` (16-bit), `bool` (8-bit), `string` (pointer), `void`
- Sized integers: 
  - Unsigned: `byte` (8-bit), `ushort` (16-bit), `uint` (32-bit), `ulong` (64-bit)
  - Signed: `sbyte` (8-bit), `short` (16-bit), `long` (64-bit)
- Character types: `char` (16-bit Unicode), `schar` (8-bit), `lchar` (32-bit Unicode)
- Arrays: `int[]`, `float[]`, `byte[]`, etc.
- All variables must be initialized at declaration
- Strict type checking: no implicit conversions between different sized types

### Functions
```c
return_type function_name(type1 param1, type2 param2) {
    // function body
    return expression
}
```

### Control Flow
```c
// If-else
if condition { } else { }

// While loop with break/skip
while condition {
    if done { break }     // Exit loop
    if skip_this { skip } // Continue to next iteration
}

// Foreach loop
for type var in collection { }
```

### Operators
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Boolean: `&&`, `||`, `!`
- Assignment: `=`, `+=`, `-=`, `*=`, `/=`, `%=`
- Increment/Decrement: `++`, `--` (both prefix and postfix)
- Type Casting: `:` (e.g., `int: floatVar`, `byte: 100`)

## Build System

The project uses CMake for cross-platform builds.

### Prerequisites
- LLVM 20+ with development headers
- C++17 compatible compiler
- CMake 3.20+

### CMake Features
- **Cross-platform support**: Works on macOS, Linux, and Windows
- **Multiple build configurations**: Debug, Release, RelWithDebInfo
- **Automatic LLVM detection**: Finds LLVM installation automatically
- **IDE integration**: Generates compile_commands.json for clangd/VSCode
- **CMake Presets**: Pre-configured builds for common scenarios
- **Parallel builds**: Efficient multi-core compilation
- **Installation support**: Standard CMake install targets
- **Package generation**: CPack support for distribution

### Build Commands

```bash
# Standard CMake
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
cmake --build build --target run-tests

# Using CMake Presets
cmake --preset=release
cmake --build --preset=release

# Using convenience script
./build.sh              # Release build
./build.sh -d           # Debug build
./build.sh -c -t        # Clean build and run tests
```

## Project Structure

```
Hybrid/
├── src/                # Compiler source code
│   └── CMakeLists.txt  # Source build configuration
├── test/               # Test suite (.hy files)
├── docs/               # Documentation
├── CMakeLists.txt      # Main build configuration
├── CMakePresets.json   # Build presets for IDEs
├── build.sh            # Convenience build script
└── run_tests.sh        # Test runner script
```

## Current Status

**Implemented**
- Complete lexer and parser
- AST construction
- LLVM code generation
- Function definitions and calls
- All primitive types and arrays
- If-else statements
- While loops
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

**Planned Features**
- Automatic Reference Counting (ARC) memory management
- Standard library integration
- Module system and imports
- Stuctures
    - `class`
    - `struct`
    - `enum`
    - `namespace`
- OOP
    - `this`
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
- Heap allocation keywords
    - `new`
    - `free`
- 128 bit base 10 floating point type
    - `decimal`
- Nullable types
    - `type?`
- Reference types
    - `ref type`
- Nullity operators
    - `?.`
    - `?[]`
    - `??`
    - `??=`
- Pointer operators
    - `@` - pointer operator and dereference operator
    - `#` - address operator
    - `unsafe` - pointers can only be used within these blocks
- Smart pointers
    - `unique`
    - `shared`
    - `weak`
- Tuples
    - `(type1, type2) var`
- String interpolation
    - $"Hello, \`name\`!"
- Switch case statements
    - `switch`
    - `case`
    - `default`
- Generics
    - `<type>`
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
    - `assert`
- Properties
    - `type var {}`
    - `get`
    - `set`
    - `value`


## Contributing

Contributions are welcome! Please ensure all tests pass and add tests for new features.

## License

MIT License - see [LICENSE](LICENSE) file for details.