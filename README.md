<h1 align="center">
 <img src="docs/img/hybrid_logo.svg" alt="Hybrid logo"/>
</h1>

# Hybrid Programming Language

Hybrid is a performant, statically-typed programming language that combines the power of C/C++ with the flexibility of Python. It has a modern, expressive syntax, interoperating with C/C++ while being memory-safe.

## Features

- **Modern syntax** with explicit type declarations
- **LLVM backend** for optimized native code generation
- **Static typing** with automatic type inference for literals
- **Control flow** including if-else, while, for, and foreach
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

# Install CMake
brew install cmake       # macOS
apt install cmake        # Ubuntu/Debian
```

### Building

```bash
# Configure and build
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build

# Or use the build script
./build.sh              # Release build
./build.sh -d           # Debug build
./build.sh -c -t        # Clean build and run tests

# Or use CMake presets
cmake --preset=release
cmake --build --preset=release
```

### Running

```bash
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
- [Control Flow](docs/control-flow.md) - Statements and loops
- [Arrays](docs/arrays.md) - Array types and operations
- [Expressions](docs/expressions.md) - Operators and expressions
- [Examples](docs/examples.md) - Complete example programs
- [Architecture](docs/architecture.md) - Compiler design and internals
- [LLVM Code Generation](docs/llvm-codegen.md) - How code is generated
- [Testing](docs/testing.md) - Test suite and writing tests

## Language Overview

### Types
- Basic primitives
    - `int` 32-bit integer
    - `float` 32-bit floating-point number
    - `double` 64-bit floating-point number
    - `char` 16-bit Unicode character
    - `bool` 8-bit boolean value
    - `string` pointer to `char` array
    - `void` for functions returning nothing
- Sized integers
    - Unsigned
        - `byte` 8-bit unsigned integer
        - `ushort` 16-bit unsigned integer
        - `uint` 32-bit unsigned integer
        - `ulong` 64-bit unsigned integer
    - Signed
        - `sbyte` 8-bit integer
        - `short` 16-bit integer
        - `long` 64-bit integer
- Sized characters
    - `schar` 8-bit character
    - `lchar` 32-bit Unicode character
- Arrays
    - e.g. `int[]`, `float[]`, `byte[]`

> [!NOTE]
> All variables must be initialized at declaration. Variables of differently sized types must be explicitly converted

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
if i == 10 { } else { }

// While loop with break/skip
while i < 10 {
    if j > 5 { break }     // Exit loop
    j++
    if i + j == 20 { skip } // Continue to next iteration
}

// For loops
for int i = 1 to 10 { }             // Basic range, increments by 1
for int i = 0 to 20 by 2 { }        // Custom additive step
for int i = 1 to 100 by * 2 { }     // Multiplicative step
for int i = 0 to i < size { }       // Custom condition
for float f = 0.0 to 1.0 by 0.1 { } // Float support
for 0 to 10 { }                     // Anonymous counter

// Foreach loop
for int i in nums { }

// Switch statements (block-style)
switch num {
    case 1 { int result = 10 }
    case 2 { int result = 20 }
    default { int result = 0 }
}

// Switch expressions (arrow-style)  
string message = switch letter {
    'a' => "Alpha"
    'b' => "Beta"
    default => "Unknown"
}
```

### Operators
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Boolean: `&&`, `||`, `!`
- Assignment: `=`, `+=`, `-=`, `*=`, `/=`, `%=`
- Increment/Decrement: `++`, `--` (both prefix and postfix)
- Type Casting: `:` (e.g., `int: floatVar`)

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

## Project Structure

```
Hybrid/
├── src/                # Compiler source code
│   └── CMakeLists.txt  # Source build configuration
├── test/               # Test suite (.hy files)
├── docs/               # Documentation
├── CMakeLists.txt      # Main build configuration
├── CMakePresets.json   # Build presets for IDEs
├── build.sh            # Build script
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
- Structs with multi-level member access and constructors
- Switch statements and expressions
    - Block-style switch statements: `switch num { case 1 { ... } default { ... } }`
    - Arrow-syntax switch expressions: `switch letter { 'a' => "Alpha" default => "Unknown" }`
    - Multiple case values: `case 1, 2 => 10`
    - Full LLVM optimization with native switch instructions

**Planned Features**
- Automatic Reference Counting (ARC) memory management
- Standard library integration
- Module system and imports
- Mulitidimensional arrays
- Binary literals e.g. `0b1010`
- Hexadecimal literals e.g. `0xABCD`
- Floating point literals with scientific notation e.g. `1.23e4`
- Multiline literals
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