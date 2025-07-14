# Hybrid Programming Language

Hybrid is a statically-typed programming language with C-style syntax that compiles to native code via LLVM. It combines familiar syntax with modern features and will feature automatic reference counting (ARC) for memory management.

## Features

- **C-style syntax** with explicit type declarations
- **LLVM backend** for optimized native code generation
- **Static typing** with automatic type inference for literals
- **Modern control flow** including if-else, while loops with break/skip, and foreach
- **Arrays** with literals and indexing
- **Interactive REPL** with live code compilation
- **Cross-platform** support (macOS, Linux)

## Quick Start

```bash
# Install LLVM
brew install llvm        # macOS
apt install llvm-dev     # Ubuntu/Debian

# Build the compiler
make

# Run interactive mode
./hybrid

# Run a program
./hybrid < program.hy

# Run tests
./run_tests.sh
```

## Example

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
        i += 1  // Compound assignment
        counter += 1
    }
    return max
}

// Modulo operator example
int checkEven(int n) {
    return n % 2  // Returns 0 if even, 1 if odd
}

// Foreach loop with skip
int sumOddNumbers(int[] nums) {
    int sum = 0
    for int n in nums {
        if checkEven(n) == 0 {
            skip  // Skip even numbers
        }
        sum += n
    }
    return sum
}

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
- Primitives: `int`, `float`, `double`, `char`, `bool`, `string`, `void`
- Arrays: `int[]`, `float[]`, etc.
- All variables must be initialized at declaration

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

## Building from Source

### Prerequisites
- LLVM 20+ with development headers
- C++17 compatible compiler
- Make build tool

### Build Commands
```bash
make          # Build the compiler
make clean    # Clean build artifacts
```

The Makefile automatically detects LLVM installation via `llvm-config`.

## Project Structure

```
Hybrid/
├── src/          # Compiler source code
├── test/         # Test suite (.hy files)
├── docs/         # Documentation
├── Makefile      # Build configuration
└── run_tests.sh  # Test runner script
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
- More sizes for primitive types
    - `byte` - 8 bit integer
    - `short` - 16 bit integer
    - `long` - 64 bit integer
    - `schar` - 8 bit character
    - `lchar` - 32 bit character
- Unsigned versions of integer types
    - `sbyte` - 8 bit (`byte` is unsigned)
    - `ushort` - 16 bit
    - `uint` - 32 bit
    - `ulong` - 64 bit
- 128 bit base 10 floating point type
    - `decimal`
- Type casting
    - `type:`
- Nullable types
    - `type?`
- Reference types
    - `ref type`
- Increment/decrement operators supporting pre/postfix
    - `++`
    - `--`
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

MIT License - see LICENSE file for details.