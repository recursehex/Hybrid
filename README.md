# Hybrid Programming Language

Hybrid is a programming language compiler/interpreter that combines elements from multiple language paradigms. The project implements a traditional compiler architecture with lexing, parsing, and AST representation.

> **📝 Branch Note**: This README describes the `codegen` branch which includes complete LLVM code generation. The `main` branch contains only the frontend parser. To use the full compiler with code generation, switch to the `codegen` branch: `git checkout codegen`

## Features

- **C-style function syntax** with typed parameters and return types
- **LLVM code generation** - generates optimized machine code via LLVM IR
- **Automatic type system** with smart type inference and casting
- **Curly bracket blocks** for function implementations  
- **Multiple bracket styles** including single-line and Allman style
- **Expression evaluation** with binary operators and precedence
- **External function declarations** for linking with external libraries
- **REPL (Read-Eval-Print Loop)** with live IR generation for interactive development
- **Foreach loops** with syntax `for type var in collection { ... }`
- **Variable declarations** with C-style syntax and mandatory initialization

## Language Syntax

### Function Definitions

Functions use C-style syntax with explicit types:

```c
// Single-line function
int add(int x, int y) { return x + y }

// Multi-line compact style
int multiply(int a, int b) {
    return a * b
}

// Allman style  
int square(int x)
{
    return x * x
}

// Function with no parameters
int getAnswer()
{
    return 42
}
```

### External Declarations

External functions can be declared for linking:

```c
extern int printf(char format, int value)
extern int getchar()
```

### Variable Declarations

Variables must be declared with C-style syntax and initialized:

```c
int x = 10
float pi = 3.14
double value = 0.0
char ch = 'A'
bool flag = true
bool active = false
```

### Foreach Loops

Iterate over collections with typed loop variables:

```c
// Simple foreach
for int num in nums {
    num * 2
}

// Nested foreach loops
for int i in list1 {
    for int j in list2 {
        i + j
    }
}
```

### Expressions

The language supports binary expressions with operator precedence:

```c
2 + 3 * 4    // Evaluates to 14
x < y + 1    // Comparison with arithmetic
```

### Supported Operators

- Arithmetic: `+`, `-`, `*`
- Comparison: `<`, `>`
- Assignment: `=`
- Function calls: `functionName(args)`

## Building

The project uses a Makefile with automatic LLVM integration:

```bash
# Build the compiler (automatically links with LLVM)
make

# Clean build artifacts
make clean

# The Makefile automatically detects LLVM installation:
# - Checks for llvm-config in PATH first
# - Falls back to Homebrew location on macOS (/opt/homebrew/opt/llvm/bin/llvm-config)
# - Links with LLVM core libraries for code generation
```

### Prerequisites

- **LLVM 20+** installed on your system
- **clang++** with C++17 support
- On macOS: `brew install llvm`
- On Linux: Install llvm-dev package for your distribution

## Usage

### Interactive Mode (REPL)

Run the compiler without arguments to start the interactive REPL with live LLVM IR generation:

```bash
./hybrid
ready> int add(int x, int y) { return x + y }
Parsed function successfully, generating code...
Generated function IR:
define i32 @add(i32 %a, i32 %b) {
entry:
  %a1 = alloca i32, align 4
  store i32 %a, ptr %a1, align 4
  %b2 = alloca i32, align 4
  store i32 %b, ptr %b2, align 4
  %a3 = load i32, ptr %a1, align 4
  %b4 = load i32, ptr %b2, align 4
  %addtmp = add i32 %a3, %b4
  ret i32 %addtmp
}

ready> 2 + 3 * 5
Generated top-level expression IR:
define void @__anon_expr() {
entry:
  ret i32 17
}

ready>
```

### File Input

You can pipe source code files to the compiler to see generated LLVM IR:

```bash
# Test with code generation examples
./hybrid < test/codegen_test.txt
./hybrid < test/single_function.txt

# Test arithmetic and expressions
./hybrid < test/expr_test.txt
./hybrid < test/bool_test.txt

# Legacy parsing tests (older format)
./hybrid < test/test_clean_comprehensive.txt
```

## Testing

The `test/` directory contains various test files demonstrating different language features:

### Code Generation Tests
- `codegen_test.txt` - Complete functions with LLVM IR generation
- `single_function.txt` - Simple function for testing codegen
- `expr_test.txt` - Arithmetic expressions and operations
- `bool_test.txt` - Boolean literals and expressions

### Legacy Parser Tests
- `test_clean_comprehensive.txt` - Comprehensive syntax examples
- `test_brackets.txt` - Various bracket placement styles  
- `test_typed_params.txt` - C-style typed parameter examples

Run tests with:
```bash
# Test code generation
./hybrid < test/codegen_test.txt

# Test arithmetic expressions  
./hybrid < test/expr_test.txt

# Test function definitions
./hybrid < test/single_function.txt

# Test boolean expressions
./hybrid < test/bool_test.txt
```

## Architecture

The compiler follows a traditional multi-pass design:

### 1. Lexer (`src/lexer.cpp/h`)
- Tokenizes input into tokens (identifiers, numbers, keywords, operators)
- Handles comments starting with `//` (C-style line comments)
- Recognizes keywords: `extern`, `return`, `for`, `in`, type keywords, `true`, `false`
- Uses newlines as statement terminators (semicolons optional)

### 2. Parser (`src/parser.cpp/h`) 
- Implements recursive descent parsing with operator precedence
- Parses C-style function declarations with typed parameters
- Handles curly bracket blocks and return statements
- Supports both single-line and multi-line function definitions
- Parses foreach loops with typed iteration variables
- Handles variable declarations with mandatory initialization

### 3. AST (`src/ast.cpp/h`)
- Defines Abstract Syntax Tree nodes:
  - `ExprAST`: Base expression class
  - `NumberExprAST`: Numeric literals
  - `BoolExprAST`: Boolean literals (true/false)
  - `VariableExprAST`: Variable references
  - `BinaryExprAST`: Binary operations
  - `CallExprAST`: Function calls
  - `PrototypeAST`: Function prototypes with return and parameter types
  - `FunctionAST`: Function definitions with statement blocks
  - `StmtAST`: Base statement class
  - `ReturnStmtAST`: Return statements
  - `BlockStmtAST`: Statement blocks
  - `VariableDeclarationStmtAST`: Variable declarations
  - `ExpressionStmtAST`: Expression statements
  - `ForEachStmtAST`: Foreach loops

### 4. Top-level Parser (`src/toplevel.cpp/h`)
- Handles the REPL loop
- Dispatches to appropriate handlers for function definitions, external declarations, and expressions
- Automatically detects function definitions vs expressions

### 5. Driver (`src/driver.cpp`)
- Main entry point that initializes operator precedence and starts the REPL

## Language Features

### Type System
- Explicit typing for function return values and parameters
- Built-in types: `int`, `float`, `double`, `char`, `void`, `bool`
- Parameters require both type and name: `int add(int x, int y)`
- All variables must be initialized at declaration (no default values)
- Boolean type with `true` and `false` literals

### Function Declaration Styles
- **Single-line**: `int func(int x) { return x }`
- **Compact multi-line**: Functions with opening brace on same line
- **Allman style**: Opening brace on separate line (traditional C style)

### Statement System
- Block statements enclosed in curly braces `{}`
- Return statements with `return expression`
- Newline handling for multi-line code

### Comments
- Line comments start with `//` and continue to end of line

### Control Flow
- Foreach loops: `for type var in collection { body }`
- Supports nested loops and can be used within functions

## Example Programs

```c
// Simple arithmetic function
int calculate(int a, int b) { return a * b + 10 }

// Variable declarations (all must be initialized)
int count = 0
float rate = 0.05
bool isActive = true

// External library function
extern int puts(char message)

// Multi-line function with Allman style
int fibonacci(int n)
{
    return n + 1  // Simplified for demo
}

// Foreach loop example
for int num in numbers {
    num * num
}

// Expression evaluation
2 + 3 * (4 + 5)
```

## Current Status

This is a **complete compiler implementation** with both frontend and backend. The compiler successfully handles:

### ✅ Fully Implemented
- **LLVM code generation** - Complete IR generation for all language constructs
- **Type system** - Automatic type inference, promotion, and casting
- **Function compilation** - Full function definitions with typed parameters
- **Expression evaluation** - Arithmetic, comparisons, and function calls
- **Variable management** - Declarations with proper memory allocation
- **Cross-platform build** - Automatic LLVM detection and linking

### ✅ Code Generation Features
- **Smart type handling** - `i32` for integers, `double` for floats, `i1` for booleans
- **Automatic casting** - Seamless conversion between compatible types
- **Memory management** - Proper alloca/load/store for variables
- **Function calls** - Type-safe parameter passing and return values
- **Live IR display** - Interactive REPL shows generated LLVM IR

### 🚧 Not Yet Implemented
- Variable assignments (only declarations supported)
- Traditional control flow statements (if/else, while, for)
- Advanced type system features (structs, arrays, pointers)
- Module system and imports
- Standard library integration

## Development

The codebase is structured for easy extension:

- Add new expression types by extending `ExprAST`
- Add new statement types by extending `StmtAST`  
- Extend the lexer for new keywords in `lexer.cpp`
- Add parsing rules in the recursive descent parser
- The REPL provides immediate feedback for testing new features

## License

See LICENSE file for details.