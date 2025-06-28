# Hybrid Programming Language

Hybrid is a programming language compiler/interpreter that combines elements from multiple language paradigms. The project implements a traditional compiler architecture with lexing, parsing, and AST representation.

## Features

- **C-style function syntax** with typed parameters and return types
- **Curly bracket blocks** for function implementations  
- **Multiple bracket styles** including single-line and Allman style
- **Expression evaluation** with binary operators and precedence
- **External function declarations** for linking with external libraries
- **REPL (Read-Eval-Print Loop)** for interactive development
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

The project uses a Makefile for building:

```bash
# Build the compiler
make

# Clean build artifacts
make clean

# Manual compilation (if needed)
clang++ -std=c++17 -o hybrid src/driver.cpp src/lexer.cpp src/parser.cpp src/ast.cpp src/toplevel.cpp
```

## Usage

### Interactive Mode (REPL)

Run the compiler without arguments to start the interactive REPL:

```bash
./hybrid
ready> int add(int x, int y) { return x + y }
Parsed a function definition.
ready> extern int printf(char msg)
Parsed an extern
ready> 2 + 3 * 4
Parsed a top-level expr
ready>
```

### File Input

You can pipe source code files to the compiler:

```bash
cat program.hy | ./hybrid

# Test with provided examples
cat test/test.txt | ./hybrid
cat test/test_allman.txt | ./hybrid
```

## Testing

The `test/` directory contains various test files demonstrating different language features:

- `test.txt` - Complete examples of all syntax forms
- `test_brackets.txt` - Various bracket placement styles
- `test_typed_params.txt` - C-style typed parameter examples
- `test_examples.txt` - Basic function definition examples

Run tests with:
```bash
# Test all features
cat test/test.txt | ./hybrid

# Test specific syntax styles
cat test/test_brackets.txt | ./hybrid
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

This is a frontend-only implementation focusing on lexical analysis, parsing, and AST construction. The compiler successfully parses:

- C-style function syntax with typed parameters
- Variable declarations with mandatory initialization
- Foreach loops with typed iteration variables
- Expression statements and return statements

Not yet implemented:
- Code generation (LLVM backend)
- Type checking and validation
- Variable assignments (only declarations)
- Traditional control flow statements (if/else, while, for)
- Advanced type system features

## Development

The codebase is structured for easy extension:

- Add new expression types by extending `ExprAST`
- Add new statement types by extending `StmtAST`  
- Extend the lexer for new keywords in `lexer.cpp`
- Add parsing rules in the recursive descent parser
- The REPL provides immediate feedback for testing new features

## License

See LICENSE file for details.