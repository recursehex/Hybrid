# Compiler Architecture

## Overview

The Hybrid compiler follows a traditional multi-pass architecture, processing source code through distinct phases: lexical analysis, parsing, AST construction, and LLVM code generation. Each phase is modular and communicates through well-defined interfaces.

## Architecture Diagram

```
Source Code (.hy)
      |
      v
  [Lexer] ---------> Tokens
      |
      v
  [Parser] --------> Parse Tree
      |
      v
[AST Builder] -----> Abstract Syntax Tree
      |
      v
[Code Generator] --> LLVM IR
      |
      v
[LLVM Backend] ----> Machine Code
```

## Components

### 1. Lexer (`src/lexer.cpp/h`)

The lexical analyzer converts source text into tokens.

#### Responsibilities
- Character stream processing
- Token recognition and classification
- Whitespace and comment handling
- String and character literal parsing
- Keyword identification

#### Key Classes
- `Token`: Represents a lexical token with type and value
- `Lexer`: Main lexer class with `gettok()` method

#### Token Types
```cpp
enum Token {
    tok_eof = -1,
    tok_def = -2,
    tok_extern = -3,
    tok_identifier = -4,
    tok_number = -5,
    tok_if = -6,
    tok_else = -7,
    tok_for = -8,
    tok_in = -9,
    tok_return = -10,
    // ... more tokens
};
```

### 2. Parser (`src/parser.h`, `src/parser/*.cpp`)

The parser is a recursive descent implementation split by grammar domain.

#### Responsibilities
- Syntax analysis
- Precedence-based expression parsing
- Error reporting and recovery
- AST node construction

#### Implementation Layout
- `src/parser/parser_core.cpp`: token replay, precedence lookup, newline skipping, parser error helpers
- `src/parser/parser_types.cpp`: type parsing, generic parameter/argument parsing, type metadata construction
- `src/parser/parser_expr_primary.cpp`: literals and primary expressions
- `src/parser/parser_expr_postfix.cpp`: calls, indexing, member/null-safe postfix parsing
- `src/parser/parser_expr_binop.cpp`: unary/binary expression flow, condition expressions, recovery helpers
- `src/parser/parser_decls.cpp`: prototypes, function/delegate declarations and definitions
- `src/parser/parser_statements.cpp`: statements and block/control-flow parsing
- `src/parser/parser_aggregates.cpp`: struct/class/interface parsing and member modifier/access rules
- `src/parser/parser_consteval.cpp`: compile-time constant-expression evaluation
- `src/parser/parser_internal.h`: shared parser-internal helpers, scopes, and context aliases

#### Key Functions
```cpp
// Primary parsing functions
std::unique_ptr<ExprAST> ParseExpression();
std::unique_ptr<ExprAST> ParsePrimary();
std::unique_ptr<ExprAST> ParseBinOpRHS(int Prec, std::unique_ptr<ExprAST> LHS);

// Statement parsing
std::unique_ptr<StmtAST> ParseStatement();
std::unique_ptr<BlockStmtAST> ParseBlock();
std::unique_ptr<ReturnStmtAST> ParseReturn();
```

#### Operator Precedence
```cpp
std::map<char, int> BinopPrecedence = {
    {'=', 1},   // Assignment
    {'||', 5},  // Logical OR
    {'&&', 6},  // Logical AND
    {'<', 10},  // Comparisons
    {'+', 20},  // Addition/Subtraction
    {'*', 40}   // Multiplication/Division
};
```

### 3. AST (`src/ast.h`, `src/ast/*.cpp`)

The Abstract Syntax Tree represents the program structure.

#### Node Hierarchy

```
AST Node
├── ExprAST (Expressions)
│   ├── NumberExprAST
│   ├── BoolExprAST
│   ├── StringExprAST
│   ├── CharExprAST
│   ├── NullExprAST
│   ├── VariableExprAST
│   ├── BinaryExprAST
│   ├── UnaryExprAST
│   ├── CallExprAST
│   ├── ArrayExprAST
│   └── ArrayIndexExprAST
│
├── StmtAST (Statements)
│   ├── ReturnStmtAST
│   ├── BlockStmtAST
│   ├── ExpressionStmtAST
│   ├── VariableDeclarationStmtAST
│   ├── ForEachStmtAST
│   ├── IfStmtAST
│   └── WhileStmtAST
│
├── PrototypeAST (Function Signatures)
└── FunctionAST (Function Definitions)
```

#### Implementation Layout
AST implementation is split into focused modules under `src/ast/`:

- `src/ast/ast_context.cpp`: core shared context, common utilities, top-level setup
- `src/ast/ast_runtime.cpp`: runtime type descriptors and runtime call plumbing
- `src/ast/ast_arc.cpp`: ARC/lifetime helpers and ARC scope management
- `src/ast/ast_overloads.cpp`: overload/default-argument/generic instantiation helpers
- `src/ast/ast_types.cpp`: type parsing/resolution and shared type helpers
- `src/ast/expr_literals.cpp`: literal/tuple/array literal expression codegen
- `src/ast/expr_access.cpp`: access/path expressions (`[]`, variable/member lvalues)
- `src/ast/expr_ops.cpp`: cast/unary/binary/ref/operator expression codegen
- `src/ast/expr_calls.cpp`: function/method/delegate call codegen
- `src/ast/stmts.cpp`: statement codegen
- `src/ast/functions.cpp`: prototype/function/delegate declarations/definitions
- `src/ast/aggregates.cpp`: struct/class codegen and related aggregate emission
- `src/ast/expr_ternary_switch.cpp`: switch-expression and ternary expression codegen

#### Key Methods
Each AST node implements:
- `codegen()`: Generate LLVM IR for the node
- Virtual destructor for proper cleanup

### 4. Code Generator (`src/codegen.cpp/h`)

Transforms AST nodes into LLVM IR.

#### Responsibilities
- LLVM context and module management
- Type mapping and conversion
- Memory allocation (alloca instructions)
- Control flow generation (basic blocks)
- Function and global variable management

#### Key Components
```cpp
// Global LLVM objects
static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;

// Symbol tables
static std::map<std::string, AllocaInst*> NamedValues;
static std::map<std::string, GlobalVariable*> GlobalValues;
```

### 5. Top-level Parser (`src/toplevel.cpp/h`)

Manages the REPL and top-level constructs.

#### Responsibilities
- REPL loop implementation
- Top-level expression handling
- Function definition processing
- External declaration handling

#### Main Functions
```cpp
void HandleDefinition();      // Process function definitions
void HandleExtern();          // Process external declarations
void HandleTopLevelExpression(); // Process expressions
```

### 6. Driver (`src/driver.cpp`)

The main entry point and initialization.

#### Responsibilities
- Command-line processing
- LLVM initialization
- Operator precedence setup
- REPL startup

## Data Flow

### 1. Lexical Analysis
```
"int x = 42" -> [tok_int, tok_identifier("x"), '=', tok_number(42)]
```

### 2. Parsing
```
Tokens -> ParseVariableDeclaration() -> VariableDeclarationStmtAST
```

### 3. AST Construction
```
VariableDeclarationStmtAST {
    type: "int",
    name: "x",
    init: NumberExprAST(42)
}
```

### 4. Code Generation
```llvm
%x = alloca i32, align 4
store i32 42, ptr %x, align 4
```

## Memory Management

### AST Ownership
- Uses `std::unique_ptr` for automatic memory management
- Parent nodes own their children
- No circular references in the AST

### LLVM Objects
- LLVM manages its own memory for IR constructs
- Module owns all functions and globals
- Context owns all types

## Error Handling

### Lexer Errors
- Invalid characters
- Unterminated strings
- Invalid escape sequences

### Parser Errors
- Syntax errors with location information
- Recovery at statement boundaries
- Clear error messages with context

### Code Generation Errors
- Type mismatches
- Undefined variables/functions
- Invalid operations

## Extension Points

### Adding New Operators
1. Add token in lexer
2. Add precedence in parser
3. Handle in `ParseBinOpRHS()`
4. Implement codegen in `BinaryExprAST`

### Adding New Types
1. Add type keyword in lexer
2. Update type parsing
3. Add LLVM type mapping
4. Update type checking and casting

### Adding New Statements
1. Create new AST node class
2. Add parsing function
3. Implement code generation
4. Update statement parser

## Build System

### CMake Configuration
The project uses CMake for building with automatic LLVM detection:

```cmake
# Find LLVM package
find_package(LLVM REQUIRED CONFIG)

# Configure build types
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_BUILD_TYPE Release)

# Link LLVM libraries
llvm_map_components_to_libnames(llvm_libs core support)
target_link_libraries(hybrid PRIVATE ${llvm_libs})
```

### Build Presets
CMakePresets.json provides pre-configured builds:
- **debug**: Debug build with symbols
- **release**: Optimized release build
- **relwithdebinfo**: Release with debug info

### Source Organization
```
Hybrid/
├── src/                # Source files
│   ├── ast.h
│   ├── ast/            # Split AST implementation units
│   │   ├── ast_context.cpp
│   │   ├── ast_runtime.cpp
│   │   ├── ast_arc.cpp
│   │   ├── ast_overloads.cpp
│   │   ├── ast_types.cpp
│   │   ├── expr_literals.cpp
│   │   ├── expr_access.cpp
│   │   ├── expr_ops.cpp
│   │   ├── expr_calls.cpp
│   │   ├── stmts.cpp
│   │   ├── functions.cpp
│   │   ├── aggregates.cpp
│   │   └── expr_ternary_switch.cpp
│   ├── lexer.cpp/h
│   ├── parser.h
│   ├── parser/
│   │   ├── parser_internal.h
│   │   ├── parser_core.cpp
│   │   ├── parser_types.cpp
│   │   ├── parser_expr_primary.cpp
│   │   ├── parser_expr_postfix.cpp
│   │   ├── parser_expr_binop.cpp
│   │   ├── parser_decls.cpp
│   │   ├── parser_statements.cpp
│   │   ├── parser_aggregates.cpp
│   │   └── parser_consteval.cpp
│   ├── toplevel.cpp/h
│   ├── driver.cpp
│   └── CMakeLists.txt
├── test/               # Test files
├── docs/               # Documentation
├── CMakeLists.txt      # Main build config
├── CMakePresets.json   # Build presets
└── build.sh            # Build script
```

## Performance Considerations

### Parsing Performance
- Single-pass parsing where possible
- Minimal backtracking
- Efficient token lookahead

### Code Generation
- Direct IR generation without intermediate representation
- Minimal temporary allocations
- LLVM handles optimization passes

### Memory Usage
- AST nodes allocated on demand
- Temporary values cleaned up after use
- Symbol tables use efficient map structures
