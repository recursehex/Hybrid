# Hybrid Language Reference

## Overview

Hybrid is a statically-typed programming language that combines elements from multiple language paradigms. It features C-style syntax with mandatory type declarations, automatic memory management via LLVM, and a clean, expressive syntax.

## Basic Syntax

### Comments

```c
// This is a single-line comment
// Comments continue until the end of the line
```

### Statement Termination

Statements are terminated by newlines. Semicolons are optional.

```c
int x = 10
int y = 20;  // Semicolon is optional
```

### Identifiers

Identifiers can contain letters, numbers, and underscores. They must start with a letter or underscore.

```c
int myVariable = 10
int _privateVar = 20
int count_123 = 30
```

## Program Structure

A Hybrid program consists of:
- Variable declarations (global or local)
- Function definitions
- External function declarations
- Top-level expressions (in REPL mode)

### Example Program Structure

```c
// External declarations
extern int printf(char format, int value)

// Global variables
int globalCount = 0
string appName = "MyApp"

// Function definitions
int add(int x, int y)
{
    return x + y
}

// Top-level expressions (REPL only)
add(5, 3)
```

## Keywords

The following keywords are reserved:

- Type keywords: 
  - Basic types: `int`, `float`, `double`, `char`, `void`, `bool`, `string`
  - Sized integers: `byte`, `sbyte`, `short`, `ushort`, `uint`, `long`, `ulong`
  - Sized characters: `schar`, `lchar`
- Control flow: `if`, `else`, `for`, `in`, `to`, `by`, `while`, `break`, `skip`, `switch`, `case`, `default`
- Function keywords: `return`, `extern`
- Structure keywords: `use`, `struct`, `this`
- Boolean literals: `true`, `false`
- Null literal: `null`
- Exception handling: `assert`

## Literals

### Numeric Literals

```c
42        // Integer literal (i32)
3.14      // Float literal (double)
-10       // Negative integer
-2.5      // Negative float
255       // Can be assigned to byte
65535     // Can be assigned to ushort
```

Integer literals are automatically sized to fit the target type with range checking.

### Boolean Literals

```c
true      // Boolean true
false     // Boolean false
```

### String Literals

```c
"Hello, World!"     // String literal
"Line 1\nLine 2"    // String with escape sequences
""                  // Empty string
```

### Character Literals

```c
'a'       // Character literal
'\n'      // Newline character
'\t'      // Tab character
'\\'      // Backslash
'\''      // Single quote
```

### Null Literal

```c
null      // Used for string initialization
```

### Array Literals

```c
[1, 2, 3]              // Integer array
[1.5, 2.5, 3.5]        // Float array
['a', 'b', 'c']        // Character array
[true, false, true]    // Boolean array
```

## Language Safety Features

Hybrid includes several safety features designed to prevent common programming errors:

### Assignment Safety

Assignments cannot be used as conditions in control flow statements. This prevents common bugs where assignment (`=`) is mistakenly used instead of comparison (`==`):

```c
// Compilation error - prevents accidental assignment
if x = 0 { /* ... */ }

// Correct - forces explicit comparison
if x == 0 { /* ... */ }
```

### Mandatory Variable Initialization

All variables must be initialized when declared, preventing use of uninitialized variables:

```c
int x = 10    // Valid - initialization required
int y         // Compilation error - no initialization
```

### Static Type Checking

All types are checked at compile time, preventing type-related runtime errors.

## Supported Constructs

- Variable declarations with mandatory initialization
- Function definitions with typed parameters and return types
- External function declarations
- Expression statements
- Return statements
- Block statements
- If-else conditional statements
- While loops
- Foreach loops
- Array declarations and operations
- Binary and unary expressions

See the individual documentation files for detailed information on each construct:
- [Type System](type-system.md)
- [Functions](functions.md)
- [Control Flow](control-flow.md)
- [Expressions](expressions.md)
- [Arrays](arrays.md)