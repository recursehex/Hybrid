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
- Function keywords: `return`, `extern`, `ref`
- Structure keywords: `use`, `struct`, `this`
- Boolean literals: `true`, `false`
- Null literal: `null`
- Exception handling: `assert`
- Memory safety: `unsafe`

### Null Safety

- Types are non-nullable unless annotated with `?` (for example `int?`, `User?`, `float?[]`, `string[]?`)
- Use `?.` for null-safe member access on nullable struct references
- Use `?[` for null-safe element access on nullable array references
- Use `??` to supply a fallback when a nullable value is `null`
- Use `??=` to assign a fallback when a nullable variable is `null`

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

### String Interpolation

Hybrid supports interpolation for building strings with embedded expressions. Use the `$"..."` prefix and wrap expressions in backticks:

```cs
string name = "World"
int count = 3
float pi = 3.14159

string greeting = $"Hello, `name`!"               // "Hello, World!"
string summary = $"Name: `name`, Count: `count`"  // multiple substitutions
string math = $"Next: `count + 1`"                // arbitrary expressions
string precise = $"Pi: `pi:2`"                    // format specifier for floats
```

- Expressions inside backticks can be any valid expression.
- Format specifiers are optional and currently supported for floating-point values using `:digits` to control precision (e.g., `pi:3`).
- To emit a literal backtick inside an interpolated string, escape it with `\``.

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
string? alias = null   // Nullable reference
int@ handle = null     // Pointers are implicitly nullable
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

## Nullability

- Types are non-nullable unless explicitly annotated with `?` (for example, `int?`, `User?`, `string[]?`).
- The null-safe member access operator `?.` must be used when dereferencing nullable struct values: `user?.address?.city`.
- Assigning `null` or any nullable expression to a non-nullable variable, field, or array element results in a compile-time error.
- Flow analysis does not currently narrow nullable types. Use helper functions or explicit conversions after performing manual null checks.

See the [Type System](type-system.md#nullable-types), [Structs](structs.md#nullable-fields), and [Expressions](expressions.md#null-safe-member-access) guides for details.

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
- [Reference Types](references.md)
- [Pointers and Memory Safety](pointers-and-memory-safety.md)
- [Structs](structs.md)
