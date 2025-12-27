# Hybrid Language Reference

## Overview

Hybrid is a statically-typed programming language that combines elements from multiple language paradigms. It features C-style syntax with mandatory type declarations, automatic memory management via LLVM, and a clean, expressive syntax.

## Program Entry Point

Hybrid programs run through a `main` function defined in the root compilation unit. Two signatures are supported:

- `int main()` - returns an explicit status code. The compiler enforces that all paths set the return value. Returning `0` signals success; any non-zero value propagates to the host process, allowing tests and scripts to detect failures.
- `void main()` - indicates the program terminates successfully. If execution reaches the end of the function without an explicit `return`, the compiler inserts a `return 0` on your behalf.

Both forms run global initializers and top-level expressions (when present) before executing user code. Pick the signature that matches your intent: use `void main()` for scripts that never fail, and `int main()` when downstream tooling should observe failure exit codes.

> [!IMPORTANT]
> CI and tooling rely on the process exit code. Use `int main()` whenever you need failing runs to surface as non-zero exits; `void main()` always returns `0`, which can mask missing error checks.

## Basic Syntax

### Comments

```c
// This is a single-line comment
// Comments continue until the end of the line

/* Block comments can span multiple lines
   and are ignored by the compiler */
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
int _myOtherVar = 20
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
- Control flow:
  - If-else: `if`, `else`
  - For and foreach loops: `for`, `in`, `to`, `by`
  - While loops: `while`
  - Loop control: `break`, `skip`
  - Switch statements and expressions: `switch`, `case`, `default`
- Function keywords: `return`, `extern`
- Structure keywords: `use`, `struct`, `this`
- Class keywords: `class`, `inherits`, `base`, `interface`, `abstract`, `virtual`, `override`
- Access modifiers: `public`, `private`, `protected`, `const`, `static`
- Property keywords: `get`, `set`, `value`
- Boolean literals: `true`, `false`
- Null literal: `null`
- Exception handling: `assert`
- Pass by reference: `ref`
- Variadic parameters: `params`
- Memory safety: `unsafe`
- Memory management: `new`, `free`
- Smart pointers: `unique`, `shared`, `weak`

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
- Format specifiers are optional and currently supported for floating-point values using `:digits` to control precision (e.g. `pi:3`).
- To emit a literal backtick inside an interpolated string, escape it with `` \` ``.

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

Multiline array literals are also supported. Place each element on its own line (including expressions or nested calls) and close the literal with the matching bracket on a new line:

```c
int[] samples = [
    1,
    compute(
        base,
        offset,
        2
    ),
    5
]
```

The same indentation rules apply to rectangular (`[,]`) and jagged (`[][]`) literals, making it easier to visualize large datasets.

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

## Heap Allocation and Release

Hybrid exposes `new` and `free` for explicit heap management on top of ARC:

- `new Type(args)` or `new(args)` allocate ARC-managed objects and run constructors. The type can be inferred from the assignment target.
- `new Type[len]` or `new[len]` allocate a 1-D array, zero-initialized with the length stored alongside the data for bounds checks.
- `new Type[len1, len2]` or `new[len1, len2]` allocate a rectangular array with one bound per dimension.
- `free expr` schedules an explicit ARC release. Its use is rare, as ARC still releases automatically, and is only valid for ARC-managed references (classes, arrays, and pointers to those). Smart pointers and stack values reject `free`.

See `docs/memory/heap_allocation.md` for full semantics and diagnostics.

### Automatic Reference Counting (ARC)

- ARC is enabled by default. Class, struct, and array references are retained on assignment and released when they leave scope; destructors run when the last strong reference drops to zero.
- `free` is optional and only valid for ARC-managed heap references when you need a deterministic release point. Stack values and smart pointer handles reject it because ARC already tears them down automatically.
- Toggle ARC with `--arc-enabled=true|false` (or `./run_tests.sh -a on|off`). Disabling ARC skips retain/release insertion and suppresses ARC-only diagnostics so tooling can compare behaviors without rewriting source.
- Smart pointer helpers (`unique<T>`, `shared<T>`, `weak<T>`) wrap the same ARC-managed payloads; use them when you need exclusive/shared/weak semantics beyond the current scope, not to get basic lifetime management.

```cs
Door local = new(2)                   // Freed automatically when it leaves scope
shared<Door> sharedDoor = (3)
weak<Door> watcher = #sharedDoor      // Observer that won't keep the Door alive
```

See `docs/memory/arc_best_practices.md` for ownership patterns and common ARC pitfalls.

## Nullability

- Types are non-nullable unless explicitly annotated with `?` (for example, `int?`, `User?`, `string[]?`).
- The null-safe member access operator `?.` must be used when dereferencing nullable struct values: `user?.address?.city`.
- Assigning `null` or any nullable expression to a non-nullable variable, field, or array element results in a compile-time error.
- Flow analysis narrows nullable types inside guarded regions. Within `if maybeUser != null { ... }` the compiler treats `maybeUser` as non-null, and the `else` side gains the complementary knowledge for `if maybeUser == null`. The same reasoning applies to loop conditions such as `while node != null`.

### Null Safety

- Types are non-nullable unless annotated with `?` (for example `int?`, `User?`, `float?[]`, `string[]?`)
- Use `?.` for null-safe member access on nullable struct references
- Use `?[` for null-safe element access on nullable array references
- Use `??` to supply a fallback when a nullable value is `null`
- Use `??=` to assign a fallback when a nullable variable is `null`

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
