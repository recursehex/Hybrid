# Built-in Types

## Primitive Types

### Basic Types

| Type | Description | LLVM Type | Example |
|------|-------------|-----------|---------|
| `int` | 32-bit signed integer | `i32` | `int x = 42` |
| `float` | 32-bit floating point | `float` | `float pi = 3.14f` |
| `double` | 64-bit floating point | `double` | `double e = 2.71828` |
| `char` | 16-bit Unicode character | `i16` | `char ch = 'A'` |
| `bool` | 8-bit boolean value | `i8` | `bool flag = true` |
| `void` | No value (functions only) | `void` | `void func() { }` |
| `string` | UTF-16 string reference | `ptr` | `string s = "hello"` |

### Sized Integer Types

| Type | Description | LLVM Type | Range | Example |
|------|-------------|-----------|-------|---------|
| `byte` | 8-bit unsigned integer | `i8` | 0 to 255 | `byte b = 255` |
| `sbyte` | 8-bit signed integer | `i8` | -128 to 127 | `sbyte sb = -100` |
| `short` | 16-bit signed integer | `i16` | -32,768 to 32,767 | `short s = 1000` |
| `ushort` | 16-bit unsigned integer | `i16` | 0 to 65,535 | `ushort us = 60000` |
| `uint` | 32-bit unsigned integer | `i32` | 0 to 4,294,967,295 | `uint ui = 3000000` |
| `long` | 64-bit signed integer | `i64` | -2^63 to 2^63-1 | `long l = 1000000000` |
| `ulong` | 64-bit unsigned integer | `i64` | 0 to 2^64-1 | `ulong ul = 10000000000` |

### Sized Character Types

| Type | Description | LLVM Type | Example |
|------|-------------|-----------|---------|
| `schar` | 8-bit signed character | `i8` | `schar sc = 'A'` |
| `lchar` | 32-bit Unicode character | `i32` | `lchar lc = '\u03a9'` |

## Nullable Types

All value and reference types are non-nullable by default. Append `?` to any type name to allow `null` assignments and propagate nullable results, like `string? maybeAlias = null`. Nullable annotations apply anywhere a type appears, including arrays (`int?[]` vs `int[]?`) and function signatures, while pointer types (`int@`, `float@2`, etc.) always allow `null`. Assigning a nullable expression to a non-nullable target is a compile-time error. Use helper functions or explicit conversions that validate nullability. Accessing members on a nullable struct requires the null-safe access operator `?.`. The compiler performs flow-sensitive narrowing to treat guarded variables as non-nullable within reachable branches.

## Array Types

Arrays are declared using the `[]` syntax after the element type:

```cs
int[] numbers = [1, 2, 3, 4, 5]
float[] temperatures = [98.6, 99.1, 97.5]
char[] vowels = ['a', 'e', 'i', 'o', 'u']
bool[] flags = [true, false, true]
string[] names = ["Alice", "Bob", "Charlie"]
```

Arrays are implemented as structs containing a pointer to elements and a size in LLVM IR. Array literals regenerate their elements to match the declared element type, so `float[] temps = [98.6, 100.0]` stores true 32-bit floats even though the literal syntax defaults to `double`. The same width-aware regeneration applies to character arrays, so `schar[] ascii = ['A', 'B']` produces 8-bit code units while `lchar[]` stores full 32-bit values.

## Strings and Unicode

`string` values are stored as UTF-16 sequences. Every string literal is validated and converted from its UTF-8 spelling during lexing, then emitted as a 16-bit array that shares a single global instance: identical literals point at the same address, so pointer equality works while still representing Unicode text correctly. Character literals adopt the consumer's width, so `'😊'` becomes a 32-bit `lchar` in wide contexts, but narrows to `char`/`schar` if it fits the target range, otherwise invalid UTF-8 triggers a diagnostic. Use the sized character aliases (`schar`, `char`, `lchar`) when you need to control storage explicitly.