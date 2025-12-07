# Type Inference

While types must be explicitly declared, the compiler performs automatic type inference for literals:

- Whole numbers without decimal points become `int` (i32) by default
- Numbers with decimal points become `double`
- Character literals in single quotes become `char`
- String literals in double quotes become `string`
- `true` and `false` become `bool`
- Integer literals are range-checked and sized to fit the target type
- Prefixed numeric literals map directly to their radix:
  - `0b` / `0B` for binary (`0b1110` = 14)
  - `0o` / `0O` for octal (`0o755` = 493)
  - `0x` / `0X` for hexadecimal (`0xDEAD` = 57005)
  - Scientific notation is accepted for floating point (`6.022e23`, `1.2E-3`)

> [!NOTE]
> Leading-dot literals such as `.5` are interpreted as floating-point numbers (`0.5`).

> [!IMPORTANT]
> Inference only regenerates literals; it never inserts implicit casts for stored values. Declarations need an explicit type, and narrowing assignments (e.g. `int` to `byte`) require `type: expr`.

## Context-Aware Literal Type Inference

Number literals automatically adapt to the target type in binary operations, eliminating the need for explicit casts in most common cases. This feature improves code readability while maintaining type safety.

Character literals behave the same way: a literal will regenerate at the consumer's width. For example, `'A'` becomes an 8-bit `schar` when assigned to an ASCII array, stays 16-bit for `char`, and upgrades to 32-bit `lchar` when the surrounding expression requires it. If the literal cannot fit, the compiler falls back to the wider type and surfaces a type error.

Array literals reuse the same logic; every element is re-emitted using the array's element type before storage.

### How It Works

When a number literal appears in a binary operation (comparison or arithmetic) with a typed variable, the literal automatically generates with the variable's type:

```java
byte b = 100
assert b == 100         // 100 becomes i8, not i32
assert 100 == b         // Works in both directions

short s = 1000
short result = s + 50   // 50 becomes i16
short doubled = 2 * s   // 2 becomes i16
```

### Supported Operations

Literal inference works with:

**Comparison Operators:**
```cs
byte b = 50
assert b == 50        // Equality
assert b != 49        // Inequality
assert b < 100        // Less than
assert b > 25         // Greater than
assert b <= 50        // Less than or equal
assert b >= 50        // Greater than or equal
```

**Arithmetic Operators:**
```cs
byte x = 10
byte sum = x + 5      // Addition: 5 becomes i8
byte diff = 20 - x    // Subtraction: 20 becomes i8
byte prod = x * 3     // Multiplication: 3 becomes i8
byte quot = x / 2     // Division: 2 becomes i8
byte mod = x % 3      // Modulo: 3 becomes i8
```

**Function Arguments:**
```c
void processByte(byte value)
{
    // ...
}

processByte(42)       // 42 automatically becomes i8
```

### All Integer Types Supported

Literal inference works with all sized integer types:

```java
// 8-bit types
byte b = 200
assert b + 55 == 255

sbyte sb = 100
assert sb - 50 == 50

// 16-bit types
short s = 30000
assert s + 2767 == 32767

ushort us = 60000
assert us + 5535 == 65535

// 32-bit types (default)
int i = 1000
assert i * 1000 == 1000000

// 64-bit types
long l = 5000000
assert l + 4000000 == 9000000
```

### Type Safety Maintained

The compiler still prevents unsafe operations:

```java
// Range checking still enforced
byte overflow = 256     // Error: 256 exceeds byte range [0-255]

// Overflow detection in literals
byte b = 200
assert b == 256         // Error: 256 out of range for byte

// Requires explicit casts for converting to smaller types
short s = 100
byte b1 = s           // Error: requires explicit cast
byte b2 = short: s    // OK: explicit cast
```

### Complex Expressions

Literal inference works in nested expressions:

```java
byte result = (10 + 20) * 2
assert result == 60     // All literals become i8

short complex = (100 + 200) / 3
assert complex == 100
```