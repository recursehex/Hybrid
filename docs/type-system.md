# Type System

## Overview

Hybrid uses a static type system with explicit type declarations. All variables must be declared with a type and initialized at the point of declaration. The compiler performs type checking at compile time and generates appropriate LLVM IR for each type.

## Built-in Types

### Primitive Types

#### Basic Types

| Type | Description | LLVM Type | Example |
|------|-------------|-----------|---------|
| `int` | 32-bit signed integer | `i32` | `int x = 42` |
| `float` | 32-bit floating point | `float` | `float pi = 3.14f` |
| `double` | 64-bit floating point | `double` | `double e = 2.71828` |
| `char` | 16-bit Unicode character | `i16` | `char ch = 'A'` |
| `bool` | 8-bit boolean value | `i8` | `bool flag = true` |
| `void` | No value (functions only) | `void` | `void func() { }` |
| `string` | String type | `ptr` | `string s = "hello"` |

#### Sized Integer Types

| Type | Description | LLVM Type | Range | Example |
|------|-------------|-----------|-------|---------|
| `byte` | 8-bit unsigned integer | `i8` | 0 to 255 | `byte b = 255` |
| `sbyte` | 8-bit signed integer | `i8` | -128 to 127 | `sbyte sb = -100` |
| `short` | 16-bit signed integer | `i16` | -32,768 to 32,767 | `short s = 1000` |
| `ushort` | 16-bit unsigned integer | `i16` | 0 to 65,535 | `ushort us = 60000` |
| `uint` | 32-bit unsigned integer | `i32` | 0 to 4,294,967,295 | `uint ui = 3000000` |
| `long` | 64-bit signed integer | `i64` | -2^63 to 2^63-1 | `long l = 1000000000` |
| `ulong` | 64-bit unsigned integer | `i64` | 0 to 2^64-1 | `ulong ul = 10000000000` |

#### Sized Character Types

| Type | Description | LLVM Type | Example |
|------|-------------|-----------|---------|
| `schar` | 8-bit signed character | `i8` | `schar sc = 'A'` |
| `lchar` | 32-bit Unicode character | `i32` | `lchar lc = '\u03a9'` |

### Array Types

Arrays are declared using the `[]` syntax after the element type:

```c
int[] numbers = [1, 2, 3, 4, 5]
float[] temperatures = [98.6, 99.1, 97.5]
char[] vowels = ['a', 'e', 'i', 'o', 'u']
bool[] flags = [true, false, true]
string[] names = ["Alice", "Bob", "Charlie"]

// Arrays of sized types
byte[] bytes = [10, 20, 30]
short[] shorts = [1000, 2000, 3000]
long[] longs = [1000000, 2000000]
```

Arrays are implemented as structs containing a pointer to elements and a size in LLVM IR.

## Variable Declarations

All variables must be initialized when declared:

```c
// Valid declarations
int count = 0
float rate = 0.05
bool isActive = true
string message = "Hello"
string empty = null  // Special case for strings

// Sized type declarations
byte b = 100
short s = 1000
long l = 1000000
uint ui = 4000000000

// Invalid - no initialization
int x              // Error: variable must be initialized
float y            // Error: variable must be initialized
```

## Type Inference

While types must be explicitly declared, the compiler performs automatic type inference for literals:

- Whole numbers without decimal points become `int` (i32) by default
- Numbers with decimal points become `double`
- Character literals in single quotes become `char`
- String literals in double quotes become `string`
- `true` and `false` become `bool`
- Integer literals are range-checked and sized to fit the target type

### Context-Aware Literal Type Inference

Number literals automatically adapt to the target type in binary operations, eliminating the need for explicit casts in most common cases. This feature, similar to C#, Rust, and Swift, improves code readability while maintaining type safety.

#### How It Works

When a number literal appears in a binary operation (comparison or arithmetic) with a typed variable, the literal automatically generates with the variable's type:

```c
byte b = 100
assert b == 100      // 100 becomes i8, not i32
assert 100 == b      // Works in both directions

short s = 1000
short result = s + 50  // 50 becomes i16
short doubled = 2 * s  // 2 becomes i16
```

#### Supported Operations

Literal inference works with:

**Comparison Operators:**
```c
byte b = 50
assert b == 50       // Equality
assert b != 49       // Inequality
assert b < 100       // Less than
assert b > 25        // Greater than
assert b <= 50       // Less than or equal
assert b >= 50       // Greater than or equal
```

**Arithmetic Operators:**
```c
byte x = 10
byte sum = x + 5     // Addition: 5 becomes i8
byte diff = 20 - x   // Subtraction: 20 becomes i8
byte prod = x * 3    // Multiplication: 3 becomes i8
byte quot = x / 2    // Division: 2 becomes i8
byte mod = x % 3     // Modulo: 3 becomes i8
```

**Function Arguments:**
```c
void processByte(byte value) {
    // ...
}

processByte(42)      // 42 automatically becomes i8
```

#### All Integer Types Supported

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

// 32-bit types (default, but still works)
int i = 1000
assert i * 1000 == 1000000

// 64-bit types
long l = 5000000
assert l + 4000000 == 9000000
```

#### Type Safety Maintained

The compiler still prevents unsafe operations:

```java
// Range checking still enforced
byte overflow = 256   // Error: 256 exceeds byte range [0-255]

// Overflow detection in literals
byte b = 200
assert b == 256       // Error: 256 out of range for byte

// Variable-to-variable still requires explicit casts
byte b1 = 10
short s1 = b1         // Error: requires explicit cast
short s2 = short: b1  // OK: explicit cast
```

#### Complex Expressions

Literal inference works in nested expressions:

```java
byte result = (10 + 20) * 2
assert result == 60   // All literals become i8

short complex = (100 + 200) / 3
assert complex == 100
```

#### Benefits

1. **Cleaner code**: No need for `byte: 100` in comparisons and arithmetic
2. **Maintains safety**: Overflow checking and type checking still apply
3. **Modern design**: Follows patterns from C#, Rust, and Swift
4. **Zero ambiguity**: Only applies when target type is clear from context

## Type Casting and Promotion

Hybrid provides both automatic type promotion and explicit type casting capabilities.

### Automatic Type Promotion

Limited automatic promotion occurs in specific contexts:

```c
int x = 5
double y = 2.5
// x is promoted to double for the addition
double result = x + y  // Result is 7.5

float f = 3.14
double d = f + 2.0     // float promoted to double - OK
```

### Explicit Type Casting

Hybrid supports explicit type casting using the colon (`:`) operator with the syntax `type: expression`. This allows controlled conversion between compatible types.

#### Basic Syntax

```c
// Cast float to int (truncates decimal part)
float pi = 3.14159
int whole = int: pi        // whole = 3

// Cast int to float
int count = 42
float precise = float: count  // precise = 42.0

// Cast between sized integers
int bigNum = 1000
short smallNum = short: bigNum  // smallNum = 1000 (if in range)

// Cast to byte with range checking
int value = 200
byte b = byte: value       // b = 200 (within byte range)
```

#### Numeric Type Casting

Casting between numeric types follows these rules:

```c
// Integer to floating-point
int x = 10
double d = double: x       // d = 10.0
float f = float: x         // f = 10.0

// Floating-point to integer (truncates)
double pi = 3.14159
int truncated = int: pi    // truncated = 3
long bigInt = long: pi     // bigInt = 3

// Between different sized integers
long big = 100000
int medium = int: big      // medium = 100000
short small = short: medium // small = 100000 (if fits)
byte tiny = byte: small    // Will fail if value > 255
```

#### Sign Extension and Zero Extension

When casting between signed and unsigned integers:

```cs
// Signed to unsigned (zero-extends if needed)
sbyte signed = -1         // 0xFF in binary
byte unsigned = byte: signed  // unsigned = 255 (0xFF)

// Unsigned to signed (sign-extends if needed)
byte b = 200              // 0xC8 in binary
sbyte sb = sbyte: b      // sb = -56 (interprets as signed)

// Proper sign extension for larger types
short s = -100
int i = int: s           // i = -100 (sign extended)
long l = long: i         // l = -100 (sign extended)

// Zero extension for unsigned
ushort us = 60000
uint ui = uint: us       // ui = 60000 (zero extended)
ulong ul = ulong: ui     // ul = 60000 (zero extended)
```

#### Range Checking

The compiler performs range checking for literal casts:

```cs
// Valid casts - values within target range
byte b1 = byte: 255      // OK - maximum byte value
sbyte sb1 = sbyte: 127   // OK - maximum sbyte value
short s1 = short: 32000  // OK - within short range

// Invalid casts - compile-time errors
byte b2 = byte: 256      // Error: 256 exceeds byte range [0-255]
sbyte sb2 = sbyte: 128   // Error: 128 exceeds sbyte range [-128-127]
short s2 = short: 100000 // Error: 100000 exceeds short range

// Runtime values are not range-checked at compile time
int userInput = 1000
byte result = byte: userInput  // Truncates to fit at runtime
```

## Integer Overflow Protection

The compiler includes comprehensive integer overflow detection for literal values:

### Lexer-Level Overflow Detection

The lexer detects integer literals that exceed the maximum representable value:

```cs
// Valid literals - within range
int maxInt = 2147483647          // OK - maximum i32 value
long maxLong = 9223372036854775807  // OK - maximum i64 value

// Invalid literals - overflow detected at tokenization
int overflow = 2147483648        // Error: exceeds 32-bit integer range
long overflow2 = 9223372036854775808  // Error: exceeds 64-bit integer range
```

### Overflow Detection Features

- **Early detection**: Overflow is caught during lexical analysis, before parsing
- **Precise error messages**: Clear indication of which value overflowed and the valid range
- **64-bit limit**: Maximum supported literal is `9,223,372,036,854,775,807` (2^63 - 1)
- **Negative numbers**: Handled correctly via unary negation (e.g., `-2147483648` for minimum i32)
- **Type-specific checking**: Range validation respects target type when assigning

### Implementation Details

The lexer uses `std::stoull()` with exception handling to detect overflow:

```cpp
try {
    unsigned long long value = std::stoull(NumStr);
    if (value > std::numeric_limits<long long>::max()) {
        // Overflow detected
    }
} catch (const std::out_of_range&) {
    // Overflow during conversion
}
```

### Test Coverage

See `test/types/test_overflow.hy` for valid edge cases and `test/errors/test_overflow_fail.hy` for overflow detection tests.

#### Character Type Casting

Character types can be cast to and from integers:

```cs
// Character to integer
char ch = 'A'            // Unicode 65
int code = int: ch       // code = 65
byte b = byte: ch        // b = 65

// Integer to character
int asciiCode = 66
char letter = char: asciiCode  // letter = 'B'

// Between character sizes
lchar unicode = lchar: 'Ω'    // 32-bit character
char standard = char: unicode  // 16-bit character
schar small = schar: standard  // 8-bit character
```

#### Complex Expression Casting

Type casting can be used in complex expressions:

```cpp
// Cast result of arithmetic
int a = 10
int b = 3
float precise = float: (a / b)    // precise = 3.0 (casts after integer division)
float better = (float: a) / b     // better = 3.333... (casts before division)

// Cast in comparisons
float x = 3.7
if (int: x) > 3 {
    // This won't execute because int:3.7 = 3
}

// Cast in function calls
void processInt(int value) { /* ... */ }
float input = 42.7
processInt(int: input)  // Passes 42 to function

// Cast array elements
float[] floats = [1.1, 2.2, 3.3]
int truncated = int: floats[0]    // truncated = 1
```

#### Type Casting in Assignments

```cs
// Cast on right-hand side
float source = 3.14
int target = int: source          // target = 3

// Cast complex expressions
int x = 5
int y = 3
float ratio = float: x / float: y  // ratio = 1.666...

// Cast with operators
byte b = 200
int shifted = (int: b) << 2       // shifted = 800

// Cast in compound assignments
float f = 3.7
int i = 10
i += int: f                       // i = 13 (10 + 3)
```

### Type Compatibility Rules

1. **Same type operations**: Operations between values of the same type are always allowed
2. **Automatic integer to float promotion**: Integer types can be promoted to floating-point types in mixed arithmetic
3. **Explicit casting required**: Different sized integers require explicit casting (e.g., `short + int` requires casting)
4. **Bool isolation**: `bool` cannot be converted to or from any other type, even with explicit casting
5. **Literal constants**: Integer literals are automatically sized to fit the target type with range checking
6. **Sign handling**: Casting between signed and unsigned types uses proper sign/zero extension

### Examples

```cs
// Valid operations with casting
short s = 100
int i = 200
int sum = (int: s) + i         // Explicit cast makes it valid

long l = 1000000
int smaller = int: l            // Explicit cast (may truncate)

// Casting for precision
int numerator = 7
int denominator = 2
double precise = (double: numerator) / denominator  // 3.5

// Invalid operations - bool cannot be cast
bool flag = true
int num = int: flag            // Error: cannot cast bool to int
```

## Function Types

Functions have types determined by their return type and parameter types:

```cs
// Function type: (int, int) -> int
int add(int x, int y) { return x + y }

// Function type: (double) -> double
double square(double x) { return x * x }

// Function type: (int[]) -> int
int sum(int[] arr) { return arr[0] + arr[1] }

// Function type: () -> void
void printHello() { return }

// Functions with sized types
byte processByte(byte b) { return b + 1 }
long calculateLong(long x, long y) { return x * y }
```

## Type Safety

The type system enforces several safety rules:

1. **No implicit narrowing conversions**: Cannot assign `double` to `int` without explicit cast
2. **Strict integer size checking**: Cannot mix different sized integers (e.g., `short` and `int`)
3. **Array bounds**: Array indices must be integers
4. **Function calls**: Arguments must match parameter types exactly (except for int-to-float promotion)
5. **Bool isolation**: Boolean values cannot be converted to or from other types
6. **Range checking**: Integer literals must fit in the target type's range
7. **Null safety**: Only strings can be initialized with `null`

## LLVM Type Mapping

Hybrid types map directly to LLVM types:

| Hybrid Type | LLVM IR Type | Notes |
|-------------|--------------|-------|
| `int` | `i32` | 32-bit signed integer |
| `uint` | `i32` | 32-bit unsigned integer |
| `short` | `i16` | 16-bit signed integer |
| `ushort` | `i16` | 16-bit unsigned integer |
| `long` | `i64` | 64-bit signed integer |
| `ulong` | `i64` | 64-bit unsigned integer |
| `byte` | `i8` | 8-bit unsigned integer |
| `sbyte` | `i8` | 8-bit signed integer |
| `float` | `float` | 32-bit IEEE float |
| `double` | `double` | 64-bit IEEE float |
| `char` | `i16` | 16-bit Unicode character |
| `schar` | `i8` | 8-bit signed character |
| `lchar` | `i32` | 32-bit Unicode character |
| `bool` | `i8` | 8-bit integer |
| `void` | `void` | No value |
| `string` | `ptr` | Pointer to i8 |
| `T[]` | `struct {ptr, i32}` | Array struct with pointer and size |

## Global vs Local Variables

Variables can be declared at global scope or local scope:

```cs
// Global variables - visible to all functions
int globalCount = 0
string appName = "MyApp"
byte maxRetries = 3
long totalBytes = 0

int useGlobal() {
    return globalCount  // Can access global variables
}

int localExample() {
    // Local variables - only visible within function
    int localVar = 10
    short localShort = 100
    return localVar
}
```

Global variables are stored as LLVM global variables and persist for the program's lifetime.