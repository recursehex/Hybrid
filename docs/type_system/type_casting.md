# Type Casting and Promotion

Hybrid provides both automatic type promotion and explicit type casting capabilities. The `as` keyword seen in languages like C# is not supported as the casting operator handles all necessary conversions safely.

## Automatic Type Promotion

Limited automatic promotion occurs in specific contexts:

```c
int x = 5
double y = 2.5
// x is promoted to double for the addition
double result = x + y   // Result is 7.5

float f = 3.14
double d = f + 2.0      // float promoted to double - OK
```

Mixed-width integer operations also widen automatically toward the larger operand while preserving signedness. Combining
a `byte` with an `int`, or a `short` with a `long`, emits zero/sign extensions as appropriate before the arithmetic or
comparison is evaluated. The promotion never runs for assignments, so you still need an explicit cast when storing a
wide value into a narrower slot. `bool` remains isolated: you cannot promote it, and it never participates in integer
arithmetic without first converting to an explicit numeric type.

## Explicit Type Casting

Hybrid supports explicit type casting using the colon (`:`) operator with the syntax `type: expression`. This allows controlled conversion between compatible types.

### Basic Syntax

```c
// Cast float to int (truncates decimal part)
float pi = 3.14159
int whole = int: pi             // whole = 3

// Cast int to float
int count = 42
float precise = float: count    // precise = 42.0

// Cast between sized integers
int bigNum = 1000
short smallNum = short: bigNum  // smallNum = 1000

// Cast to byte with range checking
int value = 200
byte b = byte: value            // b = 200 (within byte range)
```

### Numeric Type Casting

Casting between numeric types follows these rules:

```c
// Integer to floating-point
int x = 10
double d = double: x        // d = 10.0
float f = float: x          // f = 10.0

// Floating-point to integer (truncates)
double pi = 3.14159
int truncated = int: pi     // truncated = 3
long bigInt = long: pi      // bigInt = 3

// Between different sized integers
long big = 100000
int medium = int: big       // medium = 100000
short small = short: medium // small = 100000 (if fits)
byte tiny = byte: small     // Will fail if value > 255

// Decimal conversion rules
int units = 15
decimal amount = units       // implicit int -> decimal
decimal exact = decimal: 2.5 // explicit float/double -> decimal
double approx = double: amount // explicit decimal -> double
int whole = int: amount      // explicit decimal -> int
```

`decimal` follows C#-style conversion defaults:
- Implicit: integer-like types (`byte`..`ulong`, `char`/`schar`/`lchar`) to `decimal`
- Explicit only: `decimal` to any integer type
- Explicit only: `float`/`double` to `decimal` and `decimal` to `float`/`double`
- No implicit mixed arithmetic between `decimal` and `float`/`double`

Numeric literals also participate in contextual typing: when the target is `decimal`, literals are emitted as decimal.

Assignments and initializers never perform narrowing or signedness changes automatically, so use an explicit cast any time
you store a wider integer into a narrower one (e.g. `int` → `short` or `long` → `int`), or when switching between
signed and unsigned representations (e.g. `int` ↔ `uint`, `sbyte` ↔ `byte`). The promotion rules still apply inside
arithmetic and comparisons, but the final store must use `type: expression` when the widths or signedness differ.

### Sign Extension and Zero Extension

When casting between signed and unsigned integers:

```cs
// Signed to unsigned (zero-extends if needed)
sbyte signed = -1               // 0xFF in binary
byte unsigned = byte: signed    // unsigned = 255 (0xFF)

// Unsigned to signed (sign-extends if needed)
byte b = 200                    // 0xC8 in binary
sbyte sb = sbyte: b             // sb = -56 (interprets as signed)

// Proper sign extension for larger types
short s = -100
int i = int: s                  // i = -100 (sign extended)
long l = long: i                // l = -100 (sign extended)

// Zero extension for unsigned
ushort us = 60000
uint ui = uint: us              // ui = 60000 (zero extended)
ulong ul = ulong: ui            // ul = 60000 (zero extended)
```

Just like other narrowing conversions, converting the value held in a variable between signed and unsigned forms
requires an explicit cast. Hybrid will still inject zero or sign extension automatically inside expressions so mixed
operations behave mathematically, but you must spell out the conversion when you persist the result into storage.

### Range Checking

The compiler performs range checking for literal casts:

```cs
// Valid casts - values within target range
byte b1 = byte: 255             // OK - maximum byte value
sbyte sb1 = sbyte: 127          // OK - maximum sbyte value
short s1 = short: 32000         // OK - within short range

// Invalid casts - compile-time errors
byte b2 = byte: 256             // Error: 256 exceeds byte range [0-255]
sbyte sb2 = sbyte: 128          // Error: 128 exceeds sbyte range [-128-127]
short s2 = short: 100000        // Error: 100000 exceeds short range [-32768-32767]

// Runtime values are not range-checked at compile time
int userInput = 1000
byte result = byte: userInput   // Truncates to fit at runtime
```

> [!CAUTION]
> Currently, range checks only apply to literal casts. Casting runtime values can silently truncate or reinterpret bits; validate user data before narrowing.

### Character Type Casting

Character types can be cast to and from integers:

```cs
// Character to integer
char ch = 'A'                   // Unicode 65
int code = int: ch              // code = 65
byte b = byte: ch               // b = 65

// Integer to character
int asciiCode = 66
char letter = char: asciiCode   // letter = 'B'

// Between character sizes
lchar unicode = lchar: 'Ω'      // 32-bit character
char standard = char: unicode   // 16-bit character
schar small = schar: standard   // 8-bit character
```

Character literals adapt to the width of the declaration automatically, but converting an existing character value to a
different width (or to an integer type) also needs an explicit cast (e.g. `schar small = schar: standard`, `int code = int: ch`,
`char c = char: asciiCode`). This keeps conversions predictable and surfaces range issues early.

### Complex Expression Casting

Type casting can be used in complex expressions:

```cpp
// Cast result of arithmetic
int a = 10
int b = 3
float precise = float: (a / b)      // precise = 3.0 (casts after integer division)
float better = (float: a) / b       // better = 3.333... (casts before division)

// Cast in comparisons
float x = 3.7
if (int: x) > 3 {
    // This won't execute because int: 3.7 = 3
}

// Cast in function calls
void processInt(int value) { /* ... */ }
float input = 42.7
processInt(int: input)              // Passes 42 to function

// Cast array elements
float[] floats = [1.1, 2.2, 3.3]
int truncated = int: floats[0]      // truncated = 1
```

### Type Casting in Assignments

```cs
// Cast on right-hand side
float source = 3.14
int target = int: source            // target = 3

// Cast complex expressions
int x = 5
int y = 3
float ratio = float: x / float: y   // ratio = 1.666...

// Cast with operators
byte b = 200
int shifted = (int: b) << 2         // shifted = 800

// Cast in compound assignments
float f = 3.7
int i = 10
i += int: f                         // i = 13 (10 + 3)
```

## Type Compatibility Rules

1. **Same type operations**: Operations between values of the same type are always allowed.
2. **Automatic integer to float promotion**: Integer types promote to floating-point types in mixed arithmetic.
3. **Automatic integer widening**: Different sized integers widen toward the larger operand (with correct sign/zero extension) inside comparisons and arithmetic. Narrowing assignments still require explicit casts.
4. **Signed/unsigned comparisons**: Signed and unsigned integers compare like ordinary numbers; Hybrid inserts the necessary zero or sign extension before evaluating the predicate.
5. **Decimal separation**: `decimal` mixes implicitly with integers, but never with `float`/`double`.
6. **Bool isolation**: `bool` cannot be converted to or from any other type, even with explicit casting.
7. **Literal constants**: Integer literals are automatically sized to fit the target type with range checking.

## Examples

```cs
// Valid operations with casting
short s = 100
int i = 200
int sum = (int: s) + i      // Explicit cast makes it valid

long l = 1000000
int smaller = int: l        // Explicit cast (may truncate)

// Casting for precision
int numerator = 7
int denominator = 2
double precise = (double: numerator) / denominator  // 3.5

// Invalid operations - bool cannot be cast
bool flag = true
int num = int: flag         // Error: Cannot cast bool to int
```
