# Expressions

## Overview

Expressions in Hybrid are computations that produce values. The language supports arithmetic operations, comparisons, boolean logic, function calls, and variable references. All expressions have types determined at compile time.

## Numeric Literal Formats

Hybrid accepts several literal prefixes so values can be written in the representation that matches the problem domain:

| Format | Prefix | Example | Description |
|--------|--------|---------|-------------|
| Decimal | _(none)_ | `42`, `-17`, `3.1415` | Default integer or floating-point literal |
| Binary | `0b` / `0B` | `0b1101`, `0B00101111` | Base-2 integers |
| Octal | `0o` / `0O` | `0o755`, `0O12` | Base-8 integers |
| Hexadecimal | `0x` / `0X` | `0xFF`, `0XDEADBEEF` | Base-16 integers |
| Scientific | `e` / `E` | `6.022e23`, `1.2E-3` | Exponential floating-point notation |

Additional conveniences:

- Leading-dot floats such as `.5` are accepted and treated as `0.5`.
- Integer literals are automatically range-checked during lexing; values that would overflow 64 bits trigger a diagnostic.
- During code generation a literal adopts the type of its context. For example `0xFF` stored in a `byte` remains 8 bits, while the same literal in a `long` is sign-extended to 64 bits.

See [Type System - Literal Type Inference](type-system.md#context-aware-literal-type-inference) for how Hybrid narrows or widens literals based on surrounding expressions.

## Operator Precedence

Operators are evaluated according to precedence rules (higher numbers bind tighter):

| Precedence | Operators | Type | Associativity |
|------------|-----------|------|---------------|
| 40 | `*`, `/`, `%` | Multiplication | Left-to-right |
| 20 | `+`, `-` | Addition | Left-to-right |
| 15 | `<<`, `>>` | Bitwise shift | Left-to-right |
| 10 | `<`, `>`, `<=`, `>=`, `==`, `!=`, `is`, `is not` | Comparison | Left-to-right |
| 9 | `&` | Bitwise AND | Left-to-right |
| 8 | `^` | Bitwise XOR | Left-to-right |
| 7 | `\|` | Bitwise OR  | Left-to-right |
| 6 | `&&` | Logical AND | Left-to-right |
| 5 | `\|\|` | Logical OR | Left-to-right |
| 4 | `if...else...` | Ternary | Right-to-left |
| 2 | `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `\|=`, `^=`, `<<=`, `>>=` | Assignment | Right-to-left |

## Arithmetic Operators

### Binary Arithmetic

```c
// Addition
int sum = 10 + 5        // 15
float total = 3.5 + 2.5 // 6.0

// Subtraction
int diff = 10 - 3       // 7
float delta = 5.5 - 1.5 // 4.0

// Multiplication
int product = 4 * 5     // 20
float area = 3.5 * 2.0  // 7.0

// Division
float quotient = 10.0 / 3.0  // 3.33...
int intDiv = 10 / 3          // 3 (integer division)

// Modulo (remainder)
int remainder = 17 % 5  // 2
int check = 20 % 6      // 2
```

#### Arithmetic with Sized Types

Number literals automatically adapt to match the type of the variable they're used with:

```c
// With byte (8-bit)
byte b = 50
byte sum = b + 10       // 10 becomes i8, result is i8
byte doubled = b * 2    // 2 becomes i8

// With short (16-bit)
short s = 1000
short total = s + 500   // 500 becomes i16
short halved = s / 2    // 2 becomes i16

// With long (64-bit)
long l = 5000000
long big = l + 4000000  // 4000000 becomes i64

// Works in both operand positions
byte b2 = 20
byte result1 = b2 + 5   // Right operand is literal
byte result2 = 5 + b2   // Left operand is literal
```

See [Type System - Literal Type Inference](type-system.md#context-aware-literal-type-inference) for more details.

### Unary Operators

```c
// Negation
int negative = -5       // -5
float neg = -3.14      // -3.14
int expr = -(10 + 5)   // -15
```

### Increment and Decrement Operators

Hybrid supports both prefix and postfix increment (`++`) and decrement (`--`) operators:

#### Prefix Operators
Prefix operators modify the variable first, then return the new value:

```c
int x = 5
int y = ++x     // x becomes 6, y gets 6
int z = --x     // x becomes 5, z gets 5

// With floats
float f = 3.5
float g = ++f   // f becomes 4.5, g gets 4.5
```

#### Postfix Operators
Postfix operators return the current value first, then modify the variable:

```c
int x = 5
int y = x++     // y gets 5, x becomes 6
int z = x--     // z gets 6, x becomes 5

// With floats
float f = 3.5
float g = f++   // g gets 3.5, f becomes 4.5
```

#### Usage Examples

```c
// In loops - postfix is common for loop counters
int i = 0
while i < 10
{
    process(i)
    i++         // Increment after using the value
}

// With arrays
int[] arr = [10, 20, 30]
int index = 0
int val1 = arr[index++]  // val1 gets arr[0] (10), index becomes 1
int val2 = arr[index++]  // val2 gets arr[1] (20), index becomes 2

// Prefix when you need the new value immediately
int count = 0
if ++count == 1     // Increment first, then compare
{
    // This executes because count is now 1
}

// Difference in expressions
int a = 5
int b = 5
int result1 = 10 + a++  // result1 = 15 (10 + 5), a becomes 6
int result2 = 10 + ++b  // result2 = 16 (10 + 6), b becomes 6
```

Both prefix and postfix operators work with integers and floating-point numbers.

## Comparison Operators

All comparison operators return boolean values:

```c
// Equality
bool eq = 5 == 5       // true
bool neq = 5 != 3      // true

// Relational
bool lt = 3 < 5        // true
bool gt = 7 > 4        // true
bool lte = 5 <= 5      // true
bool gte = 6 >= 6      // true

// With expressions
bool complex = (x + 5) > (y * 2)
```

### Comparisons with Sized Types

Number literals automatically adapt to match the type being compared:

```c
// With byte
byte b = 100
bool check1 = b == 100      // 100 becomes i8
bool check2 = 255 > b       // 255 becomes i8
bool check3 = b <= 200      // 200 becomes i8

// With short
short s = 1000
bool valid = s == 1000      // 1000 becomes i16
bool inRange = s >= 500 && s <= 1500  // Both literals become i16

// Works in both directions
byte value = 42
bool test1 = value == 42    // Literal on right
bool test2 = 42 == value    // Literal on left
```

This eliminates the need for explicit casts like `b == byte: 100` in most cases. See [Type System - Literal Type Inference](type-system.md#context-aware-literal-type-inference) for details.

## Type Check Operators

Hybrid provides runtime type checks for conditional expressions:

- `is` returns `true` when the left-hand value matches the right-hand type.
- `is not` inverts the result.

Type checks are only valid inside `if`/`while` conditions and ternary conditions.

```c
if pet is Animal
{
    // type matches
}

if pet is not Tree
{
    // type does not match
}

while node is not null
{
    // loop until null
}

Animal fallback = pet if pet is Animal else new()
```

### Pattern Binding

Type checks can introduce a binding when the match succeeds:

```c
if pet is Dog dog
{
    print(dog.Id())
}
```

The binding is available in the branch where the type check succeeds (`then` for `is`, `else` for `is not`).

> C#-style `as` is not supported. Use explicit casts (`Type: expr`) or constructors instead.

## Boolean Operators

### Logical AND (`&&`)

```c
bool result = true && true    // true
bool check = (x > 0) && (x < 10)
bool valid = isReady && (count > 0)
```

### Logical OR (`||`)

```c
bool result = true || false   // true
bool check = (x < 0) || (x > 100)
bool allowed = isAdmin || isOwner
```

### Logical NOT (`!`)

```c
bool notTrue = !true          // false
bool invalid = !isValid
bool outside = !(x >= 0 && x <= 10)
```

### Short-circuit Evaluation

Boolean operators use short-circuit evaluation:

```c
// If x <= 0, the second part is not evaluated
if x > 0 && expensive_check(x)
{
    // ...
}

// If x == 0, the second part is not evaluated
if x == 0 || risky_operation(x)
{
    // ...
}
```

## Ternary Operator (Conditional Expression)

The ternary operator provides a concise way to write conditional expressions. It uses the syntax `value_if_true if condition else value_if_false`, similar to Python's ternary syntax.

### Basic Syntax

```c
// Basic ternary expression
int max = a if a > b else b

// Equivalent to:
int max
if a > b
{
    max = a
}
else
{
    max = b
}
```

### Common Use Cases

#### Simple Conditional Assignment

```c
// Choose between two values
int x = 10
int y = 5
int maximum = x if x > y else y  // maximum = 10

// With boolean conditions
bool isReady = true
int status = 1 if isReady else 0  // status = 1

// Absolute value implementation
int absolute(int n)
{
    return n if n >= 0 else -n
}
```

#### Default Values

```c
// Provide default when condition fails
int count = userInput if userInput > 0 else 1

// String-like behavior with null checks
string name = userName if userName != null else "Unknown"
```

#### Complex Expressions

```c
// Both branches can be complex expressions
int result = x * 2 + 1 if x > threshold else y * 3 - 2

// Condition can be complex
int value = primary if isValid && count > 0 else fallback

// Nested in function calls
print(positive if number >= 0 else negative)
```

### Type System Integration

The ternary operator integrates with Hybrid's type system, supporting automatic type promotion:

#### Automatic Type Promotion

```c
// Integer to double promotion
double mixed = 3.14 if useFloat else 42  // 42 promoted to 42.0

// Integer to float promotion
float value = 2.5 if condition else 10   // 10 promoted to 10.0
```

#### Type Compatibility Rules

```c
// Valid: same types
int same = 10 if condition else 20

// Valid: compatible numeric types (with promotion)
double promoted = 5.5 if condition else 42  // int promoted to double

// Invalid: incompatible types
int invalid = "hello" if condition else 42  // Compilation error
```

### Precedence and Associativity

The ternary operator has precedence level 4, which means:

```c
// Higher precedence than logical OR/AND
int result = a if x || y else b  // Parsed as: a if (x || y) else b

// Lower precedence than comparisons
int value = x if y > z else w    // Parsed as: x if (y > z) else w

// Right-associative for multiple ternary operators
int nested = a if cond1 else b if cond2 else c
// Parsed as: a if cond1 else (b if cond2 else c)
```

### Usage in Different Contexts

#### Variable Declarations

```c
int x = 10
int y = 20
int max = x if x > y else y
bool flag = true if x > 0 else false
```

#### Function Returns

```c
int sign(int n)
{
    return 1 if n > 0 else -1 if n < 0 else 0
}

double safeDivide(double a, double b)
{
    return a / b if b != 0.0 else 0.0
}
```

#### Function Arguments

Hybrid tracks unmatched parentheses, so you can split argument lists over many lines for clarity. Indent each argument underneath the call site and close the expression with the right parenthesis on its own line.

```c
// Pass conditional values to functions
print(x if x > 0 else 0)
process(primary if isValid else backup)

// Complex argument expressions
result = calculate(
    x * 2 if mode == 1 else x + 10,
    y if y > threshold else defaultY
)
```

#### Array Indexing

```c
int[] arr = [10, 20, 30]
int index = 1 if useSecond else 0
int value = arr[index if index < 3 else 0]
```

### Error Handling

The compiler provides clear error messages for ternary operator issues:

```c
// Missing else clause
int invalid1 = 10 if true  // Error: Expected 'else' after condition

// Invalid condition type
string text = "hello"
int invalid2 = 5 if text else 10  // Error: Ternary condition must be numeric

// Incompatible branch types
int invalid3 = "hello" if true else 42  // Error: Branches must have compatible types
```

### LLVM Code Generation

The ternary operator generates efficient LLVM IR using conditional branches and PHI nodes:

```llvm
; For: int result = a if condition else b
%cond = load i1, ptr %condition
br i1 %cond, label %ternary_then, label %ternary_else

ternary_then:
  %then_val = load i32, ptr %a
  br label %ternary_merge

ternary_else:
  %else_val = load i32, ptr %b
  br label %ternary_merge

ternary_merge:
  %result = phi i32 [ %then_val, %ternary_then ], [ %else_val, %ternary_else ]
```

This generates optimal machine code with proper control flow and type handling.

### Best Practices

#### When to Use Ternary

- **Simple conditional assignments**: `int max = a if a > b else b`
- **Default value selection**: `int count = input if input > 0 else 1`
- **Function return shortcuts**: `return x if x > 0 else 0`

#### When to Avoid Ternary

- **Complex logic**: Use if-else statements for multiple conditions
- **Side effects**: Avoid when branches have important side effects
- **Nested ternary**: Limit nesting for readability

#### Readability Guidelines

```c
// Good: simple and clear
int max = a if a > b else b

// Good: meaningful variable names make intent clear
int clampedValue = value if value <= maxValue else maxValue

// Avoid: too complex for ternary
int result = (x * 2 + calculateOffset()) if (mode == ADVANCED && isEnabled) else (y / 2 - getDefault())

// Better: use if-else for complex cases
int result
if mode == ADVANCED && isEnabled
{
    result = x * 2 + calculateOffset()
}
else
{
    result = y / 2 - getDefault()
}
```

## Bitwise Operators

Bitwise operators perform operations on the binary representations of integer values. They only work with integer types.

### Bitwise AND (`&`)

Performs a bitwise AND operation:

```c
int a = 12     // 1100 in binary
int b = 10     // 1010 in binary
int result = a & b  // 8 (1000 in binary)

// Common use: masking bits
int flags = 0xFF
int masked = flags & 0x0F  // Keep only lower 4 bits
```

### Bitwise OR (`|`)

Performs a bitwise OR operation:

```c
int a = 12     // 1100 in binary
int b = 10     // 1010 in binary
int result = a | b  // 14 (1110 in binary)

// Common use: setting flags
int flags = 0x00
flags = flags | 0x04  // Set bit 2
```

### Bitwise XOR (`^`)

Performs a bitwise exclusive OR operation:

```c
int a = 12     // 1100 in binary
int b = 10     // 1010 in binary
int result = a ^ b  // 6 (0110 in binary)

// Common use: toggling bits
int flags = 0xFF
flags = flags ^ 0x04  // Toggle bit 2
```

### Left Shift (`<<`)

Shifts bits to the left, filling with zeros:

```c
int a = 5      // 0101 in binary
int result = a << 2  // 20 (10100 in binary)

// Equivalent to multiplication by powers of 2
int doubled = x << 1   // x * 2
int quadrupled = x << 2  // x * 4
```

### Right Shift (`>>`)

Shifts bits to the right (arithmetic shift for signed integers):

```c
int a = 20     // 10100 in binary
int result = a >> 2  // 5 (00101 in binary)

// Equivalent to division by powers of 2
int halved = x >> 1    // x / 2
int quartered = x >> 2  // x / 4
```

### Bitwise Compound Assignment

All bitwise operators have compound assignment forms:

```c
int x = 15     // 1111 in binary

x &= 7         // x = x & 7   (result: 7)
x |= 8         // x = x | 8   (result: 15)
x ^= 3         // x = x ^ 3   (result: 12)
x <<= 1        // x = x << 1  (result: 24)
x >>= 2        // x = x >> 2  (result: 6)

// With arrays
int[] flags = [255, 128, 64]
flags[0] &= 0x0F   // Mask to lower 4 bits
flags[1] |= 0x01   // Set bit 0
flags[2] ^= 0xFF   // Toggle all bits
```

### Type Restrictions

Bitwise operators only work with integer types:

```c
// Valid
int a = 5 & 3
int b = 10 | 6
int c = 7 ^ 2

// Invalid - will cause compilation error
float x = 3.5 & 1.5    // Error: Bitwise AND requires integer operands
double y = 5.0 << 2    // Error: Left shift requires integer operands
```

## Assignment Operators

### Basic Assignment

The assignment operator (`=`) assigns the value of the right-hand expression to the left-hand variable. Chained assignments are not supported.

```c
// Simple assignment
x = 10
y = x + 5
```

### Compound Assignment

Compound assignment operators combine an operation with assignment:

```c
// Arithmetic compound assignments
int x = 10
x += 5   // Equivalent to: x = x + 5  (x becomes 15)
x -= 3   // Equivalent to: x = x - 3  (x becomes 12)
x *= 2   // Equivalent to: x = x * 2  (x becomes 24)
x /= 4   // Equivalent to: x = x / 4  (x becomes 6)
x %= 5   // Equivalent to: x = x % 5  (x becomes 1)

// Bitwise compound assignments (integer only)
int y = 15
y &= 7   // Equivalent to: y = y & 7   (y becomes 7)
y |= 8   // Equivalent to: y = y | 8   (y becomes 15)
y ^= 3   // Equivalent to: y = y ^ 3   (y becomes 12)
y <<= 2  // Equivalent to: y = y << 2  (y becomes 48)
y >>= 3  // Equivalent to: y = y >> 3  (y becomes 6)

// With expressions
int a = 20
a += (10 + 5)  // a = a + 15  (a becomes 35)
a *= (2 + 1)   // a = a * 3   (a becomes 105)

// With arrays
int[] arr = [10, 20, 30]
arr[0] += 5   // arr[0] becomes 15
arr[1] *= 2   // arr[1] becomes 40
arr[2] %= 7   // arr[2] becomes 2
```

All compound assignment operators work with both variables and array elements.

## Variable References

Variables can be used in expressions:

```c
int x = 10
int y = x + 5       // y = 15
bool check = x > 0  // check = true
```

## Function Calls

Function calls are expressions that return the function's result:

```c
// As standalone expression
getValue()

// In arithmetic
int result = add(5, 3) + multiply(2, 4)

// In assignments
int sum = calculateSum(array)

// Nested calls
int value = process(transform(getData()))
```

## Tuple Literals

Tuple literals use parentheses with commas and must contain at least two elements:

```cs
(int, string) pair = (8, "hello")
((int, int), string) point = ((1, 2), "origin")
```

Single-element parentheses remain a grouping expression: `(expr)` does not create a tuple.

## Tuple Access

Tuple indexing uses a zero-based, compile-time integer literal:

```cs
(int, string) pair = (2, "hi")
int first = pair[0]
string second = pair[1]
```

Named tuple elements are accessed with dot syntax:

```cs
(int count, string greeting) message = (2, "hi")
int c = message.count
```

Tuple elements are mutable, so assignments like `pair[0] = 3` or `message.count = 5` update the tuple value in place.

## Array Indexing

Array indexing expressions access array elements:

```c
int[] arr = [10, 20, 30]
int first = arr[0]           // 10
int value = arr[i]           // Dynamic index
int computed = arr[i + 1]    // Expression as index
```

## Type Coercion

### Automatic Promotion

In mixed-type expressions, narrower types are promoted:

```c
int x = 5
double y = 2.5
double result = x + y  // x promoted to double, result is 7.5
```

### Promotion Rules

1. `int` -> `double` in mixed arithmetic
2. All numeric types -> `bool` in boolean context (0 is false, non-zero is true)
3. Function arguments are promoted to match parameter types

## Expression Statements

Any expression can be used as a statement:

```c
// Function call expression statement
printValue(42)

// Arithmetic expression statement (result discarded)
x + y

// Assignment expression statement
x = 10
```

## Complex Expressions

### Parentheses

Parentheses override default precedence:

```c
int result1 = 2 + 3 * 4      // 14 (3*4=12, then 2+12)
int result2 = (2 + 3) * 4    // 20 (2+3=5, then 5*4)
```

### Nested Expressions

```c
// Complex arithmetic
int result = (x + y) * (z - w) / 2

// Boolean logic
bool valid = (age >= 18 && age <= 65) || hasPermission

// Mixed operators
bool inRange = x > min && x < max && isEnabled
```

## Null-Safe Member Access

Use the `?.` operator to safely traverse nullable structs:

```cs
User? maybeUser = getUser()
string? city = maybeUser?.address?.city
```

- Each `?.` short-circuits if its receiver is `null`, propagating `null` to the final result.
- Attempting to access a nullable value with `.` causes `Cannot access nullable type 'T?' without null-safe operator`.
- The null-safe member access operator (`?.`) returns a nullable result; combine with explicit null checks or helper functions to obtain non-nullable values.
- The null-safe element access operator (`?[]`) safely dereferences nullable array references, returning `null` if the array itself is `null`.

### Null Coalescing Operators

Use `??` to provide a fallback when a nullable expression evaluates to `null`, and `??=` to assign a fallback when the receiver is `null`.

```cs
string? name = findUser()
string display = name ?? "Unknown"  // if name is null, use fallback

string? cached = null
cached ??= computeName()   // assign only when cached is null
```

- `??` short-circuits: the right-hand side is evaluated only when the left operand is `null`.
- `??=` evaluates and assigns the right-hand expression only when the left operand is `null`, and yields the resulting value.
- Both operators preserve nullability: if either operand is nullable, the result is nullable.

## LLVM Code Generation

Expressions generate appropriate LLVM IR based on their types:

### Arithmetic Operations

```llvm
; Integer addition: x + y
%sum = add i32 %x, %y

; Float multiplication: a * b
%product = fmul double %a, %b

; Integer comparison: x < y
%cmp = icmp slt i32 %x, %y

; Bitwise operations
%and_result = and i32 %x, %y     ; x & y
%or_result = or i32 %x, %y       ; x | y
%xor_result = xor i32 %x, %y     ; x ^ y
%shl_result = shl i32 %x, %y     ; x << y
%ashr_result = ashr i32 %x, %y   ; x >> y (arithmetic shift)
```

### Type-specific Operations

- Integer operations: `add`, `sub`, `mul`, `sdiv`, `srem`, `icmp`
- Floating-point operations: `fadd`, `fsub`, `fmul`, `fdiv`, `frem`, `fcmp`
- Boolean operations: `and`, `or`, `xor` on `i1` values
- Bitwise operations: `and`, `or`, `xor`, `shl`, `ashr` on integer values

## Examples

### Arithmetic Expression Evaluation

```c
// Expression: 2 + 3 * 4 - 1
// Evaluation order: (3 * 4) = 12, then 2 + 12 = 14, then 14 - 1 = 13
int result = 2 + 3 * 4 - 1  // 13
```

### Boolean Expression Evaluation

```c
// Expression: x > 0 && x < 10 || x == 20
// With x = 5: true && true || false = true || false = true
// With x = 15: false && true || false = false || false = false
// With x = 20: false && false || true = false || true = true
bool inRange = x > 0 && x < 10 || x == 20
```

### Mixed Type Expression

```c
int count = 10
double rate = 0.5
double total = count * rate + 1.5  // 10 promoted to 10.0, result is 6.5
```

## Constant Expression Evaluation

Hybrid supports compile-time evaluation of constant expressions. This allows the compiler to detect errors early and optimize code generation.

### What Are Constant Expressions?

Constant expressions are expressions that can be evaluated at compile time because they consist only of:
- Literal values (numbers, booleans, strings)
- Arithmetic operations on constants
- Comparison operations on constants
- Boolean operations on constants
- Unary operations on constants

### Compile-Time Evaluation

The compiler evaluates constant expressions during parsing, which provides several benefits:

1. **Early Error Detection** - Errors in constant expressions are caught at compile time
2. **Optimization** - Constant values are computed once during compilation
3. **Type Safety** - Range checking for sized types happens at compile time

### Examples of Constant Expressions

```c
// Arithmetic constants
int x = 2 + 3 * 4        // Evaluated to 14 at compile time
float y = 10.0 / 3.0     // Evaluated to 3.333... at compile time

// Comparison constants
bool a = 10 > 5          // Evaluated to true at compile time
bool b = 2 + 2 == 5      // Evaluated to false at compile time

// Boolean logic constants
bool c = true && true    // Evaluated to true at compile time
bool d = false || true   // Evaluated to true at compile time

// Unary operations
int neg = -(5 + 3)       // Evaluated to -8 at compile time
bool not = !false        // Evaluated to true at compile time
```

### Constant Expression in Control Flow

Constant expressions are particularly useful for compile-time validation in assert statements:

```c
// Compile-time assertion checking
assert 2 + 2 == 4        // OK - evaluates to true
assert sizeof(int) > 0   // OK - evaluates to true
assert 1 == 2            // Error: Assert condition evaluates to false at compile time

// Constant conditions in if statements
if true
{
    // This block is always executed
}

if false
{
    // This block is never executed (dead code)
}
```

### Type Inference in Constant Expressions

The compiler properly infers types during constant evaluation:

```c
// Integer constants remain integers
int whole = 10 / 3              // 3 (integer division)

// Float constants maintain precision
float precise = 10.0 / 3.0      // 3.333...

// Mixed type promotion
double mixed = 5 + 2.5          // 7.5 (5 promoted to 5.0)
```

### Range Checking for Sized Types

Constant expressions enable compile-time range checking:

```c
// Valid assignments - values within range
byte b = 255                    // OK - maximum byte value
sbyte sb = -128                 // OK - minimum sbyte value

// Invalid assignments - caught at compile time
byte bad1 = 256                 // Error: 256 exceeds byte range [0-255]
sbyte bad2 = 128                // Error: 128 exceeds sbyte range [-128-127]
short bad3 = 100000             // Error: 100000 exceeds short range
```

### Limitations

Not all expressions can be evaluated at compile time:

```c
// These are NOT constant expressions:
int x = getValue()              // Function call
int y = userInput              // Variable reference
int z = array[index]           // Array access with variable index

// These must be evaluated at runtime
```
