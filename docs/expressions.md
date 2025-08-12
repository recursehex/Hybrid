# Expressions

## Overview

Expressions in Hybrid are computations that produce values. The language supports arithmetic operations, comparisons, boolean logic, function calls, and variable references. All expressions have types determined at compile time.

## Operator Precedence

Operators are evaluated according to precedence rules (higher numbers bind tighter):

| Precedence | Operators | Associativity |
|------------|-----------|---------------|
| 40 | `*`, `/`, `%` | Left-to-right |
| 20 | `+`, `-` | Left-to-right |
| 15 | `<<`, `>>` | Left-to-right |
| 10 | `<`, `>`, `<=`, `>=`, `==`, `!=` | Left-to-right |
| 9 | `&` | Left-to-right |
| 8 | `^` | Left-to-right |
| 7 | `|` | Left-to-right |
| 6 | `&&` | Left-to-right |
| 5 | `||` | Left-to-right |
| 2 | `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=` | Right-to-left |

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
while i < 10 {
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
if ++count == 1 {       // Increment first, then compare
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
if x > 0 && expensive_check(x) {
    // ...
}

// If x == 0, the second part is not evaluated
if x == 0 || risky_operation(x) {
    // ...
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

The assignment operator (`=`) modifies variables and returns the assigned value:

```c
// Simple assignment
x = 10
y = x + 5

// Chained assignment (right-associative)
a = b = c = 0

// Assignment in expressions
while (n = getNext()) > 0 {
    // Process n
}
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