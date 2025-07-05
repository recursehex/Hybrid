# Expressions

## Overview

Expressions in Hybrid are computations that produce values. The language supports arithmetic operations, comparisons, boolean logic, function calls, and variable references. All expressions have types determined at compile time.

## Operator Precedence

Operators are evaluated according to precedence rules (higher numbers bind tighter):

| Precedence | Operators | Associativity |
|------------|-----------|---------------|
| 40 | `*`, `/`, `%` | Left-to-right |
| 20 | `+`, `-` | Left-to-right |
| 10 | `<`, `>`, `<=`, `>=`, `==`, `!=` | Left-to-right |
| 6 | `&&` | Left-to-right |
| 5 | `||` | Left-to-right |
| 2 | `=`, `+=`, `-=`, `*=`, `/=`, `%=` | Right-to-left |

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

1. `int` → `double` in mixed arithmetic
2. All numeric types → `bool` in boolean context (0 is false, non-zero is true)
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
```

### Type-specific Operations

- Integer operations: `add`, `sub`, `mul`, `sdiv`, `icmp`
- Floating-point operations: `fadd`, `fsub`, `fmul`, `fdiv`, `fcmp`
- Boolean operations: `and`, `or`, `xor` on `i1` values

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