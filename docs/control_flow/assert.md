# Assert Statements

Assert statements are used for debugging and testing to verify that conditions hold true at runtime. If an assertion fails, the program terminates immediately.

> [!WARNING]
> Failed assertions abort the process and cannot be caught, so any failing assert will mark the test run as a hard failure.

Assert statements can be used both inside functions and at the top level (global scope), making them useful for compile-time validation and runtime checks.

## Basic Syntax

```java
assert expression
```

The expression must evaluate to a boolean value or be convertible to boolean. If the expression evaluates to false, the program calls `abort()` and terminates.

## Scope and Usage

Assert can be used in two contexts:

1. **Inside functions**: For runtime validation within function bodies
2. **At top level**: For global constant validation and test assertions

```java
// Top-level assertions (global scope)
int global_value = 42
assert global_value == 42  // OK - validates global state

// Inside functions
int test()
{
    int x = 10
    assert x > 0  // OK - runtime validation
    return x
}
```

## Usage Examples

```java
// Simple boolean assertions
assert true              // Always passes
assert 1 == 1           // Passes
assert 2 + 2 == 4       // Passes

// Variable comparisons
int x = 10
int y = 5
assert x > y            // Passes
assert x != y           // Passes
assert x >= 10          // Passes

// Boolean operations
bool flag = true
assert flag             // Passes
assert flag && x > 0    // Passes
assert !false           // Passes

// Complex expressions
assert (x > 0) && (y > 0)     // Passes
assert x + y == 15            // Passes
assert x * y == 50            // Passes
```

## Compile-Time Constant Evaluation

Assert statements with constant expressions are evaluated at compile time. If a constant assertion would fail, the compiler reports an error:

```java
// Compile-time error - assertion always fails
assert 1 == 2          // Error: Assert condition evaluates to false at compile time
assert false           // Error: Assert condition evaluates to false at compile time

// These compile successfully as they evaluate to true
assert 10 > 5          // OK - constant expression evaluates to true
assert true && true    // OK - constant expression evaluates to true
```

## Type Conversion

Non-boolean expressions are automatically converted to boolean:
- Integer values: 0 is false, non-zero is true
- Floating-point values: 0.0 is false, non-zero is true
- Boolean values: Used directly

```java
int count = 5
assert count           // Passes (5 != 0)

float value = 0.0
assert value          // Fails (0.0 == 0)

bool active = true
assert active         // Passes
```

## Use Cases

Assert statements are commonly used for:

1. **Preconditions** - Verify function requirements:
```java
int divide(int a, int b)
{
    assert b != 0     // Ensure no division by zero
    return a / b
}
```

2. **Postconditions** - Verify function results:
```java
int abs(int x)
{
    int result = x if x >= 0 else -x
    assert result >= 0    // Result must be non-negative
    return result
}
```

3. **Invariants** - Verify loop or data structure properties:
```java
int sum(int[] arr, int len)
{
    assert len > 0       // Array must not be empty
    int total = 0
    for int i = 0 to i < len
    {
        total += arr[i]
    }
    return total
}
```

4. **Testing** - Verify expected behavior:
```java
int testAddition()
{
    assert add(2, 3) == 5
    assert add(-1, 1) == 0
    assert add(0, 0) == 0
    return 0
}
```

## Implementation Details

- Assert statements generate conditional branches in LLVM IR
- On failure, the `abort()` function is called to terminate the program
- Success branches continue normal execution
- Assert has minimal performance impact when conditions pass
- In future versions, assert statements may be optionally disabled in release builds