# Control Flow

## Overview

Hybrid provides five main control flow constructs: if-else statements, while loops, for loops, foreach loops, and switch statements. All control flow statements use curly braces `{}` for their bodies and support nesting. Loops support `break` and `skip` statements for flow control.

## If-Else Statements

Conditional branching using C-style if-else syntax.

### Basic Syntax

```c
if condition
{
    // statements
}

if condition
{
    // if body
}
else
{
    // else body
}
```

### If-Else-If Chains

```c
if score >= 90
{
    return 4  // A grade
}
else if score >= 80
{
    return 3  // B grade
}
else if score >= 70
{
    return 2  // C grade
}
else
{
    return 1  // F grade
}
```

### Consecutive If Statements

Multiple if statements can be used consecutively, often with early returns:

```c
int compare(int a, int b)
{
    if a == b { return 0 }
    if a < b { return -1 }
    if a > b { return 1 }
    return 0
}
```

### Comparison Operators

All standard comparison operators are supported:

- `==` - Equal to
- `!=` - Not equal to
- `<` - Less than
- `>` - Greater than
- `<=` - Less than or equal to
- `>=` - Greater than or equal to

### Boolean Operators

Complex conditions can be built using boolean operators:

- `&&` - Logical AND
- `||` - Logical OR
- `!` - Logical NOT

```c
// AND operator
if x > 0 && x < 10
{
    return true
}

// OR operator
if status == 1 || status == 2
{
    return active
}

// NOT operator
if !finished
{
    return false
}

// Complex conditions
if (x > 0 && y > 0) || override
{
    return true
}
```

### Operator Precedence

Boolean operators have the following precedence (lowest to highest):
1. `||` (OR)
2. `&&` (AND)
3. Comparison operators (`>`, `<`, `>=`, `<=`, `==`, `!=`)
4. Arithmetic operators (`+`, `-`, `*`, `/`, `%`)

### Assignment Safety in Conditions

Assignments cannot be used as conditions in control flow statements. This prevents common bugs where assignment (`=`) is mistakenly used instead of comparison (`==`):

```c
// Compilation error - assignment in if condition
if x = 0 {
    print("This won't compile")
}

// Correct - use comparison for conditions
if x == 0 {
    print("This works correctly")
}

// Compilation error - assignment in while condition
while status = getStatus() {
    // This won't compile
}

// Correct - separate assignment and condition
status = getStatus()
while status != 0 {
    // Process status
    status = getStatus()
}
```

This safety feature applies to all control flow statements that use conditions (`if`, `while`, etc.).

## While Loops

While loops repeat a block of statements as long as a condition is true.

### Basic Syntax

```c
while condition
{
    // statements
}
```

### Examples

```c
// Simple countdown
int countdown(int n)
{
    while n > 0
    {
        n--
    }
    return n
}

// Loop with complex condition
int processItems(int count, bool active)
{
    while count > 0 && active
    {
        count--
        if count == 5
        {
            active = false
        }
    }
    return count
}
```

### Nested While Loops

```c
int nestedExample()
{
    int i = 0
    while i < 3
    {
        int j = 0
        while j < 2
        {
            j++
        }
        i++
    }
    return i
}
```

### Early Exit with Return

While loops can be exited early using return statements:

```c
int findFirst(int[] arr, int target)
{
    int i = 0
    while i < 10
    {
        if arr[i] == target
        {
            return i  // Early exit
        }
        i++
    }
    return -1  // Required return
}
```

### Break Statements

The `break` statement provides another way to exit loops early:

```c
// Basic break usage
int countUntilZero(int[] values)
{
    int count = 0
    int i = 0
    while i < 100
    {
        if values[i] == 0
        {
            break  // Exit the loop
        }
        count++
        i++
    }
    return count
}

// Break in infinite loop
int readUntilDone()
{
    int sum = 0
    while true
    {
        int value = getInput()
        if value < 0
        {
            break
        }
        sum += value
    }
    return sum
}
```

#### Break in Nested Loops

Break only exits the innermost loop:

```c
int nestedBreak()
{
    int total = 0
    int i = 0
    while i < 10
    {
        int j = 0
        while j < 10
        {
            if j == 5
            {
                break  // Only exits inner loop
            }
            total++
            j++
        }
        i++
    }
    return total  // Returns 50 (10 * 5)
}
```

### Skip Statements

The `skip` statement continues to the next iteration of a loop (similar to `continue` in C/C++):

```c
// Skip even numbers
int sumOddNumbers(int[] values)
{
    int sum = 0
    int i = 0
    while i < 10
    {
        i++
        if values[i] % 2 == 0
        {
            skip  // Continue to next iteration
        }
        sum += values[i]
    }
    return sum
}

// Skip with multiple conditions
int processFiltered(int[] data)
{
    int count = 0
    int i = 0
    while i < 100
    {
        if data[i] < 0 || data[i] > 100
        {
            i++
            skip
        }
        count++
        i++
    }
    return count
}
```

#### Skip in Nested Loops

Like `break`, `skip` only affects the innermost loop:

```c
int nestedSkip()
{
    int total = 0
    int i = 0
    while i < 10
    {
        int j = 0
        while j < 10
        {
            j++
            if j % 2 == 0
            {
                skip  // Only skips inner loop iteration
            }
            total=+
        }
        i++
    }
    return total  // Returns 50 (10 * 5 odd numbers)
}
```

#### Important Notes for Break and Skip

- `break` and `skip` can only be used inside `while` or `for` loops
- `break` immediately transfers control to the statement after the loop
- `skip` immediately transfers control to the loop's condition check (while) or increment (for)
- In nested loops, both statements only affect the innermost enclosing loop
- Using `break` or `skip` outside of a loop results in a compilation error

## For Loops

For loops provide flexible iteration with initialization, condition, and step control using Hybrid's `to` syntax.

### Basic Syntax

```c
// Basic for-to loop
for type variable = initial to limit
{
    // loop body
}

// With custom step
for type variable = initial to limit by step
{
    // loop body
}

// With custom condition
for type variable = initial to variable comparison expression
{
    // loop body
}

// Anonymous loop (no variable declaration)
for initial to limit
{
    // loop body
}
```

### Basic For Loops

```c
// Forward loop
for int i = 1 to 10
{
    print(i)  // 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
}

// Reverse loop (automatic decrement)
for int i = 10 to 1
{
    print(i)  // 10, 9, 8, 7, 6, 5, 4, 3, 2, 1
}

// Starting from 0
for int i = 0 to 5
{
    print(i)  // 0, 1, 2, 3, 4, 5
}

// Using variable limits
int limit = 7
for int i = 1 to limit
{
    print(i)  // 1, 2, 3, 4, 5, 6, 7
}
```

### Anonymous For Loops

For loops without variable declarations use internal counters:

```c
// Anonymous counter
for 0 to 5
{
    print("Hello")  // Prints 6 times (0 through 5)
}

// With expressions
int start = 2
int end = 4
for start to end
{
    print("World")  // Prints 3 times (2, 3, 4)
}

// With calculations
for 1 * 2 to 3 + 2
{
    print("Math")  // From 2 to 5, prints 4 times
}
```

### Custom Steps with 'by' Keyword

The `by` keyword allows custom step sizes:

```c
// Simple step
for int i = 0 to 20 by 2
{
    print(i)  // 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20
}

// Negative step
for int i = 10 to 0 by -2
{
    print(i)  // 10, 8, 6, 4, 2, 0
}

// Variable step
int step = 5
for int i = 0 to 25 by step
{
    print(i)  // 0, 5, 10, 15, 20, 25
}

// Expression step
for int i = 0 to 15 by 2 + 1
{
    print(i)  // 0, 3, 6, 9, 12, 15
}
```

### Multiplicative and Divisive Steps

Advanced step operations using mathematical operators:

```c
// Multiplication step
for int i = 1 to 100 by * 2
{
    print(i)  // 1, 2, 4, 8, 16, 32, 64
}

// Division step
for int i = 100 to 1 by / 2
{
    print(i)  // 100, 50, 25, 12, 6, 3, 1
}

// Subtraction step
for int i = 20 to 5 by - 3
{
    print(i)  // 20, 17, 14, 11, 8, 5
}

// Modulo step (for specialized patterns)
for int i = 17 to 1 by % 5
{
    print(i)  // 17, 2, 2, 2... (use with break)
    if i < 10 { break }
}
```

### Exclusive Bounds with Comparisons

Custom conditions for precise loop control:

```c
// Less than (exclusive upper bound)
int size = 5
for int i = 0 to i < size
{
    print(i)  // 0, 1, 2, 3, 4 (excludes 5)
}

// Less than or equal
for int i = 0 to i <= size
{
    print(i)  // 0, 1, 2, 3, 4, 5 (includes 5)
}

// Greater than (reverse, exclusive)
for int i = 10 to i > 5
{
    print(i)  // 10, 9, 8, 7, 6 (excludes 5)
}

// Complex conditions
int max = 10
for int i = 0 to i < max && i != 5
{
    print(i)  // 0, 1, 2, 3, 4 (stops at 5)
}

// With custom steps
for int i = 0 to i < 20 by 3
{
    print(i)  // 0, 3, 6, 9, 12, 15, 18
}
```

### Floating Point Loops

Full support for float and double types:

```c
// Basic float loop
for float f = 0.0 to 1.0 by 0.1
{
    print(int: (f * 10))  // 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
}

// Float with larger steps
for float f = 0.0 to 2.0 by 0.5
{
    print(int: (f * 10))  // 0, 5, 10, 15, 20
}

// Reverse float loop
for float f = 5.0 to 0.0 by -1.0
{
    print(int: f)  // 5, 4, 3, 2, 1, 0
}

// Float multiplication
for float f = 1.0 to 32.0 by * 2.0
{
    print(int: f)  // 1, 2, 4, 8, 16, 32
}

// Double precision
for double d = 0.0 to 1.0 by 0.25
{
    print(int: (d * 100))  // 0, 25, 50, 75, 100
}

// Exclusive float bounds
for float f = 0.0 to f < 1.0 by 0.2
{
    print(int: (f * 10))  // 0, 2, 4, 6, 8
}
```

### Nested For Loops

For loops can be nested with other loop types:

```c
// Nested for loops
for int i = 1 to 3
{
    for int j = 1 to 2
    {
        print(i * 10 + j)  // 11, 12, 21, 22, 31, 32
    }
}

// Mixed with while
for int i = 0 to 2
{
    int j = 0
    while j < 3
    {
        print(i * 10 + j)  // 0, 1, 2, 10, 11, 12, 20, 21, 22
        j++
    }
}

// Mixed with foreach
int[] values = [1, 2, 3]
for int i = 0 to 1
{
    for int val in values
    {
        print(i * 10 + val)  // 1, 2, 3, 11, 12, 13
    }
}
```

### Break and Skip in For Loops

Control flow statements work in for loops:

```c
// Break example
for int i = 1 to 10
{
    if i == 5
    {
        break  // Exit at 5
    }
    print(i)  // 1, 2, 3, 4
}

// Skip example  
for int i = 1 to 10
{
    if i % 2 == 0
    {
        skip  // Skip even numbers
    }
    print(i)  // 1, 3, 5, 7, 9
}

// With custom steps
for int i = 0 to 100 by 10
{
    if i == 50
    {
        break
    }
    print(i)  // 0, 10, 20, 30, 40
}
```

### All Integer Types Supported

For loops work with all Hybrid integer types:

```c
// Different integer sizes
for byte b = 0 to 255 by 50
{
    print(b)  // 0, 50, 100, 150, 200, 250
}

for short s = -100 to 100 by 25
{
    print(s)  // -100, -75, -50, -25, 0, 25, 50, 75, 100
}

for long l = 0 to 10
{
    print(int: l)  // 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
}
```

## Foreach Loops

Foreach loops iterate over collections with a typed loop variable.

### Basic Syntax

```c
for type variable in collection
{
    // loop body
}
```

### Examples

```c
// Iterate over an array
int[] numbers = [1, 2, 3]
for int num in numbers
{
    sum += num
}

// Process each element
for float temp in temperatures
{
    if temp > 98.6
    {
        count++
    }
}

// Using break and skip in foreach
for int value in data
{
    if value < 0
    {
        break  // Exit the loop
    }
    if value == 0
    {
        skip   // Continue to next element
    }
    result += value
}
```

### Nested Foreach Loops

```c
for int i in outerList
{
    for int j in innerList
    {
        result += (i * j)
    }
}
```

### Important Notes

- The loop variable is scoped to the loop body
- The loop variable type must match the collection element type
- Foreach loops support `break` and `skip` statements
- Arrays are automatically sized based on their initialization
- Loop variable is a copy of the array element

## Implementation Details

- Non-boolean conditions cannot be used and will result in a compilation error
- Block statements create new scopes for local variables
- Control flow statements can be nested to any depth
- LLVM IR uses phi nodes and basic blocks for control flow
- Foreach loops use internal counters and array indexing for iteration
- For loops create separate AST nodes (`ForLoopStmtAST`) from foreach loops (`ForEachStmtAST`)
- For loops automatically detect increment vs decrement based on initial and limit values
- Anonymous for loops generate internal variable names (`__anon_loop_var_N`)
- Custom conditions in for loops bypass automatic limit checking
- Float loops use LLVM floating-point operations (`fadd`, `fcmp`, etc.)
- Step operations support all arithmetic operators (`+`, `-`, `*`, `/`, `%`)
- Type conversions ensure proper matching between variable type and operand types
- Break jumps to the loop exit block, skip jumps to the increment/condition block

## Switch Statements and Expressions

Switch statements provide efficient multi-way branching based on a value. Hybrid supports both block-style switch statements and arrow-syntax switch expressions.

### Switch Statements (Block Style)

Switch statements execute different blocks of code based on the value of an expression.

#### Basic Syntax

```c
switch expression
{
    case value
    {
        // statements to execute
    }
    
    case value2
    {
        // statements to execute
    }
    
    default
    {
        // default case (optional but recommended)
    }
}
```

#### Examples

```c
// Simple switch statement
int x = 2

switch x
{
    case 1
    {
        int result = 10
        print(result)
    }
    
    case 2
    {
        int result = 20
        print(result)
    }
    
    default
    {
        int result = 0
        print(result)
    }
}
```

#### Multiple Values per Case

```c
// Multiple values in a single case
int dayOfWeek = 1

switch dayOfWeek
{
    case 1, 2, 3, 4, 5
    {
        string type = "Weekday"
        print(type)
    }
    
    case 6, 7
    {
        string type = "Weekend"
        print(type)
    }
    
    default
    {
        string type = "Invalid"
        print(type)
    }
}
```

#### Character Cases

```c
// Switch on character values
char grade = 'B'

switch grade
{
    case 'A'
    {
        int points = 4
    }
    
    case 'B'
    {
        int points = 3
    }
    
    case 'C'
    {
        int points = 2
    }
    
    default
    {
        int points = 0
    }
}
```

### Switch Expressions (Arrow Style)

Switch expressions return values directly using arrow syntax (`=>`).

#### Basic Syntax

```c
type variable = switch expression
{
    value1 => result1
    value2 => result2
    default => default_result
}
```

#### Examples

```c
// Simple switch expression
int result = switch 2
{
    1 => 10
    2 => 20
    default => 30
}
// result is now 20

// Character to string mapping
char letter = 'a'
string phonetic = switch letter
{
    'a' => "Alpha"
    'b' => "Beta"
    'c' => "Charlie"
    default => "Unknown"
}
// phonetic is now "Alpha"
```

#### Multiple Values in Expression Cases

```c
// Multiple values with arrow syntax
int value = 3
int output = switch value
{
    1, 2 => 10        // Multiple values for same result
    3 => 30
    4, 5, 6 => 100
    default => -1
}
// output is now 30
```

#### Complex Expressions

```c
// Switch expressions can be used anywhere expressions are allowed
int calculateBonus(int performance)
{
    return switch performance
    {
        5 => 1000
        4 => 750
        3 => 500
        2 => 250
        default => 0
    }
}

// In variable declarations
string status = switch getErrorCode()
{
    0 => "Success"
    1, 2 => "Warning"
    default => "Error"
}

// As function arguments
print(switch day
{
    1 => "Monday"
    2 => "Tuesday"  
    3 => "Wednesday"
    default => "Other"
})
```

### Nested Switch Statements

Switch statements can be nested within other control flow constructs:

```c
int category = 1
int subtype = 2

switch category
{
    case 1
    {
        switch subtype
        {
            case 1
            {
                string result = "Category 1, Type A"
            }
            case 2  
            {
                string result = "Category 1, Type B"
            }
            default
            {
                string result = "Category 1, Unknown Type"
            }
        }
    }
    
    case 2
    {
        string result = "Category 2"
    }
    
    default
    {
        string result = "Unknown Category"
    }
}
```

### Mixed Switch Types

You can mix switch statements and expressions:

```c
int mode = 1
string message = ""

switch mode
{
    case 1
    {
        // Switch statement with nested switch expression
        int level = 2
        message = switch level
        {
            1 => "Beginner"
            2 => "Intermediate"
            3 => "Advanced"
            default => "Unknown"
        }
    }
    
    case 2
    {
        message = "Special mode"
    }
    
    default
    {
        message = "Default mode"
    }
}
```

### Type Requirements

- Switch expressions must have consistent return types for all cases
- The switch condition can be any integer or character type
- Case values must be compile-time constants
- Case values must match the type of the switch expression

### Performance Notes

- Switch statements generate optimized LLVM `switch` instructions
- Switch expressions use PHI nodes for efficient value merging
- Multiple case values are handled efficiently without duplicating code
- Large switch statements with sequential values may be optimized to jump tables

### Important Differences

**Switch Statements vs Switch Expressions:**

| Feature | Statements | Expressions |
|---------|------------|-------------|
| Syntax | `case value { ... }` | `value => result` |
| Return Value | None (void) | Returns a value |
| Usage | Standalone | In expressions/assignments |
| Body | Block of statements | Single expression |
| Multiple Cases | `case 1, 2 { ... }` | `1, 2 => result` |

### Error Cases

```c
// This will fail - inconsistent types in switch expression
int bad = switch 1
{
    1 => 10        // int
    2 => "hello"   // string - ERROR!
    default => 0
}

// This will fail - missing default in switch expression
int incomplete = switch x
{
    1 => 10
    2 => 20
    // ERROR: Switch expressions require default case
}
```

### Implementation Details

- Switch statements create separate basic blocks for each case
- Switch expressions merge results using LLVM PHI nodes
- Multiple case values are converted to multiple switch targets
- Default cases are optional for statements but required for expressions
- No fall-through behavior - each case is isolated
- Case values are evaluated at compile time and must be constants

## Assert Statements

Assert statements are used for debugging and testing to verify that conditions hold true at runtime. If an assertion fails, the program terminates immediately.

Assert statements can be used both inside functions and at the top level (global scope), making them useful for compile-time validation and runtime checks.

### Basic Syntax

```c
assert expression
```

The expression must evaluate to a boolean value or be convertible to boolean. If the expression evaluates to false, the program calls `abort()` and terminates.

### Scope and Usage

Assert can be used in two contexts:

1. **Inside functions**: For runtime validation within function bodies
2. **At top level**: For global constant validation and test assertions

```c
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

### Usage Examples

```c
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

### Compile-Time Constant Evaluation

Assert statements with constant expressions are evaluated at compile time. If a constant assertion would fail, the compiler reports an error:

```c
// Compile-time error - assertion always fails
assert 1 == 2          // Error: Assert condition evaluates to false at compile time
assert false           // Error: Assert condition evaluates to false at compile time

// These compile successfully as they evaluate to true
assert 10 > 5          // OK - constant expression evaluates to true
assert true && true    // OK - constant expression evaluates to true
```

### Type Conversion

Non-boolean expressions are automatically converted to boolean:
- Integer values: 0 is false, non-zero is true
- Floating-point values: 0.0 is false, non-zero is true
- Boolean values: Used directly

```c
int count = 5
assert count           // Passes (5 != 0)

float value = 0.0
assert value          // Fails (0.0 == 0)

bool active = true
assert active         // Passes
```

### Use Cases

Assert statements are commonly used for:

1. **Preconditions** - Verify function requirements:
```c
int divide(int a, int b)
{
    assert b != 0     // Ensure no division by zero
    return a / b
}
```

2. **Postconditions** - Verify function results:
```c
int abs(int x)
{
    int result = x if x >= 0 else -x
    assert result >= 0    // Result must be non-negative
    return result
}
```

3. **Invariants** - Verify loop or data structure properties:
```c
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
```c
int testAddition()
{
    assert add(2, 3) == 5
    assert add(-1, 1) == 0
    assert add(0, 0) == 0
    return 0
}
```

### Implementation Details

- Assert statements generate conditional branches in LLVM IR
- On failure, the `abort()` function is called to terminate the program
- Success branches continue normal execution
- Assert has minimal performance impact when conditions pass
- In future versions, assert statements may be optionally disabled in release builds