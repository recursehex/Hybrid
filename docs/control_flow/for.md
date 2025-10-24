# For and Foreach Loops

For and foreach loops allow iteration over ranges and collections.

## For Loops

For loops provide flexible iteration with initialization, condition, and step control using Hybrid's `to` syntax.

### Basic Syntax

```cpp
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

```cpp
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

```cpp
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

### Custom Steps with `by` Keyword

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

```cpp
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

```cpp
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

```cpp
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

```cpp
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

### `break` and `skip` in For Loops

Control flow statements work in for loops:

```cpp
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

```cpp
// Iterate over an array
int[] numbers = [1, 2, 3]
for int num in numbers
{
    sum += num
}

// Mutate elements in-place with ref
for ref int value in numbers
{
    value += 10
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

## Important Notes

- The loop variable is scoped to the loop body
- The loop variable type must match the collection element type
- Arrays are automatically sized based on their initialization
- The loop variable is a copy of the array element unless `ref` is used (e.g. `for ref int x in array`)

## Implementation Details

- Non-boolean conditions cannot be used and will result in a compilation error
- Block statements create new scopes for local variables
- Control flow statements can be nested to any depth
- LLVM IR uses phi nodes and basic blocks for control flow
- Foreach loops use internal counters and array indexing for iteration, storing pointers when `ref` loop variables are used so element mutations update the source array
- For loops create separate AST nodes (`ForLoopStmtAST`) from foreach loops (`ForEachStmtAST`)
- For loops automatically detect increment vs decrement based on initial and limit values
- Anonymous for loops generate internal variable names (`__anon_loop_var_N`)
- Custom conditions in for loops bypass automatic limit checking
- Float loops use LLVM floating-point operations (`fadd`, `fcmp`, etc.)
- Step operations support all arithmetic operators (`+`, `-`, `*`, `/`, `%`)
- Type conversions ensure proper matching between variable type and operand types
- Break jumps to the loop exit block, skip jumps to the increment/condition block