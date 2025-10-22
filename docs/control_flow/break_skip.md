# Break and Skip Statements

Break and skip statements are used to control `for` and `while` loop execution flow.

## Break Statements

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

### Break in Nested Loops

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

## Skip Statements

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

### Skip in Nested Loops

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

## Important Notes for Break and Skip

- `break` and `skip` can only be used inside `while` or `for` loops
- `break` immediately transfers control to the statement after the loop
- `skip` immediately transfers control to the loop's condition check (while) or increment (for)
- In nested loops, both statements only affect the innermost enclosing loop
- Using `break` or `skip` outside of a `for` or `while` loop results in a compilation error