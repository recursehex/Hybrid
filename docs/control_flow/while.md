# While Loops

While loops repeat a block of statements as long as a condition is true.

## Basic Syntax

```c
while condition
{
    // statements
}
```

## Examples

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