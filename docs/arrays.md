# Arrays

## Overview

Arrays in Hybrid are fixed-size collections of elements of the same type. They are declared using the `[]` syntax after the element type and must be initialized with array literals at declaration.

## Array Declaration

### Syntax

```c
type[] array_name = [element1, element2, element3, ...]
```

### Examples

```c
// Integer array
int[] numbers = [1, 2, 3, 4, 5]

// Float array
float[] temperatures = [98.6, 99.1, 97.5]

// Character array
char[] vowels = ['a', 'e', 'i', 'o', 'u']

// Boolean array
bool[] flags = [true, false, true, false]

// String array
string[] names = ["Alice", "Bob", "Charlie"]
```

## Array Literals

Array literals are comma-separated values enclosed in square brackets:

```c
[1, 2, 3]              // Integer array literal
[1.5, 2.5, 3.5]        // Float array literal
['x', 'y', 'z']        // Character array literal
[true, false]          // Boolean array literal
["hello", "world"]     // String array literal
```

### Type Inference

Array element types are inferred from the literals:

| Literal Type | Example | Inferred Array Type |
|--------------|---------|-------------------|
| Whole numbers | `[1, 2, 3]` | `int[]` |
| Decimal numbers | `[1.5, 2.5]` | `float[]` or `double[]` |
| Characters (single quotes) | `['a', 'b']` | `char[]` |
| Booleans | `[true, false]` | `bool[]` |
| Strings (double quotes) | `["hello", "world"]` | `string[]` |

## Array Indexing

### Element Access

Array elements are accessed using zero-based indexing:

```c
int[] arr = [10, 20, 30, 40, 50]
int first = arr[0]    // 10
int third = arr[2]    // 30
int last = arr[4]     // 50
```

### Expression Indices

Array indices can be expressions:

```c
int i = 2
int value = arr[i]           // arr[2]
int next = arr[i + 1]        // arr[3]
int computed = arr[i * 2]    // arr[4]
```

### Element Assignment

Array elements can be modified using assignment:

```c
int[] scores = [85, 90, 78]
scores[0] = 95    // Change first element
scores[1] = 88    // Change second element

// Using expressions for indices
int index = 2
scores[index] = 92
```

## Nullable Arrays

Use nullable annotations to express optional arrays or optional elements:

```cs
int?[] scores = [100, null, 85]   // Elements can be null
string[]? maybeLines = readFile() // Array reference itself may be null
```

- `T?[]` allows `null` per element while keeping the array reference non-nullable.
- `T[]?` allows the array reference to be `null` while preserving non-nullable elements.
- Indexing a nullable array (`T[]?`) without first proving it non-null emits `Cannot index nullable array without null-safe operator`.
- The null-safe element access operator (`?[]`) lets you safely index nullable array references. The operator returns `null` when the receiver is `null`; otherwise it yields the element (which may itself be nullable, depending on the element type).

## Arrays in Functions

### Pass-by-Reference Semantics

Arrays are passed to functions **by reference**, not by copy. This means:

- Functions receive a pointer to the original array, not a copy
- Modifications inside the function affect the original array
- No performance penalty for passing large arrays
- Memory is shared between caller and function

```c
void modifyArray(int[] arr) {
    arr[0] = 999  // This changes the original array
}

int main() {
    int[] numbers = [1, 2, 3]
    modifyArray(numbers)
    // numbers[0] is now 999, not 1
    return numbers[0]  // Returns 999
}
```

### Array Parameters

Arrays can be passed as function parameters:

```c
int sum(int[] arr)
{
    return arr[0] + arr[1] + arr[2]
}

float average(float[] values)
{
    float total = values[0] + values[1] + values[2]
    return total / 3.0
}
```

### Using Arrays in Function Bodies

Since arrays are passed by reference, functions can both read and modify array elements:

```c
int findMax(int[] numbers)
{
    int max = numbers[0]
    int i = 1
    while i < 5
    {
        if numbers[i] > max
        {
            max = numbers[i]
        }
        i++
    }
    return max
}

// This function modifies the array in-place
void sortFirst3(int[] arr)
{
    // Simple bubble sort for first 3 elements
    if arr[0] > arr[1] {
        int temp = arr[0]
        arr[0] = arr[1]
        arr[1] = temp
    }
    if arr[1] > arr[2] {
        int temp = arr[1]
        arr[1] = arr[2]
        arr[2] = temp
    }
    if arr[0] > arr[1] {
        int temp = arr[0]
        arr[0] = arr[1]
        arr[1] = temp
    }
}
```

### Local Array Variables

Arrays can be declared as local variables within functions:

```c
int processData()
{
    int[] local = [1, 2, 3, 4, 5]
    int sum = 0
    int i = 0
    while i < 5
    {
        sum += local[i]
        i++
    }
    return sum
}
```

## Global Arrays

Arrays can be declared at global scope:

```c
// Global arrays
int[] globalScores = [100, 95, 87, 92]
float[] globalRates = [0.05, 0.10, 0.15]

int getGlobalScore(int index)
{
    return globalScores[index]
}
```

## Implementation Details

### Memory Representation

Arrays are implemented as pointers to their first element:

| Hybrid Type | LLVM Representation |
|-------------|-------------------|
| `int[]` | `ptr` to `i32` |
| `float[]` | `ptr` to `float` |
| `char[]` | `ptr` to `i16` |
| `bool[]` | `ptr` to `i8` |
| `double[]` | `ptr` to `double` |
| `string[]` | `ptr` to `ptr` |

### Stack Allocation

Array literals are allocated on the stack:

```llvm
; int[] arr = [1, 2, 3]
%arr = alloca [3 x i32], align 4
; Initialize elements
%elem0 = getelementptr [3 x i32], ptr %arr, i32 0, i32 0
store i32 1, ptr %elem0
; ... continue for other elements
```

### Array Indexing in LLVM

Array indexing uses `getelementptr` instruction:

```llvm
; arr[i]
%idx_ext = sext i32 %i to i64
%elem_ptr = getelementptr i32, ptr %arr, i64 %idx_ext
%value = load i32, ptr %elem_ptr
```

## Limitations

### Current Limitations

1. **Fixed Size**: Arrays must be initialized with literals; dynamic sizing not supported
2. **No Length Property**: Array length must be tracked separately
3. **No Bounds Checking**: Accessing out-of-bounds indices is undefined behavior
4. **No Return Type**: Functions cannot return arrays directly
5. **No Multi-dimensional**: Only single-dimensional arrays are supported

### Future Enhancements

Planned features include:
- Dynamic array allocation with `new[]`
- Array length property `size`
- Compile- and runtime bounds checking
- Multi-dimensional arrays
- Array slicing operations

## Examples

### Array Sum

```c
int sumArray(int[] nums)
{
    int total = 0
    int i = 0
    while i < 5
    {
        total += nums[i]
        i++
    }
    return total
}

int main()
{
    int[] values = [10, 20, 30, 40, 50]
    return sumArray(values)  // Returns 150
}
```

### Array Manipulation

```c
void doubleElements(int[] arr)
{
    int i = 0
    while i < 4
    {
        arr[i] *= 2
        i++
    }
    return
}

int example()
{
    int[] nums = [1, 2, 3, 4]
    doubleElements(nums)
    // nums is now [2, 4, 6, 8]
    return nums[0]
}
```
