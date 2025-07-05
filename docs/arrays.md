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
- Whole numbers → `int[]`
- Decimal numbers → `float[]` or `double[]`
- Characters in single quotes → `char[]`
- Booleans → `bool[]`
- Strings in double quotes → `string[]`

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

## Arrays in Functions

### Array Parameters

Arrays can be passed as function parameters:

```c
int sum(int[] arr) {
    return arr[0] + arr[1] + arr[2]
}

float average(float[] values) {
    float total = values[0] + values[1] + values[2]
    return total / 3.0
}
```

### Using Arrays in Function Bodies

```c
int findMax(int[] numbers) {
    int max = numbers[0]
    int i = 1
    while i < 5 {
        if numbers[i] > max {
            max = numbers[i]
        }
        i = i + 1
    }
    return max
}
```

### Local Array Variables

Arrays can be declared as local variables within functions:

```c
int processData() {
    int[] local = [1, 2, 3, 4, 5]
    int sum = 0
    int i = 0
    while i < 5 {
        sum = sum + local[i]
        i = i + 1
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

int getGlobalScore(int index) {
    return globalScores[index]
}
```

## Implementation Details

### Memory Representation

Arrays are implemented as pointers to their first element:
- `int[]` → `ptr` to `i32`
- `float[]` → `ptr` to `float`
- `char[]` → `ptr` to `i8`

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
int sumArray(int[] nums) {
    int total = 0
    int i = 0
    while i < 5 {
        total = total + nums[i]
        i = i + 1
    }
    return total
}

int main() {
    int[] values = [10, 20, 30, 40, 50]
    return sumArray(values)  // Returns 150
}
```

### Array Manipulation

```c
void doubleElements(int[] arr) {
    int i = 0
    while i < 4 {
        arr[i] = arr[i] * 2
        i = i + 1
    }
    return
}

int example() {
    int[] nums = [1, 2, 3, 4]
    doubleElements(nums)
    // nums is now [2, 4, 6, 8]
    return nums[0]
}
```