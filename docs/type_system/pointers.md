# Pointers and Memory Safety

## Overview

Hybrid provides C++ style pointer operations with C#-inspired memory safety through the `unsafe` keyword. All pointer operations must occur within explicitly marked unsafe contexts, making it clear where memory safety guarantees are relaxed.

## Pointer Syntax

### Pointer Type Declaration

Pointers use the `@` symbol (instead of C's `*`):

```cs
int@ ptr            // Pointer to int
float@ fptr         // Pointer to float
char@ cptr          // Pointer to char
```

### Multi-level Pointers

Multi-level pointers (pointer-to-pointer) are declared with a numeric suffix:

```cs
int@2 ptrToPtr      // Pointer to pointer to int (int**)
int@3 ptr3          // Three levels of indirection (int***)
```

### Pointer Arrays

Arrays of pointers combine array and pointer syntax:

```cs
int@[] ptrArray     // Array of int pointers
float@[] fptrArr    // Array of float pointers
```

## Pointer Operators

### Address-of Operator (`#`)

Takes the address of a variable (like `&` in C++):

```cs
unsafe
{
    int x = 42
    int@ ptr = #x       // ptr now holds the address of x
}
```

### Dereference Operator (`@`)

Accesses the value at a pointer address (like `*` in C++):

```cs
unsafe
{
    int x = 42
    int@ ptr = #x
    int value = @ptr    // value = 42
    @ptr = 100          // x is now 100
}
```

### Arrow Operator (`->`)

Pointer member access for structs (syntactic sugar for `(@ptr).member`):

```cs
unsafe struct Point
{
    int x
    int y
}

unsafe
{
    Point p = Point(10, 20)
    Point@ ptr = #p
    int xVal = ptr->x       // Same as (@ptr).x
    ptr->y = 30             // Modify through pointer
}
```

## Unsafe Contexts

### Unsafe Blocks

Pointer operations must be within `unsafe` blocks:

```cs
void safeFunction()
{
    int x = 10

    int@ ptr = #x           // ERROR: Cannot use # outside unsafe

    unsafe
    {
        int@ ptr = #x       // OK: Inside unsafe block
        @ptr = 20           // OK: Can dereference here
    }

    int val = @ptr          // ERROR: Cannot use @ outside unsafe
}
```

### Unsafe Functions

Functions can be marked as unsafe, allowing pointer operations throughout:

```cs
unsafe void swap(int@ a, int@ b)
{
    int temp = @a
    @a = @b
    @b = temp
}

void test()
{
    int x = 5, y = 10

    unsafe {
        swap(#x, #y)        // Must call unsafe function from unsafe context
    }
}
```

### Unsafe Structs

Structs containing pointer fields must be declared as unsafe:

```cs
// Regular struct - cannot have pointer fields
struct SafeStruct
{
    int value
    int@ ptr      // ERROR: Pointer fields require unsafe struct
}

// Unsafe struct - can have pointer fields
unsafe struct UnsafeStruct
{
    int@ ptr
    int@[] pointerArray
    float@2 doublePtr

    UnsafeStruct(int@ p)
    {
        this.ptr = p
    }
}
```

## Practical Examples

### Example 1: Manual Memory Management

```cs
unsafe void pointerArithmetic()
{
    int[] array = [1, 2, 3, 4, 5]
    int@ ptr = #array[0]

    // Access first element
    int first = @ptr

    // Note: Hybrid doesn't currently support pointer arithmetic like C
    // Use array indexing instead
    int@ secondPtr = #array[1]
    int second = @secondPtr
}
```

### Example 2: Linked List Node

```cs
unsafe struct Node
{
    int value
    Node@ next

    Node(int val)
    {
        this.value = val
        this.next = null
    }
}

unsafe void createList()
{
    Node n1 = Node(10)
    Node n2 = Node(20)
    n1.next = #n2

    // Traverse
    Node@ current = #n1
    int val1 = current->value      // 10
    current = current->next
    int val2 = current->value      // 20
}
```

### Example 3: Swap Function

```cs
unsafe void swap(int@ a, int@ b)
{
    int temp = @a
    @a = @b
    @b = temp
}

void main()
{
    int x = 5
    int y = 10

    unsafe
    {
        swap(#x, #y)
        // Now x = 10, y = 5
    }
}
```

### Example 4: Array of Pointers

```cs
unsafe void pointerArray()
{
    int a = 1, b = 2, c = 3

    // Create array of pointers
    int@[] ptrs = [#a, #b, #c]

    // Access through array
    int sum = 0
    sum = sum + @ptrs[0]  // Add 1
    sum = sum + @ptrs[1]  // Add 2
    sum = sum + @ptrs[2]  // Add 3

    // Modify through pointers
    @ptrs[0] = 100       // a is now 100
}
```

### Example 5: Nested Unsafe Blocks

```cs
void nextedUnsafe()
{
    int x = 42

    unsafe
    {
        int@ ptr1 = #x
        // Nested unsafe blocks are allowed
        unsafe
        {
            int@2 ptr2 = #ptr1
            int value = @(@ptr2)  // Double dereference
        }
    }
}
```

## Safety Guarantees

### Compile-Time Enforcement

The compiler enforces that:
1. **No pointer operations outside unsafe contexts** - All uses of `@`, `#`, and `->` must be within an `unsafe` block or function
2. **Unsafe structs required for pointer fields** - Structs containing pointers must be declared with `unsafe struct`
3. **Type safety maintained** - Pointer types are checked (can't assign `int@` to `float@`)

### What Unsafe Means

When you use `unsafe`:
- You take responsibility for memory safety
- You must ensure pointers are valid before dereferencing
- You must avoid use-after-free and double-free bugs
- You must ensure proper aliasing rules

### Best Practices

1. **Minimize unsafe blocks** - Keep unsafe sections as small as possible
2. **Document unsafe code** - Explain why unsafe is needed and what invariants must be maintained
3. **Validate pointers** - Check pointers before dereferencing when possible
4. **Use safe abstractions** - Wrap unsafe operations in safe interfaces when possible

## Limitations

### Current Limitations

1. **No pointer arithmetic** - Unlike C/C++, you cannot yet do `ptr + 1` or `ptr++`
2. **Manual null checks** - The compiler does not insert runtime null guards for pointers; check before dereferencing
3. **Limited type tracking** - Type information may be lost in complex scenarios (e.g. pointer arrays in struct fields)
4. **No dynamic memory allocation** - No `malloc`/`free` or `new`/`delete` equivalents

### Future Enhancements

Potential future additions:
- Optional diagnostics for raw-pointer bounds errors
- Smart pointer types (like C++'s `std::unique_ptr` and `std::shared_ptr`)
- Lifetime annotations for compile-time borrow checking

## Comparison with Other Languages

| Feature | Hybrid | C/C++ | C# |
|---------|--------|-------|------|
| Pointer syntax | `int@` | `int*` | `int*` |
| Address-of | `#x` | `&x` | `&x` |
| Dereference | `@ptr` | `*ptr` | `*ptr` |
| Safety requirement | `unsafe` block | None | `unsafe` block |
| Pointer arithmetic | No (currently) | Yes | In unsafe only |
| Null pointers | Yes (`null`) | Yes (`nullptr`) | Yes (`null`) |

## Summary

Hybrid's pointer system provides:
- **Explicit unsafe marking** - Clear boundaries where memory safety is relaxed
- **Familiar syntax** - C++ style operations with different symbols
- **Type safety** - Pointer types are checked at compile time
- **Struct integration** - Pointers can be struct fields (in unsafe structs)
- **Array support** - Arrays of pointers are fully supported

The design philosophy is to make unsafe operations possible but explicit, encouraging developers to think carefully about memory safety while still allowing low-level control when needed.