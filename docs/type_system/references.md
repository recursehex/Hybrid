# Reference Types (ref keyword)

The `ref` keyword enables pass-by-reference semantics and variable linking, similar to C#'s `ref` and C++'s reference types (`type&`). This provides "safe pointers" for sharing variables without explicit pointer arithmetic.

## Table of Contents
- [Overview](#overview)
- [Syntax](#syntax)
- [Use Cases](#use-cases)
- [Implementation Details](#implementation-details)
- [Examples](#examples)

## Overview

References in Hybrid allow you to:
- Create aliases to existing variables (variable linking)
- Pass variables by reference to functions (avoiding copies)
- Return references from functions
- Modify the original variable through the reference

Unlike pointers, references:
- Don't require explicit dereferencing (`@` operator)
- Are always valid (no null references)
- Have automatic indirection
- Are type-safe

## Syntax

### Variable Declaration with ref

```cs
// Create a ref variable
ref int a = 10

// Link two ref variables (both point to same memory)
ref int b = a

// Link a regular variable to a ref variable
int x = ref a

// Link a regular variable to another regular variable
int y = ref x
```

### Function Parameters

```cs
// Function with ref parameter
void increment(ref int value)
{
    value++
}

// Calling function with ref
int x = 5
increment(ref x)  // x is now 6
```

### Function Return Types

```cs
// Return a ref that aliases caller-owned storage
ref int pickLarger(ref int left, ref int right)
{
    if (left > right) return ref left
    return ref right
}

int a = 5
int b = 10
ref int biggest = pickLarger(ref a, ref b)
biggest = 42  // Mutates b, because pickLarger returned ref b
```

> [!WARNING]
> Do not return a reference to a stack-local or temporary. The compiler rejects escaping refs that would dangle. Instead, return a value or store it in longer-lived storage first (e.g. a global, static, or caller-provided ref).

## Use Cases

### 1. Variable Linking

Create two variables that share the same memory location:

```cs
ref int a = 100
ref int b = a  // b is linked to a
b = 200        // Both a and b are now 200
```

Or link a regular variable:

```cs
int x = 50
int y = ref x  // y is linked to x
y = 75         // Both x and y are now 75
```

### 2. Pass-by-Reference

Modify function arguments directly:

```cs
void swap(ref int a, ref int b)
{
    int temp = a
    a = b
    b = temp
}

int x = 10
int y = 20
swap(ref x, ref y)  // x is now 20, y is now 10
```

### 3. Avoiding Copies

For large data structures (like structs), use ref to avoid expensive copies:

```cs
void processLargeStruct(ref MyStruct data)
{
    // Work with data directly, no copy made
    data.field = 42
}

MyStruct bigData = MyStruct(...)
processLargeStruct(ref bigData)
```

### 4. Multiple References to Same Data

```cs
ref int original = 10
ref int alias1 = original
ref int alias2 = original

alias1 = 20
// original, alias1, and alias2 are all 20
```

## Implementation Details

### Memory Representation

#### First ref Variable
The first ref variable in a chain is a **regular variable**:

```cs
ref int a = 1  // LLVM: @a = global i32 0
```

#### Linked ref Variables
Subsequent ref variables are **pointers**:

```cs
ref int a = 1
ref int b = a  // LLVM: @b = global ptr null
               //       store ptr @a, ptr @b
```

#### Regular Variable with ref
Regular variables initialized with `ref` become pointers:

```cs
int x = 10
int y = ref x  // LLVM: @y = global ptr null
               //       store ptr @x, ptr @y
```

### Type Tracking

The compiler tracks reference metadata with a structured `TypeInfo` record instead of string prefixes:
- `typeName` preserves the language-visible type (e.g. `int`, `MyStruct[]`).
- `refStorage` captures whether a binding owns storage (`RefValue`) or aliases another binding (`RefAlias`).
- `declaredRef` records whether the variable was declared with the `ref` keyword so the linker can validate future aliases.

Both global and local symbol tables map identifiers directly to `TypeInfo`, eliminating the former `ref_`/`refptr_` bookkeeping.

Pointer lowering uses the pointee's LLVM type, so ref parameters and aliases carry typed pointers (`ptr <ty>` in opaque-pointer IR) instead of generic `i8*` placeholders.

### Reading Through References

When reading a linked ref variable, the compiler performs **double indirection**:

```cs
int y = ref x
int z = y      // Load pointer from y, then load value from pointer
```

LLVM IR:
```llvm
%y_ptr = load ptr, ptr @y, align 8      ; Load the pointer
%y_val = load i32, ptr %y_ptr, align 4  ; Load the value
```

### Writing Through References

When assigning to a linked ref variable, the compiler stores through the pointer:

```cs
int x = ref y
x = 42         // Store to the location pointed to by x
```

LLVM IR:
```llvm
%x_ptr = load ptr, ptr @x, align 8      ; Load the pointer
store i32 42, ptr %x_ptr, align 4       ; Store through pointer
```

### Function Parameters

Ref parameters become pointer parameters in LLVM:

```cs
void func(ref int x) { ... }
```

LLVM IR:
```llvm
define void @func(ptr %x) {
  %x1 = alloca ptr, align 8
  store ptr %x, ptr %x1, align 8
  ; Access through pointer...
}
```

Function calls pass the address:
```llvm
call void @func(ptr @variable)
```

## Examples

### Example 1: Basic Linking

```cs
// Create first ref variable
ref int a = 5

// Link second ref variable to first
ref int b = a

// Modify through second ref
b = 10

// Both a and b are now 10
```

### Example 2: Swap Function

```cs
void swap(ref int a, ref int b)
{
    int temp = a
    a = b
    b = temp
}

int x = 100
int y = 200
swap(ref x, ref y)
// x is now 200, y is now 100
```

### Example 3: Counter with ref

```cs
void increment(ref int counter)
{
    counter = counter + 1
}

void decrement(ref int counter)
{
    counter = counter - 1
}

int count = 0
increment(ref count)  // count = 1
increment(ref count)  // count = 2
decrement(ref count)  // count = 1
```

### Example 4: Multiple Linked Variables

```cs
ref int shared = 100
ref int link1 = shared
ref int link2 = shared

// All three variables point to the same memory
link1 = 200
// shared, link1, and link2 are all 200

link2 = 300
// shared, link1, and link2 are all 300
```

### Example 5: Reading vs Writing

```cs
ref int a = 10
ref int b = a

// Reading through ref (gets value)
int c = b  // c = 10 (copy of value, not linked)

// Writing through ref (modifies original)
b = 20     // a and b are both 20, but c is still 10
```

## Best Practices

1. **Use ref for large structs**: Avoid expensive copies by passing structs by reference
2. **Be explicit at call site**: Always use `ref` when calling functions with ref parameters
3. **Understand linking**: Remember that ref variables can be linked to share memory
4. **Avoid confusion**: Don't mix value and reference semantics unnecessarily
5. **Document ref parameters**: Make it clear in comments when a function modifies its arguments

## Comparison with Pointers

| Feature | ref | Pointers (`@`) |
|---------|-----|----------------|
| Explicit dereferencing | No (automatic) | Yes (`@ptr`) |
| Address-of operator | No (automatic) | Yes (`#var`) |
| Safety | Safe (always valid) | Unsafe (require `unsafe` blocks) |
| Null values | No | Yes |
| Arithmetic | No | Yes (pointer arithmetic) |
| Use case | Function parameters, linking | Low-level memory operations |

## Type Safety

References are fully type-safe:

```cs
ref int x = 10
ref float y = x  // ERROR: Type mismatch

void func(ref int x) { ... }
float f = 3.14
func(ref f)  // ERROR: Type mismatch
```

## Ref Rebinding

Rebinding a `ref` variable requires the `ref` keyword on the right-hand side.
Without it, assignment writes through the existing binding.
```cs
int a = 10
int b = 20
ref int x = ref a
x = 15       // Writes to a
x = ref b    // Rebinds x to b
x = 25       // Writes to b
```

## Restrictions

1. **No ref to temporaries**: Cannot create refs to literal values
   ```cs
   ref int x = 5  // OK - creates a variable
   ref int y = ref 10  // Not allowed - 10 is not a variable
   ```

2. **Function scope**: Returning ref to local variables is disallowed
   ```cs
   ref int getRef()
   {
       int local = 42
       return ref local  // Dangerous - local goes out of scope
   }
   ```
   The compiler rejects this pattern with a clear diagnostic. Return a value copy or a reference to caller-owned/storage with a longer lifetime instead.
