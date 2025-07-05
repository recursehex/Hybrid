# Type System

## Overview

Hybrid uses a static type system with explicit type declarations. All variables must be declared with a type and initialized at the point of declaration. The compiler performs type checking at compile time and generates appropriate LLVM IR for each type.

## Built-in Types

### Primitive Types

| Type | Description | LLVM Type | Example |
|------|-------------|-----------|---------|
| `int` | 32-bit signed integer | `i32` | `int x = 42` |
| `float` | Single-precision floating point | `float` | `float pi = 3.14f` |
| `double` | Double-precision floating point | `double` | `double e = 2.71828` |
| `char` | 8-bit character | `i8` | `char ch = 'A'` |
| `bool` | Boolean value | `i1` | `bool flag = true` |
| `void` | No value (functions only) | `void` | `void func() { }` |
| `string` | String type | `ptr` | `string s = "hello"` |

### Array Types

Arrays are declared using the `[]` syntax after the element type:

```c
int[] numbers = [1, 2, 3, 4, 5]
float[] temperatures = [98.6, 99.1, 97.5]
char[] vowels = ['a', 'e', 'i', 'o', 'u']
bool[] flags = [true, false, true]
string[] names = ["Alice", "Bob", "Charlie"]
```

Arrays are implemented as pointers to their elements in LLVM IR.

## Variable Declarations

All variables must be initialized when declared:

```c
// Valid declarations
int count = 0
float rate = 0.05
bool isActive = true
string message = "Hello"
string empty = null  // Special case for strings

// Invalid - no initialization
int x              // Error: variable must be initialized
float y            // Error: variable must be initialized
```

## Type Inference

While types must be explicitly declared, the compiler performs automatic type inference for literals:

- Whole numbers without decimal points become `int` (i32)
- Numbers with decimal points become `double`
- Character literals in single quotes become `char`
- String literals in double quotes become `string`
- `true` and `false` become `bool`

## Type Casting and Promotion

The compiler automatically promotes types in binary operations:

```c
int x = 5
double y = 2.5
// x is promoted to double for the addition
double result = x + y  // Result is 7.5
```

### Promotion Rules

1. In binary operations between `int` and `double`, the `int` is promoted to `double`
2. Function arguments are automatically cast to match parameter types when compatible
3. Return values are cast to match the function's declared return type

## Function Types

Functions have types determined by their return type and parameter types:

```c
// Function type: (int, int) -> int
int add(int x, int y) { return x + y }

// Function type: (double) -> double
double square(double x) { return x * x }

// Function type: (int[]) -> int
int sum(int[] arr) { return arr[0] + arr[1] }

// Function type: () -> void
void printHello() { return }
```

## Type Safety

The type system enforces several safety rules:

1. **No implicit narrowing conversions**: Cannot assign `double` to `int` without explicit cast
2. **Array bounds**: Array indices must be integers
3. **Function calls**: Arguments must match parameter types (with automatic promotion)
4. **Null safety**: Only strings can be initialized with `null`

## LLVM Type Mapping

Hybrid types map directly to LLVM types:

| Hybrid Type | LLVM IR Type | Notes |
|-------------|--------------|-------|
| `int` | `i32` | 32-bit signed integer |
| `float` | `float` | 32-bit IEEE float |
| `double` | `double` | 64-bit IEEE float |
| `char` | `i8` | 8-bit integer |
| `bool` | `i1` | 1-bit integer |
| `void` | `void` | No value |
| `string` | `ptr` | Pointer to i8 |
| `T[]` | `ptr` | Pointer to T |

## Global vs Local Variables

Variables can be declared at global scope or local scope:

```c
// Global variables - visible to all functions
int globalCount = 0
string appName = "MyApp"

int useGlobal() {
    return globalCount  // Can access global variables
}

int localExample() {
    // Local variables - only visible within function
    int localVar = 10
    return localVar
}
```

Global variables are stored as LLVM global variables and persist for the program's lifetime.