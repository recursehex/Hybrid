# Type System

## Overview

Hybrid uses a static type system with explicit type declarations. All variables must be declared with a type and initialized at the point of declaration. The compiler performs type checking at compile time and generates appropriate LLVM IR for each type.

## Built-in Types

### Primitive Types

#### Basic Types

| Type | Description | LLVM Type | Example |
|------|-------------|-----------|---------|
| `int` | 32-bit signed integer | `i32` | `int x = 42` |
| `float` | 32-bit floating point | `float` | `float pi = 3.14f` |
| `double` | 64-bit floating point | `double` | `double e = 2.71828` |
| `char` | 16-bit Unicode character | `i16` | `char ch = 'A'` |
| `bool` | 8-bit boolean value | `i8` | `bool flag = true` |
| `void` | No value (functions only) | `void` | `void func() { }` |
| `string` | String type | `ptr` | `string s = "hello"` |

#### Sized Integer Types

| Type | Description | LLVM Type | Range | Example |
|------|-------------|-----------|-------|---------|
| `byte` | 8-bit unsigned integer | `i8` | 0 to 255 | `byte b = 255` |
| `sbyte` | 8-bit signed integer | `i8` | -128 to 127 | `sbyte sb = -100` |
| `short` | 16-bit signed integer | `i16` | -32,768 to 32,767 | `short s = 1000` |
| `ushort` | 16-bit unsigned integer | `i16` | 0 to 65,535 | `ushort us = 60000` |
| `uint` | 32-bit unsigned integer | `i32` | 0 to 4,294,967,295 | `uint ui = 3000000` |
| `long` | 64-bit signed integer | `i64` | -2^63 to 2^63-1 | `long l = 1000000000` |
| `ulong` | 64-bit unsigned integer | `i64` | 0 to 2^64-1 | `ulong ul = 10000000000` |

#### Sized Character Types

| Type | Description | LLVM Type | Example |
|------|-------------|-----------|---------|
| `schar` | 8-bit signed character | `i8` | `schar sc = 'A'` |
| `lchar` | 32-bit Unicode character | `i32` | `lchar lc = '\u03a9'` |

### Array Types

Arrays are declared using the `[]` syntax after the element type:

```c
int[] numbers = [1, 2, 3, 4, 5]
float[] temperatures = [98.6, 99.1, 97.5]
char[] vowels = ['a', 'e', 'i', 'o', 'u']
bool[] flags = [true, false, true]
string[] names = ["Alice", "Bob", "Charlie"]

// Arrays of sized types
byte[] bytes = [10, 20, 30]
short[] shorts = [1000, 2000, 3000]
long[] longs = [1000000, 2000000]
```

Arrays are implemented as structs containing a pointer to elements and a size in LLVM IR.

## Variable Declarations

All variables must be initialized when declared:

```c
// Valid declarations
int count = 0
float rate = 0.05
bool isActive = true
string message = "Hello"
string empty = null  // Special case for strings

// Sized type declarations
byte b = 100
short s = 1000
long l = 1000000
uint ui = 4000000000

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
- Integer literals are range-checked and sized to fit the target type

## Type Casting and Promotion

Hybrid enforces strict type checking with limited automatic promotion:

```c
int x = 5
double y = 2.5
// x is promoted to double for the addition
double result = x + y  // Result is 7.5

// Error cases - no implicit conversion between different sized integers
short s = 100
int i = 200
int bad = s + i  // Error: cannot mix short and int

// Bool is completely isolated
bool flag = true
int num = flag  // Error: cannot convert bool to int
```

### Type Compatibility Rules

1. **Same type operations**: Operations between values of the same type are always allowed
2. **Integer to float promotion**: Integer types can be promoted to floating-point types
3. **Float precision promotion**: `float` can be promoted to `double`
4. **No integer size mixing**: Different sized integers cannot be mixed (e.g., `short + int` is an error)
5. **Bool isolation**: `bool` cannot be converted to or from any other type
6. **Literal constants**: Integer literals are automatically sized to fit the target type with range checking

### Examples

```c
// Valid operations
int i1 = 100
int i2 = 200
int sum = i1 + i2      // Same type - OK

float f = 3.14
double d = f + 2.0     // float promoted to double - OK

byte b = 255           // Literal fits in byte range - OK

// Invalid operations
short s = 100
long l = 1000
long bad = s + l       // Error: cannot mix short and long

byte b2 = 256          // Error: 256 out of range for byte
```

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

// Functions with sized types
byte processByte(byte b) { return b + 1 }
long calculateLong(long x, long y) { return x * y }
```

## Type Safety

The type system enforces several safety rules:

1. **No implicit narrowing conversions**: Cannot assign `double` to `int` without explicit cast
2. **Strict integer size checking**: Cannot mix different sized integers (e.g., `short` and `int`)
3. **Array bounds**: Array indices must be integers
4. **Function calls**: Arguments must match parameter types exactly (except for int-to-float promotion)
5. **Bool isolation**: Boolean values cannot be converted to or from other types
6. **Range checking**: Integer literals must fit in the target type's range
7. **Null safety**: Only strings can be initialized with `null`

## LLVM Type Mapping

Hybrid types map directly to LLVM types:

| Hybrid Type | LLVM IR Type | Notes |
|-------------|--------------|-------|
| `int` | `i32` | 32-bit signed integer |
| `uint` | `i32` | 32-bit unsigned integer |
| `short` | `i16` | 16-bit signed integer |
| `ushort` | `i16` | 16-bit unsigned integer |
| `long` | `i64` | 64-bit signed integer |
| `ulong` | `i64` | 64-bit unsigned integer |
| `byte` | `i8` | 8-bit unsigned integer |
| `sbyte` | `i8` | 8-bit signed integer |
| `float` | `float` | 32-bit IEEE float |
| `double` | `double` | 64-bit IEEE float |
| `char` | `i16` | 16-bit Unicode character |
| `schar` | `i8` | 8-bit signed character |
| `lchar` | `i32` | 32-bit Unicode character |
| `bool` | `i8` | 8-bit integer |
| `void` | `void` | No value |
| `string` | `ptr` | Pointer to i8 |
| `T[]` | `struct {ptr, i32}` | Array struct with pointer and size |

## Global vs Local Variables

Variables can be declared at global scope or local scope:

```c
// Global variables - visible to all functions
int globalCount = 0
string appName = "MyApp"
byte maxRetries = 3
long totalBytes = 0

int useGlobal() {
    return globalCount  // Can access global variables
}

int localExample() {
    // Local variables - only visible within function
    int localVar = 10
    short localShort = 100
    return localVar
}
```

Global variables are stored as LLVM global variables and persist for the program's lifetime.