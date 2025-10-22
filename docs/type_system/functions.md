# Functions

## Overview

Functions in Hybrid use C-style syntax with explicit return types and typed parameters. Functions must declare their return type and all parameters must have explicit types. The language supports multiple function definition styles and external function declarations.

## Function Definitions

### Basic Syntax

```c
returnType functionName(type1 param1, type2 param2)
{
    // function body
    return expression
}
```

### Definition Styles

#### Single-line Functions

```c
int add(int x, int y) { return x + y }
int square(int n) { return n * n }
```

#### K&R Style

Opening brace on the same line as the function definition:

```c
int multiply(int a, int b) {
    int result = a * b
    return result
}
```

#### Allman Style (preferred)

Opening brace on the next line:

```c
int fibonacci(int n)
{
    if n <= 1 { return n }
    return fibonacci(n - 1) + fibonacci(n - 2)
}
```

## Parameters

### Parameter Declaration

Parameters require both type and name:

```c
float divide(float numerator, float denominator)
{
    return numerator / denominator
}
```

### No Parameters

Functions with no parameters use empty parentheses:

```c
int getAnswer()
{
    return 42
}
```

### Multiline Parameter Lists

Complex signatures often read better when each parameter sits on its own line. Hybrid accepts newline-separated parameter lists as long as the closing parenthesis sits on a new line by itself:

```c
int transform(
    int x,
    int y,
    int scale
)
{
    return scale * (x + y)
}
```

Use the same layout when calling the function; indent the arguments underneath the opening parenthesis and close the call on a fresh line:

```c
int result = transform(
    source.x,
    source.y,
    computeScale()
)
```

### Array Parameters

Arrays can be passed as parameters by reference:

```c
int sumArray(int[] arr)
{
    return arr[0] + arr[1] + arr[2]
}

float average(float[] values)
{
    float sum = values[0] + values[1] + values[2]
    return sum / 3.0
}
```

## Return Types

### Supported Return Types

All built-in types can be used as return types:

```c
int returnInt() { return 42 }
float returnFloat() { return 3.14 }
double returnDouble() { return 2.71828 }
char returnChar() { return 'A' }
bool returnBool() { return true }
string returnString() { return "Hello" }
```

### Void Functions

Functions that don't return a value use `void`:

```c
void printValue(int x)
{
    // Do something with x
    return  // Empty return for void functions
}

void noReturn()
{
    // Implicit return at end of void function
}
```

### Array Return Types

Currently, functions cannot return arrays directly. Arrays must be passed as parameters and modified in place.

## Return Statements

### Basic Return

```c
int getValue()
{
    return 10
}
```

### Empty Return

Void functions use empty return statements:

```c
void doSomething()
{
    return  // No value
}
```

### Conditional Returns

Functions can have multiple return points:

```c
int absolute(int x)
{
    if x < 0 {
        return -x
    }
    return x
}
```

### Type Casting on Return

Valid return values are automatically cast to match the function's declared return type:

```c
double mixedArithmetic(int x)
{
    return x / 2  // int is implicitly cast to double
}
```

## External Function Declarations

External functions can be declared for linking with C libraries:

### Syntax

```c
extern returnType functionName(type1 param1, type2 param2)
```

### Examples

```c
// Standard C library functions
extern int printf(char format, int value)
extern int getchar()
extern int puts(char str)

// Math functions
extern double sin(double x)
extern double cos(double x)
extern double sqrt(double x)
```

### Using External Functions

Once declared, external functions can be called like regular functions:

```c
extern int printf(char format, int value)

void printNumber(int n)
{
    printf("%d\n", n)
    return
}
```

## Function Calls

### Basic Calls

```c
int result = add(5, 3)
float avg = average(temps)
```

### Nested Calls

Function calls can be nested:

```c
int result = add(multiply(2, 3), square(4))
```

### Expression Context

Functions can be called as expressions or statements:

```c
// As expression
int x = getValue()

// As statement
printValue(42)
```

### Type Promotion in Arguments

Arguments are automatically promoted when compatible:

```c
double sqrt(double x) { /* ... */ }

int n = 16
double root = sqrt(n)  // n is promoted to double
```

## Scope and Visibility

### Function Scope

Functions are visible from their point of declaration:

```c
// Forward declaration not required
int factorial(int n)
{
    if n <= 1 { return 1 }
    return n * factorial(n - 1)  // Recursive call OK
}
```

### Local Variables

Variables declared within functions are local to that function:

```c
int example()
{
    int local = 10  // Only visible within example()
    return local
}
```

### Accessing Global Variables

Functions can access global variables:

```c
int globalCounter = 0

int incrementCounter()
{
    globalCounter = globalCounter + 1
    return globalCounter
}
```