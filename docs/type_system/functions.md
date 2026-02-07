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

### Default Parameters

Hybrid supports default values on trailing parameters for free functions, methods, and constructors:

```c
int log(string message, bool writeToFile = true, bool append = true)
{
    // ...
}
```

- Defaults must be compile-time evaluable (literals, simple arithmetic, enum values, address-of globals) and side-effect free.
- Constant-foldable arithmetic may include bitwise and shift operators as long as the expression contains no side effects.
- Once a parameter declares a default, all parameters to its right must also declare defaults.
- Variadic parameters cannot have defaults. `ref` parameters may when the expression is const-evaluable; null defaults are only valid when the ref target type itself permits null.
- Defaults are injected at call sites; function signatures and mangling do not change.

### Variadic Parameters (`params`)

Use `params` on the final parameter to accept a variable number of trailing arguments:

```cs
int sum(params int[] values)
{
    int total = 0
    for int v in values
    {
        total += v
    }
    return total
}
```

- Exactly one `params` parameter is allowed and it must be the final parameter.
- The parameter type must be a single-dimensional array (e.g. `int[]`), and `params` cannot be combined with `ref` or default values.
- Calls may supply zero or more trailing arguments; they are packed into an array of the element type.
- Supplying a single argument of the exact array type forwards it directly without repacking.

### Named Arguments

Calls may supply arguments by name, which is required when skipping earlier defaults:

```c
log("startup", append = false)   // skips writeToFile, overrides append
```

- Positional arguments must precede any named arguments; once named, the rest must be named.
- Named arguments can target required parameters but only parameters with defaults may be omitted.
- Duplicate or unknown names are rejected at compile time.

## Delegates

Delegates are named function pointer types with explicit signatures. Declare them wherever a type is allowed and use them in variables, parameters, and return types.

```c
delegate int Transform(int value)
```

- Delegate parameters follow the same rules as function parameters, including defaults and `params`.
- Delegate types are non-nullable by default. Append `?` to allow `null`.
- Assigning a function or method reference requires an exact signature match. Overload selection uses the delegate signature.
- Instance method references capture the receiver: `Accumulator inc = counter.Add`.
- Invoke delegates with the usual call syntax: `inc(5)`. Nullable delegates must be checked before calling.

### Delegate Scope and Placement

Delegates can appear at the top level or inside type bodies for grouping:

```cs
class Box
{
    delegate int Transformer(int value)

    int Apply(Transformer op, int value)
    {
        return op(value)
    }
}

int square(int x)
{
    return x * x
}

int main()
{
    Box b = Box()
    int result = b.Apply(square, 6)
    return result
}
```

- Delegates declared inside a type body still use a module-wide name. Refer to them by name without a type prefix.
- Delegate names must be unique within the module and cannot match the enclosing type name.
- Member modifiers (`public`, `static`, `abstract`, etc.) are not allowed on delegate declarations.
- Delegates declared inside generic types are not supported yet.

### Method References

Use instance or static method references when the signatures match:

```cs
class Counter
{
    int total

    Counter(int start)
    {
        total = start
    }

    int Add(int amount)
    {
        total += amount
        return total
    }
}

delegate int Accumulator(int amount)

int main()
{
    Counter counter = Counter(10)
    Accumulator inc = counter.Add
    return inc(5)
}
```

- Instance method references require a receiver. `Counter.Add` is not valid without an instance.

### Nullable Delegates

Nullable delegates use `?` and must be checked before calling:

```cs
delegate int Transform(int value)

int square(int x)
{
    return x * x
}

int main()
{
    Transform? op = null
    if op != null
    {
        return op(3)
    }

    op = square
    if op != null
    {
        return op(4)
    }
    return 0
}
```

### Defaults and Params

Delegates can carry default arguments and `params`:

```cs
delegate int Compute(int start, int step = 1, params int[] extras)

int sum(int start, int step = 1, params int[] extras)
{
    int total = start
    total += step
    for int value in extras
    {
        total += value
    }
    return total
}

int main()
{
    Compute op = sum
    int a = op(10)
    int b = op(10, 2, 3, 4)
    return a + b
}
```

### Style Guidance

- Prefer positional calls for the common path; reach for named arguments when skipping earlier defaults or clarifying boolean/sentinel parameters.
- Keep caller ordering aligned with parameter ordering unless skipping defaults; avoid reordering purely for style.
- Avoid mixing positional and named arguments in short calls unless it materially improves readability; once named, keep the rest named.
- For APIs with many optional flags, consider a small options struct instead of long chains of named arguments.

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

## Function Overloading

Hybrid supports function overloading for both free functions and class methods. Multiple functions may share the same name as long as their signatures differ by parameter types, `ref` qualifiers, or return type/`ref` return modifiers.

```cpp
int    distance(int x1, int y1, int x2, int y2) { /* ... */ }
double distance(double x1, double y1, double x2, double y2) { /* ... */ }

int main()
{
    int d = distance(0, 0, 3, 4)                    // picks the int overload
    double precise = distance(0.0, 0.0, 1.5, 2.0)   // resolves to the double version
    return d + int: precise
}
```

During code generation the compiler records every overload. Call sites resolve to the "best" matching overload using these rules:

- All argument counts must match exactly.
- `ref` arguments must line up with `ref` parameters.
- The compiler prefers exact type matches. If multiple overloads remain viable, it considers overloads that require implicit numeric promotions.
- Ambiguous calls (e.g. two viable overloads requiring the same number of conversions) are rejected with a compile-time error.

Extern declarations and struct constructors participate in the same mechanism, so overloading works consistently across modules and within struct method groups.

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

Functions can return arrays directly. Returned arrays are full values, so callers can index them, pass them to other functions, or store them in variables.

```c
int[] makeNumbers()
{
    return [1, 2, 3]
}

string[]? maybeNames(bool include)
{
    return ["alpha", "beta"] if include else null
}
```

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
