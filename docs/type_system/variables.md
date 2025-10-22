# Variable Declarations

All variables must be initialized when declared, except for struct members given they are initialized in all constructors:

```cs
// Valid declarations
int count = 0
float rate = 0.05
bool isActive = true
string message = "Hello"
string? empty = null    // Nullable string

// Invalid - no initialization
int x       // Error: variable must be initialized

// Valid struct member constructor initialization
struct Point
{
    int x
    int y

    Point(int x, int y)
    {
        this.x = x
        this.y = y
    }
}

// Invalid struct member without constructor initialization
struct Rectangle
{
    Point topLeft
    Point bottomRight

    // Error: both members must be initialized in all constructors
    Rectangle(Point topLeft)
    {
        this.topLeft = topLeft
    }
}
```

## Nullable Types

All value and reference types are non-nullable by default. Append `?` to the type name to allow `null` assignments and propagate nullable results:

```cs
string? maybeAlias = null
int? optionalCount = parseNumber(input)
Address? address = user?.primaryAddress
```

- Nullable annotations apply anywhere a type appears: variables, struct fields, function parameters, and return types.
- `int?[]` describes an array whose elements can be `null` while the array reference itself remains non-nullable. `int[]?` flips that relationship, so the array itself can be `null` but its elements cannot.
- Pointer types (`int@`, `float@2`, etc.) implicitly allow `null` regardless of annotation because they are raw references.
- Assigning a nullable expression to a non-nullable target is a compile-time error. Use helper functions or explicit conversions that validate nullability.
- Accessing members on a nullable struct requires the null-safe access operator `?.`. See [Structs](structs.md) and [Expressions](expressions.md) for examples.
- The compiler performs flow-sensitive narrowing. After guards like `if maybeAlias != null` (and the symmetric `if maybeAlias == null` in the `else`) the guarded variable is treated as its non-nullable type for the remainder of the reachable branch. The same analysis applies to `while maybeAlias != null` loops and survives across early returns that prove a variable is null.

## Global vs Local Variables

Variables can be declared at global scope or local scope:

```cs
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