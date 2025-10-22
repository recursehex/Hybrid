# Switch Statements and Expressions

Switch statements provide efficient multi-way branching based on a value. Hybrid supports both block-style switch statements and arrow-syntax switch expressions.

## Switch Statements

Switch statements execute different blocks of code based on the value of an expression.

### Basic Syntax

```cpp
switch expression
{
    case value
    {
        // statements to execute
    }
    case value2
    {
        // statements to execute
    }
    default
    {
        // default case (optional but recommended)
    }
}
```

### Examples

```cpp
// Simple switch statement
int x = 2

switch x
{
    case 1
    {
        int result = 10
        print(result)
    }
    case 2
    {
        int result = 20
        print(result)
    }
    default
    {
        int result = 0
        print(result)
    }
}
```

### Multiple Values per Case

```cpp
// Multiple values in a single case
int dayOfWeek = 1

switch dayOfWeek
{
    case 1, 2, 3, 4, 5
    {
        string type = "Weekday"
        print(type)
    }
    case 6, 7
    {
        string type = "Weekend"
        print(type)
    }
    default
    {
        string type = "Invalid"
        print(type)
    }
}
```

### Character Cases

```cpp
// Switch on character values
char grade = 'B'

switch grade
{
    case 'A'
    {
        int points = 4
    }
    case 'B'
    {
        int points = 3
    }
    case 'C'
    {
        int points = 2
    }
    default
    {
        int points = 0
    }
}
```

## Switch Expressions

Switch expressions return values directly using arrow syntax (`=>`).

### Basic Syntax

```cpp
type variable = switch expression
{
    value1 => result1
    value2 => result2
    default => default_result
}
```

### Examples

```cpp
// Simple switch expression
int result = switch 2
{
    1 => 10
    2 => 20
    default => 30
}
// result is now 20

// Character to string mapping
char letter = 'a'
string phonetic = switch letter
{
    'a' => "Alpha"
    'b' => "Beta"
    'c' => "Charlie"
    default => "Unknown"
}
// phonetic is now "Alpha"
```

### Multiple Values in Expression Cases

```cpp
// Multiple values with arrow syntax
int value = 3
int output = switch value
{
    1, 2 => 10        // Multiple values for same result
    3 => 30
    4, 5, 6 => 100
    default => -1
}
// output is now 30
```

### Complex Expressions

```cpp
// Switch expressions can be used anywhere expressions are allowed
int calculateBonus(int performance)
{
    return switch performance
    {
        5 => 1000
        4 => 750
        3 => 500
        2 => 250
        default => 0
    }
}

// In variable declarations
string status = switch getErrorCode()
{
    0 => "Success"
    1, 2 => "Warning"
    default => "Error"
}

// As function arguments
print(switch day
{
    1 => "Monday"
    2 => "Tuesday"  
    3 => "Wednesday"
    default => "Other"
})
```

### Nested Switch Statements

Switch statements can be nested within other control flow constructs:

```cpp
int category = 1
int subtype = 2

switch category
{
    case 1
    {
        switch subtype
        {
            case 1
            {
                string result = "Category 1, Type A"
            }
            case 2  
            {
                string result = "Category 1, Type B"
            }
            default
            {
                string result = "Category 1, Unknown Type"
            }
        }
    }
    
    case 2
    {
        string result = "Category 2"
    }
    
    default
    {
        string result = "Unknown Category"
    }
}
```

### Mixed Switch Types

You can mix switch statements and expressions:

```cpp
int mode = 1
string message = ""

switch mode
{
    case 1
    {
        // Switch statement with nested switch expression
        int level = 2
        message = switch level
        {
            1 => "Beginner"
            2 => "Intermediate"
            3 => "Advanced"
            default => "Unknown"
        }
    }
    case 2
    {
        message = "Special mode"
    }
    default
    {
        message = "Default mode"
    }
}
```

## Type Requirements

- Switch expressions must have consistent return types for all cases
- The switch condition can be any integer or character type
- Case values must be compile-time constants
- Case values must match the type of the switch expression

## Performance Notes

- Switch statements generate optimized LLVM `switch` instructions
- Switch expressions use PHI nodes for efficient value merging
- Multiple case values are handled efficiently without duplicating code
- Large switch statements with sequential values may be optimized to jump tables

## Important Differences

**Switch Statements vs Switch Expressions:**

| Feature | Statements | Expressions |
|---------|------------|-------------|
| Syntax | `case value { ... }` | `value => result` |
| Return Value | None (void) | Returns a value |
| Usage | Standalone | In expressions/assignments |
| Body | Block of statements | Single expression |
| Multiple Cases | `case 1, 2 { ... }` | `1, 2 => result` |

## Error Cases

```cpp
// This will fail - inconsistent types in switch expression
int bad = switch 1
{
    1 => 10        // int
    2 => "hello"   // string - ERROR!
    default => 0
}

// This will fail - missing default in switch expression
int incomplete = switch x
{
    1 => 10
    2 => 20
    // ERROR: Switch expressions require default case
}
```

## Implementation Details

- Switch statements create separate basic blocks for each case
- Switch expressions merge results using LLVM PHI nodes
- Multiple case values are converted to multiple switch targets
- Default cases are optional for statements but required for expressions
- No fall-through behavior, each case is isolated
- Case values are evaluated at compile time and must be constants