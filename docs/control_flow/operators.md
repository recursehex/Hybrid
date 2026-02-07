# Control Flow Operators

Comparison, equality, and boolean control flow operators are used to build conditions in `if`, `for`, `while`, and `assert` statements.

## Comparison Operators

All standard comparison operators are supported:

- `==` - Equal to
- `!=` - Not equal to
- `<` - Less than
- `>` - Greater than
- `<=` - Less than or equal to
- `>=` - Greater than or equal to

## Boolean Operators

Complex conditions can be built using boolean operators:

- `&&` - Logical AND / Conjunction
- `||` - Logical OR / Disjunction
- `!` - Logical NOT / Negation

```cpp
// AND operator
if x > 0 && x < 10
{
    return true
}

// OR operator
if status == 1 || status == 2
{
    return active
}

// NOT operator
if !finished
{
    return false
}

// Complex conditions
if (x > 0 && y > 0) || override
{
    return true
}
```

### Operator Precedence

Boolean operators have the following precedence (lowest to highest):
1. `||` (OR)
2. `&&` (AND)
3. Comparison operators (`>`, `<`, `>=`, `<=`, `==`, `!=`)
4. Arithmetic operators (`+`, `-`, `*`, `/`, `%`)

### Assignment Safety in Conditions

Assignments cannot be used as conditions in control flow statements. This prevents common bugs where assignment (`=`) is mistakenly used instead of comparison (`==`):

```cpp
// Compilation error - assignment in if condition
if x = 0 {
    print("This won't compile")
}

// Correct - use comparison for conditions
if x == 0 {
    print("This works correctly")
}

// Compilation error - assignment in while condition
while status = getStatus() {
    // This won't compile
}

// Correct - separate assignment and condition
status = getStatus()
while status != 0 {
    // Process status
    status = getStatus()
}
```

This safety feature applies to all control flow statements that use conditions (`if`, `for`, `while`, `assert`).