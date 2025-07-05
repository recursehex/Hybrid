# Control Flow

## Overview

Hybrid provides three main control flow constructs: if-else statements, while loops, and foreach loops. All control flow statements use curly braces `{}` for their bodies and support nesting.

## If-Else Statements

Conditional branching using C-style if-else syntax.

### Basic Syntax

```c
if condition {
    // statements
}

if condition {
    // if body
} else {
    // else body
}
```

### If-Else-If Chains

```c
if score >= 90 {
    return 4  // A grade
} else if score >= 80 {
    return 3  // B grade
} else if score >= 70 {
    return 2  // C grade
} else {
    return 1  // F grade
}
```

### Consecutive If Statements

Multiple if statements can be used consecutively, often with early returns:

```c
int compare(int a, int b) {
    if a == b { return 0 }
    if a < b { return -1 }
    if a > b { return 1 }
    return 0
}
```

### Comparison Operators

All standard comparison operators are supported:

- `==` - Equal to
- `!=` - Not equal to
- `<` - Less than
- `>` - Greater than
- `<=` - Less than or equal to
- `>=` - Greater than or equal to

### Boolean Operators

Complex conditions can be built using boolean operators:

- `&&` - Logical AND
- `||` - Logical OR
- `!` - Logical NOT

```c
// AND operator
if x > 0 && x < 10 {
    return true
}

// OR operator
if status == 1 || status == 2 {
    return active
}

// NOT operator
if !finished {
    return false
}

// Complex conditions
if (x > 0 && y > 0) || override {
    return true
}
```

### Operator Precedence

Boolean operators have the following precedence (lowest to highest):
1. `||` (OR) - precedence 5
2. `&&` (AND) - precedence 6
3. Comparison operators - precedence 10
4. Arithmetic operators - precedence 20, 40

## While Loops

While loops repeat a block of statements as long as a condition is true.

### Basic Syntax

```c
while condition {
    // statements
}
```

### Examples

```c
// Simple countdown
int countdown(int n) {
    while n > 0 {
        n = n - 1
    }
    return n
}

// Loop with complex condition
int processItems(int count, bool active) {
    while count > 0 && active {
        count = count - 1
        if count == 5 {
            active = false
        }
    }
    return count
}
```

### Nested While Loops

```c
int nestedExample() {
    int i = 0
    while i < 3 {
        int j = 0
        while j < 2 {
            // Inner loop body
            j = j + 1
        }
        i = i + 1
    }
    return i
}
```

### Early Exit with Return

While loops can be exited early using return statements:

```c
int findFirst(int[] arr, int target) {
    int i = 0
    while i < 10 {
        if arr[i] == target {
            return i  // Early exit
        }
        i = i + 1
    }
    return -1  // Not found
}
```

## Foreach Loops

Foreach loops iterate over collections with a typed loop variable.

### Basic Syntax

```c
for type variable in collection {
    // loop body
}
```

### Examples

```c
// Iterate over an array
for int num in numbers {
    sum = sum + num
}

// Process each element
for float temp in temperatures {
    if temp > 98.6 {
        count = count + 1
    }
}
```

### Nested Foreach Loops

```c
for int i in outerList {
    for int j in innerList {
        result = result + (i * j)
    }
}
```

### Important Notes

- The loop variable is scoped to the loop body
- The loop variable type must match the collection element type
- Currently, foreach loops are parsed but code generation is pending

## Implementation Details

- All conditions are evaluated to boolean values
- Numeric values can be used as conditions (0 is false, non-zero is true)
- Block statements create new scopes for local variables
- Control flow statements can be nested to any depth
- LLVM IR uses phi nodes and basic blocks for control flow