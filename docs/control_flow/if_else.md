# If-Else Statements

If-else statements allow for conditional branching based on boolean expressions.

## Basic Syntax

```c
if condition
{
    // statements
}

if condition
{
    // if body
}
else
{
    // else body
}
```

## If-Else-If Chains

```c
if score >= 90
{
    return 4  // A grade
}
else if score >= 80
{
    return 3  // B grade
}
else if score >= 70
{
    return 2  // C grade
}
else
{
    return 1  // F grade
}
```

## Consecutive If Statements

Multiple if statements can be used consecutively, often with early returns:

```c
int compare(int a, int b)
{
    if a == b { return 0 }
    if a < b { return -1 }
    if a > b { return 1 }
    return 0
}
```