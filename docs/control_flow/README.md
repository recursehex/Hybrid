# Control Flow

Hybrid provides six main control flow constructs: if-else statements, while loops, for loops, foreach loops, switch statements, and switch expressions. All control flow statements use curly braces `{}` for their bodies and support nesting. Loops support `break` and `skip` statements for flow control.

Additionally, Hybrid has the `assert` statement for compile and runtime checks, which throws an error if a condition is false.

Unlike most C-style languages, Hybrid does not use parentheses around conditions, similar to Swift.

## If-Else Statements

```cpp
if flag
{
    // code to execute if condition is true
}
else if time <= 0
{
    // code to execute if another condition is true
}
else
{
    // code to execute if none of the above conditions are true
}
```

## For Loops

```cpp
for int i = 0 to str.size by * 2
{
    // code to execute in each iteration
}
```

## Foreach Loops

```cpp
for char c in str
{
    // code to execute for each element
}
```

## While Loops

```cpp
while i < 10
{
    // code to execute while condition is true
}
```

## Switch Statements

```cpp
switch value
{
    case 1
    {
        // code for case 1
    }
    case 2
    {
        // code for case 2
    }
    default
    {
        // code for default case
    }
}
```

## Switch Expressions

```cpp
string result = switch letter
{
    'a' => "Alpha"
    'b' => "Bravo"
    default => "Unknown"
}
```

## Assert Statements

```cpp
assert num == 42
```