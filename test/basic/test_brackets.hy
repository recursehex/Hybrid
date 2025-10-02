// Single-line style
int add(int x, int y) { return x + y }

// Multi-line compact style
int multiply(int a, int b) {
    return a * b
}

// Allman style
int square(int x)
{
    return x * x
}

// Validate all bracket styles work correctly
assert add(2, 3) == 5
assert multiply(4, 5) == 20
assert square(7) == 49