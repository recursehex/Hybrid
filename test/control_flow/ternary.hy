// Test file for ternary operator expressions (a if b else c)

// Basic ternary with integers
int x = 10 if true else 5
int y = 20 if false else 15
assert x == 10
assert y == 15

// Ternary with variables
int a = 42
int b = 99
int result = a if a > b else b
assert result == 99

// Ternary with expressions
int max_val = x + 5 if x > y else y * 2
assert max_val == 30

// Ternary with float promotion
double mixed = 3.14 if false else 42
assert mixed == 42

// Function using ternary
int absolute(int n)
{
    return n if n >= 0 else -n
}

// Test absolute function
int pos_test = absolute(5)
int neg_test = absolute(-7)
assert pos_test == 5
assert neg_test == 7

// Nested ternary (should work with proper precedence)
int nested = 1 if true else 2 if false else 3
assert nested == 1

// Ternary with boolean expressions
bool flag = true
int conditional = 100 if flag && true else 200
assert conditional == 100

// Ternary with comparison operations
int compare_result = 50 if 10 > 5 else 75
assert compare_result == 50

// Test in variable declarations with complex conditions
int complex_cond = 999 if x > 0 && y < 100 else 0
assert complex_cond == 999