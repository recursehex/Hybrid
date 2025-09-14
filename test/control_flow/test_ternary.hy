// Test file for ternary operator expressions (a if b else c)

// Basic ternary with integers
int x = 10 if true else 5
int y = 20 if false else 15

// Ternary with variables
int a = 42
int b = 99
int result = a if a > b else b

// Ternary with expressions
int max_val = x + 5 if x > y else y * 2

// Ternary with float promotion
double mixed = 3.14 if false else 42

// Function using ternary
int absolute(int n)
{
    return n if n >= 0 else -n
}

// Test absolute function
int pos_test = absolute(5)
int neg_test = absolute(-7)

// Nested ternary (should work with proper precedence)
int nested = 1 if true else 2 if false else 3

// Ternary with boolean expressions
bool flag = true
int conditional = 100 if flag && true else 200

// Ternary with comparison operations
int compare_result = 50 if 10 > 5 else 75

// Test in variable declarations with complex conditions
int complex_cond = 999 if x > 0 && y < 100 else 0