// Test modulo operator %
int x = 10
int y = 3
int z = x % y  // Should be 1

// Test with variables
int a = 17
int b = 5
int result = a % b  // Should be 2

// Test in expressions
int expr_test = (15 % 4) + (20 % 6)  // 3 + 2 = 5

// Function using modulo
int mod_func(int n, int m) {
    return n % m
}

// Test function calls
mod_func(13, 7)  // Should be 6
mod_func(25, 8)  // Should be 1