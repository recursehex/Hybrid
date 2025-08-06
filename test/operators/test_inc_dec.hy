// Comprehensive test for increment and decrement operators

// Test 1: Basic prefix increment/decrement
int test_basic_prefix() {
    int x = 10
    ++x  // x = 11
    --x  // x = 10
    ++x  // x = 11
    ++x  // x = 12
    return x  // Should return 12
}

// Test 2: Prefix operators with assignment
int test_prefix_assignment() {
    int a = 5
    int b = ++a  // a = 6, b = 6 (prefix returns new value)
    int c = --a  // a = 5, c = 5
    return a + b + c  // Should return 5 + 6 + 5 = 16
}

// Test 3: Multiple variables
int test_multiple_vars() {
    int x = 1
    int y = 2
    int z = 3
    ++x  // x = 2
    ++y  // y = 3
    --z  // z = 2
    return x + y + z  // Should return 2 + 3 + 2 = 7
}

// Test 4: In expressions
int test_in_expressions() {
    int a = 10
    int b = 5
    int result = ++a + b  // a = 11, result = 11 + 5 = 16
    result = result + --b  // b = 4, result = 16 + 4 = 20
    return result  // Should return 20
}

// Test 5: With different types
float test_float_inc_dec() {
    float f = 5.5
    ++f  // f = 6.5
    --f  // f = 5.5
    ++f  // f = 6.5
    double d = 10.25
    ++d  // d = 11.25
    --d  // d = 10.25
    return f + d  // Should return 6.5 + 10.25 = 16.75
}

// Test 6: In loop conditions
int test_loop_increment() {
    int count = 0
    int i = 0
    while i < 10 {
        count = count + 1
        ++i
    }
    return count  // Should return 10
}

// Test 7: In for loops
int test_for_loop() {
    int sum = 0
    for int i in [0, 1, 2, 3, 4] {
        sum = sum + i
    }
    // Traditional C-style for loop would be:
    // for (int i = 0; i < 5; ++i) { sum += i; }
    // Currently we use foreach, but increment works in loop body
    int j = 0
    while j < 5 {
        sum = sum + j
        ++j
    }
    return sum  // Should return 10 (0+1+2+3+4) + 10 = 20
}

// Test 8: Complex expressions with parentheses
int test_complex_expressions() {
    int x = 5
    int y = 10
    int result = (++x) * 2  // x = 6, result = 6 * 2 = 12
    result = result + (--y)  // y = 9, result = 12 + 9 = 21
    return result  // Should return 21
}

// Test 9: With array indexing
int test_with_arrays() {
    int[] arr = [10, 20, 30, 40, 50]
    int i = 0
    int sum = 0
    
    sum = sum + arr[i]     // sum = 10
    ++i
    sum = sum + arr[i]     // sum = 10 + 20 = 30
    ++i
    sum = sum + arr[i]     // sum = 30 + 30 = 60
    
    return sum  // Should return 60
}

// Test 10: Chain of operations
int test_chain_operations() {
    int a = 1
    int b = 2
    int c = 3
    
    // Multiple increments in sequence
    ++a
    ++a
    ++a  // a = 4
    
    --b
    --b  // b = 0
    
    ++c
    --c
    ++c  // c = 4
    
    return a + b + c  // Should return 4 + 0 + 4 = 8
}

// Test 11: With function calls
int helper(int x) {
    return x * 2
}

int test_with_function_calls() {
    int val = 5
    int result = helper(++val)  // val = 6, result = helper(6) = 12
    return result  // Should return 12
}

// Test 12: Increment/decrement in conditional statements
int test_in_conditionals() {
    int x = 5
    int result = 0
    
    ++x  // x = 6
    if x > 5 {
        result = 10
    }
    
    --x  // x = 5
    if x == 5 {
        result = result + 5  // result = 15
    }
    
    return result  // Should return 15
}

// Note: Postfix operators (x++, x--) are not yet implemented
// When implemented, they should work like:
// int a = 5
// int b = a++  // b = 5, a = 6 (postfix returns old value)
// int c = a--  // c = 6, a = 5

// Run all tests
test_basic_prefix()
test_prefix_assignment()
test_multiple_vars()
test_in_expressions()
test_float_inc_dec()
test_loop_increment()
test_for_loop()
test_complex_expressions()
test_with_arrays()
test_chain_operations()
test_with_function_calls()
test_in_conditionals()