// Comprehensive test for increment and decrement operators

// Test 1: Basic prefix increment/decrement
int test_basic_prefix()
{
    int x = 10
    ++x         // x = 11
    --x         // x = 10
    ++x         // x = 11
    ++x         // x = 12
    return x    // Should return 12
}

// Test 2: Prefix operators with assignment
int test_prefix_assignment()
{
    int a = 5
    int b = ++a         // a = 6, b = 6 (prefix returns new value)
    int c = --a         // a = 5, c = 5
    return a + b + c    // Should return 5 + 6 + 5 = 16
}

// Test 3: Multiple variables
int test_multiple_vars()
{
    int x = 1
    int y = 2
    int z = 3
    ++x                 // x = 2
    ++y                 // y = 3
    --z                 // z = 2
    return x + y + z    // Should return 2 + 3 + 2 = 7
}

// Test 4: In expressions
int test_in_expressions()
{
    int a = 10
    int b = 5
    int result = ++a + b    // a = 11, result = 11 + 5 = 16
    result += --b           // b = 4, result = 16 + 4 = 20
    return result           // Should return 20
}

// Test 5: With different types
float test_float_inc_dec()
{
    float f = 5.5
    ++f                 // f = 6.5
    --f                 // f = 5.5
    ++f                 // f = 6.5
    double d = 10.25
    ++d                 // d = 11.25
    --d                 // d = 10.25
    return f + d        // Should return 6.5 + 10.25 = 16.75
}

// Test 6: In loop conditions
int test_loop_increment()
{
    int count = 0
    int i = 0
    while i < 10
    {
        count += 1
        ++i
    }
    return count    // Should return 10
}

// Test 7: In for loops
int test_for_loop()
{
    int sum = 0
    for int i in [0, 1, 2, 3, 4]
    {
        sum += i
    }
    // In while loops
    int j = 0
    while j < 5
    {
        sum += j
        ++j
    }
    return sum      // Should return 10 (0+1+2+3+4) + 10 = 20
}

// Test 8: Complex expressions with parentheses
int test_complex_expressions()
{
    int x = 5
    int y = 10
    int result = (++x) * 2      // x = 6, result = 6 * 2 = 12
    result += (--y)             // y = 9, result = 12 + 9 = 21
    return result               // Should return 21
}

// Test 9: With array indexing
int test_with_arrays()
{
    int[] arr = [10, 20, 30, 40, 50]
    int i = 0
    int sum = 0
    
    sum += arr[i]           // sum = 10
    ++i
    sum += arr[i]           // sum = 10 + 20 = 30
    ++i
    sum += arr[i]           // sum = 30 + 30 = 60
    
    return sum              // Should return 60
}

// Test 10: Chain of operations
int test_chain_operations()
{
    int a = 1
    int b = 2
    int c = 3
    
    // Multiple increments in sequence
    ++a
    ++a
    ++a                 // a = 4
    
    --b
    --b                 // b = 0
    
    ++c
    --c
    ++c                 // c = 4
    
    return a + b + c    // Should return 4 + 0 + 4 = 8
}

// Test 11: With function calls
int helper(int x)
{
    return x * 2
}

int test_with_function_calls()
{
    int val = 5
    int result = helper(++val)      // val = 6, result = helper(6) = 12
    return result                   // Should return 12
}

// Test 12: Increment/decrement in conditional statements
int test_in_conditionals()
{
    int x = 5
    int result = 0
    
    ++x                         // x = 6
    if x > 5
    {
        result = 10
    }
    
    --x                         // x = 5
    if x == 5
    {
        result += 5             // result = 15
    }
    
    return result               // Should return 15
}

// Test 13: Basic postfix increment/decrement
int test_basic_postfix()
{
    int x = 10
    int a = x++         // a = 10 (old value), x = 11 (after increment)
    int b = x--         // b = 11 (old value), x = 10 (after decrement)
    return a + b + x    // Should return 10 + 11 + 10 = 31
}

// Test 14: Postfix vs prefix comparison
int test_postfix_vs_prefix()
{
    int x = 5
    int y = 5
    
    int a = ++x         // a = 6, x = 6 (prefix returns new value)
    int b = y++         // b = 5, y = 6 (postfix returns old value)
    
    return a + b        // Should return 6 + 5 = 11
}

// Test 15: Postfix in expressions
int test_postfix_expressions()
{
    int x = 10
    int y = 5
    
    int result = x++ + y        // result = 10 + 5 = 15, x becomes 11
    result += x--               // result = 15 + 11 = 26, x becomes 10
    result += y++               // result = 26 + 5 = 31, y becomes 6
    
    return result               // Should return 31
}

// Test 16: Multiple postfix operations
int test_multiple_postfix()
{
    int a = 1
    int b = 2
    int c = 3
    
    int sum = a++ + b++ + c++   // sum = 1 + 2 + 3 = 6
                                // a = 2, b = 3, c = 4 after operations
    sum += a + b + c            // sum = 6 + 2 + 3 + 4 = 15
    
    return sum                  // Should return 15
}

// Test 17: Postfix with function calls
int double_value(int x)
{
    return x * 2
}

int test_postfix_function_calls()
{
    int val = 5
    int result = double_value(val++)    // Passes 5, val becomes 6
    result += val                       // result = 10 + 6 = 16
    
    return result                       // Should return 16
}

// Test 18: Postfix with arrays
int test_postfix_arrays()
{
    int[] arr = [10, 20, 30, 40]
    int i = 0
    int sum = 0
    
    sum += arr[i++]        // sum = 10, i = 1
    sum += arr[i++]        // sum = 30, i = 2
    sum += arr[i++]        // sum = 60, i = 3
    
    return sum             // Should return 60
}

// Test 19: Complex postfix expressions
int test_complex_postfix()
{
    int x = 5
    int y = 10
    
    int result = (x++) * (y--)  // result = 5 * 10 = 50
                                // x = 6, y = 9 after operations
    result += x + y     // result = 50 + 6 + 9 = 65
    
    return result               // Should return 65
}

// Test 20: Postfix with conditionals
int test_postfix_conditionals()
{
    int count = 0
    int result = 0
    
    if count++ == 0             // Uses 0, count becomes 1
    {
        result = 10
    }
    
    if count-- == 1             // Uses 1, count becomes 0
    {
        result += 5     // result = 15
    }
    
    return result + count       // Should return 15 + 0 = 15
}

// Test 21: Mixed prefix and postfix
int test_mixed_operations()
{
    int x = 5
    int y = 5
    
    int a = ++x + y++           // a = 6 + 5 = 11, x = 6, y = 6
    int b = x-- + --y           // b = 6 + 5 = 11, x = 5, y = 5
    
    return a + b                // Should return 11 + 11 = 22
}

// Test 22: Postfix with float types
float test_postfix_float()
{
    float f = 5.5
    double d = 10.25
    
    float old_f = f++           // old_f = 5.5, f = 6.5
    double old_d = d--          // old_d = 10.25, d = 9.25
    
    return old_f + old_d        // Should return 5.5 + 10.25 = 15.75
}

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
test_basic_postfix()
test_postfix_vs_prefix()
test_postfix_expressions()
test_multiple_postfix()
test_postfix_function_calls()
test_postfix_arrays()
test_complex_postfix()
test_postfix_conditionals()
test_mixed_operations()
test_postfix_float()