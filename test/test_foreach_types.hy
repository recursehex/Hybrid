// Test foreach loops with different array types

// Test with int array
int test_int_foreach() {
    int sum = 0
    for int x in [5, 10, 15, 20, 25] {
        sum = sum + x
    }
    return sum  // Should return 75
}

// Test with float array
float test_float_foreach() {
    float sum = 0.0
    for float f in [1.1, 2.2, 3.3, 4.4] {
        sum = sum + f
    }
    return sum  // Should return 11.0
}

// Test with nested foreach and different types
int test_nested_mixed() {
    int result = 0
    for int i in [1, 2, 3] {
        for int j in [10, 20] {
            result = result + (i * j)
        }
    }
    return result  // Should return 120
}

// Test with break in foreach
int test_foreach_break() {
    int count = 0
    for int n in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] {
        if n > 5 {
            break
        }
        count = count + 1
    }
    return count  // Should return 5
}

// Execute tests
test_int_foreach()
test_float_foreach()
test_nested_mixed()
test_foreach_break()