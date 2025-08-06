// Test foreach loops with runtime array size tracking

// Test 1: Foreach with array variable
int test_array_var() {
    int[] numbers = [10, 20, 30, 40, 50]
    int sum = 0
    
    // This should now work with runtime size tracking
    for int n in numbers {
        sum = sum + n
    }
    
    return sum  // Should return 150
}

// Test 2: Array passed to function
int sum_array(int[] arr) {
    int total = 0
    
    // This will use the default size for now
    for int val in arr {
        total = total + val
    }
    
    return total
}

// Test 3: Multiple arrays with different sizes
int test_multiple_arrays() {
    int[] small = [1, 2, 3]
    int[] large = [10, 20, 30, 40, 50, 60]
    int result = 0
    
    for int x in small {
        result = result + x
    }
    
    for int y in large {
        result = result + y
    }
    
    return result  // Should return 6 + 210 = 216
}

// Test array indexing with new struct format
int test_array_indexing() {
    int[] data = [100, 200, 300]
    
    // These should work with the new array struct
    int first = data[0]
    int second = data[1]
    int third = data[2]
    
    return first + second + third  // Should return 600
}

// Execute tests
test_array_var()
test_multiple_arrays()
test_array_indexing()