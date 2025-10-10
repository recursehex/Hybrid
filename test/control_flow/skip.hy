// Test skip statement in while loop
int test_skip_while() {
    int sum = 0
    int i = 0
    
    while i < 10 {
        i = i + 1
        if i == 5 {
            skip  // Skip when i is 5
        }
        sum = sum + i
    }
    
    // Should return 50 (55 - 5)
    assert sum == 50
    return sum
}

// Test skip statement in foreach loop
int test_skip_foreach() {
    int[] numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    int sum = 0
    
    for int num in numbers {
        if num == 5 {
            skip  // Skip when num is 5
        }
        sum = sum + num
    }
    
    // Should return 50 (55 - 5)
    assert sum == 50
    return sum
}

// Test nested loops with skip
int test_nested_skip() {
    int sum = 0
    int i = 0
    
    while i < 3 {
        i = i + 1
        int j = 0
        while j < 3 {
            j = j + 1
            if j == 2 {
                skip  // Skip affects inner loop only
            }
            sum = sum + j
        }
    }
    
    // Inner loop: 1 + 3 = 4 per iteration
    // 3 iterations: 4 * 3 = 12
    assert sum == 12
    return sum
}

// Test skip with multiple conditions
int test_skip_multiple() {
    int sum = 0
    int i = 0
    
    while i < 10 {
        i = i + 1
        if i == 3 || i == 5 || i == 7 {
            skip  // Skip multiple values
        }
        sum = sum + i
    }
    
    // Should return 40 (55 - 3 - 5 - 7)
    assert sum == 40
    return sum
}

// Main function to test all skip scenarios
int main() {
    int result1 = test_skip_while()
    int result2 = test_skip_foreach()
    int result3 = test_nested_skip()
    int result4 = test_skip_multiple()
    
    // Print results for verification
    print(result1)  // Should print 50
    print(result2)  // Should print 50
    print(result3)  // Should print 12
    print(result4)  // Should print 40
    assert result1 == 50
    assert result2 == 50
    assert result3 == 12
    assert result4 == 40
    
    return 0
}