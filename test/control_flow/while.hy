// Test file for while loops

// Simple while loop with decrementing counter
int test_countdown() {
    int count = 5
    int sum = 0
    while count > 0 {
        sum = sum + count
        count = count - 1
    }
    return sum
}

// While loop with boolean condition
int test_boolean_while() {
    bool running = true
    int iterations = 0
    
    while running {
        iterations = iterations + 1
        if iterations >= 3 {
            running = false
        }
    }
    return iterations
}

// Nested while loops
int test_nested_while() {
    int i = 0
    int total = 0
    while i < 3 {
        int j = 0
        while j < 2 {
            total = total + (i * 10 + j)
            j = j + 1
        }
        i = i + 1
    }
    return total
}

// While loop with complex condition
int test_complex_condition() {
    int x = 10
    int y = 5
    int sum = 0
    
    while x > 0 && y > 0 {
        sum = sum + (x + y)
        x = x - 2
        y = y - 1
    }
    
    return sum
}

// While loop with early return
int test_early_return() {
    int i = 0
    while i < 100 {
        if i == 5 {
            return i
        }
        i = i + 1
    }
    return -1
}

// Test infinite loop prevention
int test_while_false() {
    int counter = 0
    while false {
        counter = counter + 1
    }
    return counter
}

// Main function to run tests
int main() {
    int result1 = test_countdown()
    int result2 = test_boolean_while()
    int result3 = test_nested_while()
    int result4 = test_complex_condition()
    int result5 = test_early_return()
    int result6 = test_while_false()
    assert result1 == 15
    assert result2 == 3
    assert result3 == 63
    assert result4 == 45
    assert result5 == 5
    assert result6 == 0
    
    // Return sum of all results to verify everything worked
    return result1 + result2 + result3 + result4 + result5 + result6
}