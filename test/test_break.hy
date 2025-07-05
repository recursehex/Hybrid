// Test file for break statements

// Simple break in while loop
int test_break_while() {
    int count = 0
    while true {
        count = count + 1
        if count == 5 {
            break
        }
    }
    return count
}

// Break with multiple conditions
int test_conditional_break() {
    int sum = 0
    int i = 0
    while i < 100 {
        sum = sum + i
        if sum > 20 {
            break
        }
        i = i + 1
    }
    return sum
}

// Nested loops with break
int test_nested_break() {
    int total = 0
    int i = 0
    while i < 10 {
        int j = 0
        while j < 10 {
            total = total + 1
            if j == 3 {
                break  // This only breaks inner loop
            }
            j = j + 1
        }
        i = i + 1
    }
    return total  // Should be 40 (10 outer * 4 inner iterations)
}

// Break in outer loop of nested loops
int test_break_outer() {
    int count = 0
    int i = 0
    while i < 10 {
        if i == 3 {
            break
        }
        int j = 0
        while j < 5 {
            count = count + 1
            j = j + 1
        }
        i = i + 1
    }
    return count  // Should be 15 (3 outer * 5 inner iterations)
}

// Multiple break points
int test_multiple_breaks() {
    int result = 0
    int x = 0
    while x < 100 {
        x = x + 1
        if x == 10 {
            result = 1
            break
        }
        if x == 20 {
            result = 2
            break
        }
        if x == 30 {
            result = 3
            break
        }
    }
    return result  // Should be 1
}

// Break after other statements
int test_break_after_statements() {
    int sum = 0
    int i = 0
    while i < 10 {
        sum = sum + i
        i = i + 1
        if i == 5 {
            sum = sum * 2  // Execute this before break
            break
        }
    }
    return sum  // Should be (0+1+2+3+4)*2 = 20
}

// Main function to run tests
int main() {
    int result1 = test_break_while()         // 5
    int result2 = test_conditional_break()   // 21
    int result3 = test_nested_break()        // 40
    int result4 = test_break_outer()         // 15
    int result5 = test_multiple_breaks()     // 1
    int result6 = test_break_after_statements() // 20
    
    // Return sum to verify: 5 + 21 + 40 + 15 + 1 + 20 = 102
    return result1 + result2 + result3 + result4 + result5 + result6
}