// Test file for break statements

// Simple break in while loop
int test_break_while()
{
    int count = 0
    while true
    {
        count++
        if count == 5
        { 
            break
        }
    }
    return count
}

// Break with multiple conditions
int test_conditional_break()
{
    int sum = 0
    int i = 0
    while i < 100
    {
        sum += i
        if sum > 20
        {
            break
        }
        i++
    }
    return sum
}

// Nested loops with break
int test_nested_break()
{
    int total = 0
    int i = 0
    while i < 10 {
        int j = 0
        while j < 10
        {
            total++
            if j == 3
            {
                break   // This only breaks inner loop
            }
            j++
        }
        i++
    }
    return total        // Should be 40 (10 outer * 4 inner iterations)
}

// Break in outer loop of nested loops
int test_break_outer()
{
    int count = 0
    int i = 0
    while i < 10
    {
        if i == 3
        {
            break
        }
        int j = 0
        while j < 5
        {
            count++
            j++
        }
        i++
    }
    return count        // Should be 15 (3 outer * 5 inner iterations)
}

// Multiple break points
int test_multiple_breaks()
{
    int result = 0
    for int i = 0 to 100
    {
        if i == 10
        {
            result = 1
            break
        }
        if i == 20
        {
            result = 2
            break
        }
        if i == 30
        {
            result = 3
            break
        }
    }
    return result       // Should be 1
}

// Break after other statements
int test_break_after_statements()
{
    int sum = 0
    int i = 0
    while i < 10
    {
        sum += i
        i++
        if i == 5
        {
            sum *= 2    // Execute this before break
            break
        }
    }
    return sum          // Should be (0+1+2+3+4)*2 = 20
}

// Main function to run tests
int main()
{
    int result1 = test_break_while()            // 5
    int result2 = test_conditional_break()      // 21
    int result3 = test_nested_break()           // 40
    int result4 = test_break_outer()            // 15
    int result5 = test_multiple_breaks()        // 1
    int result6 = test_break_after_statements() // 20
    
    // Return sum to verify: 5 + 21 + 40 + 15 + 1 + 20 = 102
    assert result1 == 5
    assert result2 == 21
    assert result3 == 40
    assert result4 == 15
    assert result5 == 1
    assert result6 == 20
    assert result1 + result2 + result3 + result4 + result5 + result6 == 102
    return result1 + result2 + result3 + result4 + result5 + result6
}