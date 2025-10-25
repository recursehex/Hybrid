// Test skip statement in while loop
int test_skip_while()
{
    int sum = 0
    int i = 0
    
    while i < 10
    {
        i++
        if i == 5
        {
            skip  // Skip when i is 5
        }
        sum += i
    }
    
    // Should return 50 (55 - 5)
    assert sum == 50
    return sum
}

// Test skip statement in foreach loop
int test_skip_foreach() 
{
    int[] numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    int sum = 0
    
    for int num in numbers 
    {
        if num == 5 
        {
            skip  // Skip when num is 5
        }
        sum += num
    }
    
    // Should return 50 (55 - 5)
    assert sum == 50
    return sum
}

// Test nested loops with skip
int test_nested_skip()
{
    int sum = 0
    int i = 0
    
    while i < 3
    {
        i++
        int j = 0
        while j < 3
        {
            j++
            if j == 2
            {
                skip  // Skip affects inner loop only
            }
            sum += j
        }
    }
    
    // Inner loop: 1 + 3 = 4 per iteration
    // 3 iterations: 4 * 3 = 12
    assert sum == 12
    return sum
}

// Test skip with multiple conditions
int test_skip_multiple()
{
    int sum = 0
    int i = 0
    
    while i < 10
    {
        i++
        if i == 3 || i == 5 || i == 7
        {
            skip  // Skip multiple values
        }
        sum += i
    }
    
    // Should return 40 (55 - 3 - 5 - 7)
    assert sum == 40
    return sum
}

// Main function to test all skip scenarios
int main()
{
    assert test_skip_while() == 50
    assert test_skip_foreach() == 50
    assert test_nested_skip() == 12
    assert test_skip_multiple() == 40
    
    return 0
}