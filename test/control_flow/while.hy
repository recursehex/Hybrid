// Test file for while loops

// Simple while loop with decrementing counter
int test_countdown()
{
    int count = 5
    int sum = 0
    while count > 0
    {
        sum += count
        count--
    }
    return sum
}

// While loop with boolean condition
int test_boolean_while()
{
    bool running = true
    int iterations = 0
    
    while running
    {
        iterations++
        if iterations >= 3
        {
            running = false
        }
    }
    return iterations
}

// Nested while loops
int test_nested_while()
{
    int i = 0
    int total = 0
    while i < 3
    {
        int j = 0
        while j < 2
        {
            total = total + (i * 10 + j)
            j++
        }
        i++
    }
    return total
}

// While loop with complex condition
int test_complex_condition()
{
    int x = 10
    int y = 5
    int sum = 0
    
    while x > 0 && y > 0
    {
        sum += (x + y)
        x -= 2
        y -= 1
    }
    
    return sum
}

// While loop with early return
int test_early_return()
{
    int i = 0
    while i < 100
    {
        if i == 5
        {
            return i
        }
        i++
    }
    return -1
}

// Test infinite loop prevention
int test_while_false()
{
    int counter = 0
    while false
    {
        counter++
    }
    return counter
}

int main()
{
    assert test_countdown() == 15
    assert test_boolean_while() == 3
    assert test_nested_while() == 63
    assert test_complex_condition() == 45
    assert test_early_return() == 5
    assert test_while_false() == 0

    return 0
}