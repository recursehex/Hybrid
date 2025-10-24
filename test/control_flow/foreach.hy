// Test foreach loops with arrays

// Test 1: Simple foreach with int array literal
int sum_array()
{
    int[] nums = [1, 2, 3, 4, 5]
    int total = 0

    for int n in nums
    {
        total += n
    }

    return total
}

// Test 2: Foreach with float array
float avg_array()
{
    float[] values = [1.5, 2.5, 3.5, 4.5]
    float sum = 0.0
    int count = 0
    
    for float v in values
    {
        sum += v
        count += 1
    }
    
    return sum / count
}

// Test 3: Nested foreach loops
int nested_foreach()
{
    int[] outer = [1, 2]
    int[] inner = [10, 20, 30]
    int result = 0
    
    for int x in outer
    {
        for int y in inner
        {
            result += x * y
        }
    }
    
    return result
}

// Test 4: Foreach with break
int foreach_with_break()
{
    int[] numbers = [10, 20, 30, 40, 50]
    int sum = 0
    
    for int num in numbers
    {
        if num > 30
        {
            break
        }
        sum += num
    }
    
    return sum
}

// Test 5: Foreach with ref variable mutating the collection
int foreach_ref_updates()
{
    int[] values = [1, 2, 3, 4]
    
    for ref int value in values
    {
        value += 10
    }
    
    int total = 0
    for int value in values
    {
        total += value
    }
    
    return total
}

assert sum_array() == 15
assert avg_array() == 3.0
assert nested_foreach() == 180
assert foreach_with_break() == 60
assert foreach_ref_updates() == 50