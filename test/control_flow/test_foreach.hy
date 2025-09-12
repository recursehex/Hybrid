// Test foreach loops with arrays

// Test 1: Simple foreach with int array literal
int sum_array()
{
    int[] nums = [1, 2, 3, 4, 5]
    int total = 0

    for int n in nums
    {
        total = total + n
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
        sum = sum + v
        count = count + 1
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
        result = result + (x * y)
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
        sum = sum + num
    }
    
    return sum
}

sum_array()
avg_array()
nested_foreach()
foreach_with_break()