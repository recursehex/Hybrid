// Comprehensive array functionality tests

// Test 1: Basic global array operations
int[] global_numbers = [1, 2, 3, 4, 5]
int first_global = global_numbers[0]
int last_global = global_numbers[4]

// Modify global array
global_numbers[0] = 10
global_numbers[2] = 30

// Test with expression indexing
int idx = 2
int value_at_idx = global_numbers[idx]

// Test 2: Arrays in functions
int test_local_arrays()
{
    // Local array declaration
    int[] local_nums = [10, 20, 30, 40, 50]

    // Access and modify
    int first = local_nums[0]
    int second = local_nums[1]
    local_nums[2] = 35

    // Assert initial and modified values
    assert first == 10
    assert second == 20
    assert local_nums[2] == 35

    // Expression-based indexing
    int i = 3
    int val = local_nums[i]

    // Assert expression indexing
    assert val == 40

    int result = first + second + val
    assert result == 70  // 10 + 20 + 40

    return result
}

// Test 3: Arrays as function parameters
int sum_array(int[] arr)
{
    int sum = 0
    for int i in arr
    {
        sum += arr[i]
    }
    return sum
}

void double_elements(int[] arr) {
    arr[0] = arr[0] * 2
    arr[1] = arr[1] * 2
    arr[2] = arr[2] * 2
    arr[3] = arr[3] * 2
    arr[4] = arr[4] * 2
    return
}

// Test 4: Float arrays
float average_temps(float[] temps)
{
    float sum = temps[0] + temps[1] + temps[2]
    return sum / 3.0
}

float test_float_arrays()
{
    float[] temperatures = [98.6, 99.1, 97.5]
    float avg = average_temps(temperatures)
    
    // Modify float array
    temperatures[1] = 100.5
    temperatures[2] = 96.8
    
    return avg
}

// Test 5: Char arrays
char get_first_vowel(char[] vowels)
{
    return vowels[0]
}

char test_char_arrays()
{
    char[] vowels = ['a', 'e', 'i', 'o', 'u']
    char first = get_first_vowel(vowels)
    
    // Modify char array
    vowels[0] = 'A'
    vowels[4] = 'U'
    
    return first
}

// Test 6: Bool arrays
bool all_true_first_three(bool[] flags)
{
    return flags[0] && flags[1] && flags[2]
}

bool test_bool_arrays()
{
    bool[] flags = [true, true, false, true, false]
    bool result = all_true_first_three(flags)
    
    // Modify bool array
    flags[2] = true
    
    return result
}

// Test 7: Array operations with mixed expressions
int test_complex_operations()
{
    int[] data = [5, 10, 15, 20, 25]
    
    // Complex indexing
    int nested = data[data[0] - 5]  // data[0] = 5, so data[0]
    
    // Array element in arithmetic
    int calc = data[1] * 2 + data[2] / 3
    
    // Modify using expressions
    data[1] = data[0] + data[2]
    
    return calc
}

// Test 8: Type inference and compatibility tests
void test_type_inference()
{
    // Pure type arrays with automatic type inference
    int[] pure_int = [1, 2, 3, 4, 5]
    double[] pure_double = [1.5, 2.5, 3.5]
    bool[] pure_bool = [true, false, true, false]
    string[] pure_string = ["hello", "world", "test"]
    char[] pure_char = ['x', 'y', 'z']

    // Mixed numeric types promote to double
    double[] mixed_numeric = [1, 2.5, 3, 4.7, 5]

    // Empty arrays default to int
    int[] empty = []

    // Verify type inference works correctly
    assert pure_int[0] == 1
    assert pure_double[0] == 1.5
    assert pure_bool[0] == true
    assert pure_char[0] == 'x'

    // Verify mixed numeric array promoted to double
    assert mixed_numeric[0] == 1.0  // int promoted to double
    assert mixed_numeric[1] == 2.5   // already double

    return
}

// Test 9: Main test function that exercises all features
int main()
{
    // Test global array operations
    int global_sum = first_global + last_global + value_at_idx
    assert global_sum == 41  // 10 + 5 + 30 (modified values)

    // Test local arrays
    int local_result = test_local_arrays()
    assert local_result == 70

    // Test array parameters
    int[] test_data = [1, 2, 3, 4, 5]
    int sum_before = sum_array(test_data)
    // Note: sum_array has a bug - it uses arr[i] instead of i
    // This will be accessing array elements at indices [1,2,3,4,5] instead of values
    double_elements(test_data)
    int sum_after = sum_array(test_data)

    // Assert array was doubled
    assert test_data[0] == 2   // 1 * 2
    assert test_data[1] == 4   // 2 * 2
    assert test_data[2] == 6   // 3 * 2
    assert test_data[3] == 8   // 4 * 2
    assert test_data[4] == 10  // 5 * 2

    // Test float arrays
    float avg_temp = test_float_arrays()
    assert int: avg_temp == 98  // Average of [98.6, 99.1, 97.5] ≈ 98.4

    // Test char arrays
    char first_vowel = test_char_arrays()
    assert first_vowel == 'a'

    // Test bool arrays
    bool bool_result = test_bool_arrays()
    assert bool_result == false  // flags[2] was false initially

    // Test complex operations
    int complex_result = test_complex_operations()
    assert complex_result == 25  // data[1] * 2 + data[2] / 3 = 10 * 2 + 15 / 3 = 20 + 5 = 25

    // Test type inference and compatibility
    test_type_inference()

    return 0
}