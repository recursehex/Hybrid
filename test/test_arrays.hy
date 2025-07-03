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
int test_local_arrays() {
    // Local array declaration
    int[] local_nums = [10, 20, 30, 40, 50]
    
    // Access and modify
    int first = local_nums[0]
    int second = local_nums[1]
    local_nums[2] = 35
    
    // Expression-based indexing
    int i = 3
    int val = local_nums[i]
    
    return first + second + val
}

// Test 3: Arrays as function parameters
int sum_array(int[] arr) {
    // Manual sum since foreach isn't implemented
    return arr[0] + arr[1] + arr[2] + arr[3] + arr[4]
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
float average_temps(float[] temps) {
    float sum = temps[0] + temps[1] + temps[2]
    return sum / 3.0
}

float test_float_arrays() {
    float[] temperatures = [98.6, 99.1, 97.5]
    float avg = average_temps(temperatures)
    
    // Modify float array
    temperatures[1] = 100.5
    temperatures[2] = 96.8
    
    return avg
}

// Test 5: Char arrays
char get_first_vowel(char[] vowels) {
    return vowels[0]
}

char test_char_arrays() {
    char[] vowels = ['a', 'e', 'i', 'o', 'u']
    char first = get_first_vowel(vowels)
    
    // Modify char array
    vowels[0] = 'A'
    vowels[4] = 'U'
    
    return first
}

// Test 6: Bool arrays
bool all_true_first_three(bool[] flags) {
    return flags[0] && flags[1] && flags[2]
}

bool test_bool_arrays() {
    bool[] flags = [true, true, false, true, false]
    bool result = all_true_first_three(flags)
    
    // Modify bool array
    flags[2] = true
    
    return result
}

// Test 7: Array operations with mixed expressions
int test_complex_operations() {
    int[] data = [5, 10, 15, 20, 25]
    
    // Complex indexing
    int nested = data[data[0] - 5]  // data[0] = 5, so data[0]
    
    // Array element in arithmetic
    int calc = data[1] * 2 + data[2] / 3
    
    // Modify using expressions
    data[1] = data[0] + data[2]
    
    return calc
}

// Test 8: Main test function that exercises all features
int main() {
    // Test global array operations
    int global_sum = first_global + last_global + value_at_idx
    
    // Test local arrays
    int local_result = test_local_arrays()
    
    // Test array parameters
    int[] test_data = [1, 2, 3, 4, 5]
    int sum_before = sum_array(test_data)
    double_elements(test_data)
    int sum_after = sum_array(test_data)
    
    // Test float arrays
    float avg_temp = test_float_arrays()
    
    // Test char arrays
    char first_vowel = test_char_arrays()
    
    // Test bool arrays
    bool bool_result = test_bool_arrays()
    
    // Test complex operations
    int complex_result = test_complex_operations()
    
    // Return aggregate result
    return global_sum + local_result + sum_before + sum_after + complex_result
}