// Test basic function definition
int add(int a, int b)
{
    return a + b
}

// Test function with local variables
int compute(int x)
{
    int temp = x * 2
    int result = temp + 10
    return result
}

// Test void function
void doNothing()
{
    return
}

// Test floating point
double circle_area(double radius)
{
    double pi = 3.14159
    return pi * radius * radius
}

// Test boolean
bool isPositive(int num)
{
    return num > 0
}

// Test top-level variable declarations
int global_x = 42
double global_pi = 3.14159

// Test expression statements
add(5, 3)
compute(10)

// Test function calls assigned to variables
int x = add(1, 2)
double y = circle_area(5)

// Main function to test all functions with assertions
int main()
{
    // Test basic addition
    int sum_result = add(5, 3)
    assert sum_result == 8

    // Test function with local variables
    int compute_result = compute(10)
    assert compute_result == 30  // (10 * 2) + 10 = 30

    // Test floating point calculation
    double area = circle_area(2.0)
    assert int: area == 12  // pi * 2^2 = 3.14159 * 4 ≈ 12.56, truncated to 12

    // Test boolean function
    bool positive_test = isPositive(5)
    bool negative_test = isPositive(-3)
    bool zero_test = isPositive(0)

    assert positive_test == true
    assert negative_test == false
    assert zero_test == false

    // Test global variable access
    assert global_x == 42
    assert int: global_pi == 3

    // Test assigned function results
    assert x == 3  // add(1, 2)
    assert int: y == 78  // circle_area(5) ≈ 78.54, truncated to 78

    return 0
}