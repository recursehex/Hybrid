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