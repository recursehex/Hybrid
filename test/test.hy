// Clean comprehensive test without problematic patterns

// Global variables
int global_x = 100
float global_pi = 3.14159

// Function returning constant
int get_answer() {
    return 42
}

// Function with parameters and local variables
float circle_area(float radius) {
    float pi = 3.14159
    float area = pi * radius * radius
    return area
}

// Function with expression statements
int calculate(int a, int b) {
    int sum = a + b
    int product = a * b
    
    // Expression statements
    sum + product
    a * b * 2
    
    return sum
}

// Void function with empty return
void print_hello() {
    int dummy = 1
    return
}

// Main function
int main() {
    // Variable declarations
    int x = 10
    int y = 20
    float z = 5.5
    
    // Calculations
    int sum = x + y
    float area = circle_area(z)
    
    // Expression statements
    x + y
    sum * 2
    get_answer()
    
    // Function calls in expressions
    int result = calculate(x, y) + get_answer()
    
    return result
}

// Another function after main
float average(float a, float b, float c) {
    float sum = a + b + c
    float avg = sum * 0.33333
    return avg
}