// This is a test comment
int global_x = 5

// Another comment
float global_y = 3.14

int add(int a, int b)
{
    // Comment inside function
    return a + b
}

// Test expressions with comments in function context
int testExpressions()
{
    int x = 5
    float y = 3.14
    
    // Test expression with comment
    int result = x + 10 // inline comment
    return result
}

int main()
{
    int sum = add(5, 3)
    int expr_result = testExpressions()
    return sum + expr_result
}