// Test that chained assignment is not allowed

int main()
{
    int a = 0
    int b = 0
    int c = 0

    // This should fail - chained assignment not allowed
    a = b = c = 10

    return 0
}