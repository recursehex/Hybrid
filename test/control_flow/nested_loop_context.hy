// Test that nested loops correctly track loop depth

int test_nested_loops()
{
    int sum = 0

    // Outer loop
    for int i = 0 to 3
    {
        // Inner loop
        for int j = 0 to 3
        {
            if i == 1 && j == 1
            {
                skip  // Should skip in inner loop
            }

            if i == 2 && j == 2
            {
                break  // Should break out of inner loop
            }

            sum++
        }
    }

    assert sum == 13
    return sum
}

int main()
{
    int result = test_nested_loops()
    assert result == 13
    return 0
}