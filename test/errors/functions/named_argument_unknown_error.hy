// EXPECT_DIAGNOSTIC: Unknown parameter name 'c' for call to 'sum'
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// EXPECT_OUTPUT: Unknown parameter name 'c' for call to 'sum'
int sum(int a, int b = 2)
{
    return a + b
}

int main()
{
    int i = sum(a = 1, c = 3)
    return 0
}

