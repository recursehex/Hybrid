// EXPECT_OUTPUT: Missing argument for parameter 'a' in call to 'sum'
int sum(int a, int b = 2)
{
    return a + b
}

int main()
{
    int i = sum(b = 3)
    return 0
}

