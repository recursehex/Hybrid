// EXPECT_OUTPUT: Duplicate argument for parameter 'a'
int sum(int a, int b = 2)
{
    return a + b
}

int main()
{
    int i = sum(a = 1, a = 2)
    return 0
}

