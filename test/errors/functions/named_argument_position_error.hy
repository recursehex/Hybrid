// EXPECT_DIAGNOSTIC: Positional argument cannot follow a named argument
// EXPECT_OUTPUT: Positional argument cannot follow a named argument
int sum(int a, int b = 2)
{
    return a + b
}

int main()
{
    int i = sum(a = 1, 3)
    return 0
}

