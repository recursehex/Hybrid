// EXPECT_OUTPUT: Parameters with default values must be the trailing parameters
int bad(int a = 1, int b)
{
    return a + b
}

