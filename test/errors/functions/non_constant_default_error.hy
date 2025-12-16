// EXPECT_OUTPUT: Default value for parameter 'value' must be a compile-time constant expression
int source()
{
    return 3
}

int bad(int value = source())
{
    return value
}

