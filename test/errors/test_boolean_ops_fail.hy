// Test that should fail - boolean operators with non-bool types

int testFailWith(int x, int y)
{
    if x && y
    {
        return 1
    }
    return 0
}

int testFailOr(int a, int b)
{
    if a || b
    {
        return 1
    }
    return 0
}

int testFailNot(int value)
{
    if !value
    {
        return 1
    }
    return 0
}