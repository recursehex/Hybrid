// Test that boolean operators fail with non-bool types

int testFailWith(int x, int y)
{
    // This should fail - boolean operators with non-bool types
    if x && y
    {
        return 1
    }
    return 0
}

int testFailOr(int a, int b)
{
    // This should also fail
    if a || b
    {
        return 1
    }
    return 0
}

int testFailNot(int value)
{
    // This should fail
    if !value
    {
        return 1
    }
    return 0
}