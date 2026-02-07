// EXPECT_DIAGNOSTIC: Boolean AND operator '&&' can only be used with bool types
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
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
