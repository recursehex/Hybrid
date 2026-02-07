// EXPECT_DIAGNOSTIC: Parameters with default values must be the trailing parameters
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unknown variable name: a
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_OUTPUT: Parameters with default values must be the trailing parameters
int bad(int a = 1, int b)
{
    return a + b
}

