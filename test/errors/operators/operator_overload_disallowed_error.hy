// EXPECT_DIAGNOSTIC: Unsupported operator overload declaration
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unknown variable name: rhs
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: 'this' can only be used inside struct methods
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
struct BadOperator
{
    BadOperator()
    {
    }

    BadOperator &(const ref BadOperator rhs)
    {
        return this
    }
}
