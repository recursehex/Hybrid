// EXPECT_DIAGNOSTIC: Duplicate overload for operator '+'
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unknown variable name: rhs
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unknown variable name: rhs
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
struct DuplicateOperator
{
    DuplicateOperator()
    {
    }

    DuplicateOperator +(const ref DuplicateOperator rhs)
    {
        return this
    }

    DuplicateOperator +(const ref DuplicateOperator rhs)
    {
        return rhs
    }
}
