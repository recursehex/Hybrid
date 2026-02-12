// EXPECT_DIAGNOSTIC: Invalid signature for operator '+'
struct BadOperatorSignature
{
    BadOperatorSignature()
    {
    }

    int +(const ref BadOperatorSignature rhs)
    {
        return 0
    }
}
