// EXPECT_DIAGNOSTIC: Variable must be initialized
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// free on smart pointer handle should be rejected

class Thing
{
    Thing() {}
}

int main()
{
    shared<Thing> handle
    free handle
    return 0
}
