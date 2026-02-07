// EXPECT_DIAGNOSTIC: Property 'count' cannot be declared private because it prevents getting and setting
// EXPECT_DIAGNOSTIC: Unknown function referenced: HiddenCounter
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: 'this' can only be used inside struct methods
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_OUTPUT: Property 'count' cannot be declared private because it prevents getting and setting
class HiddenCounter
{
    private int count
    {
        get count
        set count = value
    }

    HiddenCounter()
    {
        this.count = 0
    }
}

int main()
{
    HiddenCounter counter = HiddenCounter()
    return 0
}
