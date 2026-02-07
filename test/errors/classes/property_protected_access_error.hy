// EXPECT_DIAGNOSTIC: Property 'size' cannot be declared protected because it prevents getting and setting
// EXPECT_DIAGNOSTIC: Unknown function referenced: ProtectedBox
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: 'this' can only be used inside struct methods
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_OUTPUT: Property 'size' cannot be declared protected because it prevents getting and setting
class ProtectedBox
{
    protected int size
    {
        get size
        set size = value
    }

    ProtectedBox()
    {
        this.size = 1
    }
}

int main()
{
    ProtectedBox box = ProtectedBox()
    return 0
}
