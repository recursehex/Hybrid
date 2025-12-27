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
