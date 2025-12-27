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
