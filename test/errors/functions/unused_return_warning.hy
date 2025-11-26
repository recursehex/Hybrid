// EXPECT_OUTPUT: Result of call to 'ComputeValue' is unused
// EXPECT_OUTPUT: Result of call to 'GetValue' is unused
// EXPECT_OUTPUT: Assign the return value to a variable if you meant to use it.

int ComputeValue()
{
    return 42
}

class Box
{
    int stored

    Box()
    {
        this.stored = 7
    }

    int GetValue()
    {
        return this.stored
    }
}

int main()
{
    ComputeValue()
    Box box = Box()
    box.GetValue()
    return 0
}