// EXPECT_DIAGNOSTIC: Cannot print value of type 'Plain' because it does not implement string this()
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
class Plain
{
    int value

    Plain(int value)
    {
        this.value = value
    }
}

void printPlain()
{
    Plain p = (1)
    print(p)  // expect: Cannot print value of type 'Plain' because it does not implement string this()
}
