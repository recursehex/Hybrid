// EXPECT_DIAGNOSTIC: Type 'NumberBox' cannot be converted to string without a this() formatter
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
class NumberBox
{
    int value

    NumberBox(int value)
    {
        this.value = value
    }
}

void formatBox()
{
    NumberBox box = NumberBox(5)
    string message = $"Box: `box`"  // expect: Type 'NumberBox' cannot be converted to string without a this() formatter
}
