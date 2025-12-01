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