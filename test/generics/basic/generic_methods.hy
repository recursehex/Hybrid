// Runtime test for generic instance and static methods

class Box<T>
{
    T value

    Box(T value)
    {
        this.value = value
    }

    void SwapWith<U>(ref U slot, U replacement)
    {
        slot = replacement
    }

    void CopyValue<U>(ref U slot, U value)
    {
        slot = value
    }
}

void main()
{
    Box<int> numbers = Box<int>(1)

    int observed = 0
    numbers.SwapWith<int>(ref observed, 99)
    assert observed == 99

    string message = ""
    numbers.CopyValue<string>(ref message, "hello")
    assert message == "hello"
}