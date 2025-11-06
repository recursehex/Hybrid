// Runtime test for free generic functions with explicit type arguments

void AssignIdentity<T>(ref T target, T value)
{
    target = value
}

void ChooseRight<T>(ref T target, T first, T second)
{
    target = second
}

void main()
{
    int number = 0
    AssignIdentity<int>(ref number, 42)
    assert number == 42

    string word = ""
    ChooseRight<string>(ref word, "left", "right")
    assert word == "right"
}