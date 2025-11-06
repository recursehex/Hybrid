// Expect error when calling generic function without explicit type arguments

void Assign<T>(ref T target, T value)
{
    target = value
}

void main()
{
    int number = 0
    Assign(ref number, 7)
}