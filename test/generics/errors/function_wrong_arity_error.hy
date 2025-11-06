// Expect diagnostic when providing wrong number of type arguments

void Assign<T>(ref T target, T value)
{
    target = value
}

void main()
{
    int number = 0
    Assign<int, string>(ref number, 5)
}