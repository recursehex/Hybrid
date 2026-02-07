class Box
{
    delegate int Transformer(int value)

    Box()
    {
    }

    int apply(Transformer op, int value)
    {
        return op(value)
    }
}

int square(int x)
{
    return x * x
}

int main()
{
    Box b = Box()
    int result = b.apply(square, 6)
    if result != 36
    {
        return 1
    }
    return 0
}
