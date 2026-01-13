delegate int Transform(int value)

int square(int x)
{
    return x * x
}

int main()
{
    Transform t = square
    int result = t(5)
    if result != 25
    {
        return 1
    }
    return 0
}
