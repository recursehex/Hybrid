delegate int Offset(int value, int delta = 1)
delegate int Sum(params int[] values)

int addOffset(int value, int delta = 1)
{
    return value + delta
}

int sum(params int[] values)
{
    int total = 0
    for int v in values
    {
        total += v
    }
    return total
}

int main()
{
    Offset off = addOffset
    int a = off(5)
    int b = off(5, delta = 3)

    Sum s = sum
    int total = s(1, 2, 3, 4)

    if a != 6
    {
        return 1
    }
    if b != 8
    {
        return 2
    }
    if total != 10
    {
        return 3
    }
    return 0
}
