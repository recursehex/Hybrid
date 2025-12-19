int bump(ref int value = 10)
{
    value = value + 1
    return value
}

int combine(ref int left = 1, ref int right = 2)
{
    return left + right
}

int main()
{
    int provided = 5
    int first = bump()
    int second = bump(ref provided)
    int third = bump()

    assert first == 11
    assert second == 6
    assert third == 11
    assert provided == 6

    int x = 4
    int y = 7
    assert combine() == 3
    assert combine(ref x) == 6
    assert combine(ref x, ref y) == 11

    return 0
}

