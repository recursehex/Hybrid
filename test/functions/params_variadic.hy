int sum(params int[] values)
{
    int total = 0
    for int v in values
    {
        total += v
    }
    return total
}

int sum_with_seed(int seed, params int[] values)
{
    int total = seed
    for int v in values
    {
        total += v
    }
    return total
}

int main()
{
    assert sum() == 0
    assert sum(1, 2, 3) == 6

    int[] data = [4, 5, 6]
    assert sum(data) == 15

    assert sum_with_seed(10) == 10
    assert sum_with_seed(10, 1, 2) == 13
    assert sum_with_seed(10, data) == 25

    return 0
}
