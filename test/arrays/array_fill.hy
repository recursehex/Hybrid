// Array fill shorthand should replicate the initializer across the declared bounds

int[3] global_fill = 2
int[2, 3] multi_fill = 4

int verify_multi()
{
    assert multi_fill[0, 0] == 4
    assert multi_fill[1, 2] == 4
    multi_fill[1, 1] = 8
    return multi_fill[1, 1]
}

int main()
{
    assert global_fill[0] == 2
    assert global_fill[1] == 2
    assert global_fill[2] == 2

    int[4] local_fill = 7
    assert local_fill[0] == 7
    assert local_fill[3] == 7

    local_fill[1] = 3
    assert local_fill[2] == 7

    int sum = local_fill[0] + local_fill[1] + local_fill[2] + local_fill[3]
    assert sum == 24

    int[3] sized_literal = [1, 2, 3]
    assert sized_literal[0] == 1
    assert sized_literal[1] == 2
    assert sized_literal[2] == 3

    assert verify_multi() == 8
    return 0
}