int main()
{
    (int, string) pair = (8, "hello")
    assert pair[0] == 8
    assert pair[1] == "hello"

    pair[0] = 9
    assert pair[0] == 9

    (int count, string greeting) named = (2, "hi")
    assert named.count == 2
    named.count = 5
    assert named.count == 5
    assert named.greeting == "hi"

    ((int, int), string) nested = ((1, 2), "origin")
    assert nested[0][1] == 2
    assert nested[1] == "origin"

    return 0
}
