class GetOnlyList
{
    private int[] items

    int this[int index]
    {
        get items[index]
    }

    GetOnlyList()
    {
        this.items = [1, 2, 3]
    }
}

int main()
{
    GetOnlyList list = GetOnlyList()
    int value = list[0]
    assert value == 1
    return 0
}
