// EXPECT_DIAGNOSTIC: Cannot assign nullable value to non-nullable indexer on type 'NumberList'
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// EXPECT_OUTPUT: Cannot assign nullable value to non-nullable indexer on type 'NumberList'
class NumberList
{
    private int[] items

    int this[int index]
    {
        get items[index]
        set items[index] = value
    }

    NumberList()
    {
        this.items = [1, 2, 3]
    }
}

int main()
{
    NumberList list = NumberList()
    int? maybe = null
    list[0] = maybe
    return 0
}
