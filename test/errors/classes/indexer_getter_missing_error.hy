// EXPECT_DIAGNOSTIC: Indexer on type 'SetOnlyList' does not define a getter
// EXPECT_DIAGNOSTIC: Unknown function referenced: SetOnlyList
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: 'this' can only be used inside struct methods
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_OUTPUT: Indexer on type 'SetOnlyList' does not define a getter
class SetOnlyList
{
    private int[] items

    int this[int index]
    {
        set items[index] = value
    }

    SetOnlyList()
    {
        this.items = [1, 2, 3]
    }
}

int main()
{
    SetOnlyList list = SetOnlyList()
    int current = list[0]
    return current
}
