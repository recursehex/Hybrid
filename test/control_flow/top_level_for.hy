struct Item
{
    int value

    Item(int value)
    {
        this.value = value
    }
}

int topLevelForTo = 0
for int i = 0 to 3
{
    topLevelForTo += i
}

Item[] topItems = [Item(2), Item(4), Item(6)]
int topLevelForeach = 0
for Item item in topItems
{
    topLevelForeach += item.value
}

int sumItems(Item[] items)
{
    int total = 0
    for Item item in items
    {
        total += item.value
    }
    return total
}

int main()
{
    assert topLevelForTo == 6
    assert topLevelForeach == 12
    assert sumItems(topItems) == 12
    return 0
}
