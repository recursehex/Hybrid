struct SumBox
{
    int value

    SumBox()
    {
        this.value = 0
    }

    SumBox(int value)
    {
        this.value = value
    }

    SumBox +(const ref SumBox rhs)
    {
        return SumBox(this.value + rhs.value)
    }
}

class LegacyIndexer
{
    private int[] items

    int this[int index]
    {
        get this.items[index]
        set this.items[index] = value
    }

    LegacyIndexer()
    {
        this.items = [0, 0, 0]
    }
}

int main()
{
    SumBox left = SumBox(8)
    SumBox right = SumBox(3)
    SumBox combined = left + right
    assert combined.value == 11

    LegacyIndexer list = LegacyIndexer()
    list[1] = 9
    list[1] += 2
    assert list[1] == 11

    int builtin = 6
    builtin *= 7
    builtin -= 2
    assert builtin == 40
    return 0
}
