// Properties and indexers on classes.

class Counter
{
    static int total = 0
    {
        get
        set
    }

    int value
    {
        get
        set
    }

    Counter(int value)
    {
        this.value = value
        Counter.total += 1
    }

    int Current()
    {
        return this.value
    }
}

class IntList
{
    private int[] items

    int this[int index]
    {
        get this.items[index]
        set this.items[index] = value
    }

    IntList()
    {
        this.items = [0, 0, 0]
    }

    void Bump(int index, int delta)
    {
        this[index] += delta
    }
}

int main()
{
    Counter.total = 0
    Counter counter = Counter(10)
    counter.value += 2

    IntList list = IntList()
    list[0] = 4
    list[0] += 3
    list.Bump(0, 1)

    int result = counter.Current() + list[0] + Counter.total
    assert result == 21
    return 0
}
