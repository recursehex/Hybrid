// Expect error when supplying type arguments to a non-generic method

class Counter
{
    Counter(int start)
    {
        this.value = start
    }

    int Increment()
    {
        this.value = this.value + 1
        return this.value
    }

    int value
}

void main()
{
    Counter counter = Counter(0)
    int value = counter.Increment<int>()
}