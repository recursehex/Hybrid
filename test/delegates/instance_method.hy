class Counter
{
    int total

    Counter(int start)
    {
        total = start
    }

    int add(int amount)
    {
        total += amount
        return total
    }
}

delegate int Accumulator(int amount)

int main()
{
    Counter c = Counter(0)
    Accumulator inc = c.add
    int first = inc(5)
    int second = inc(2)
    if first != 5
    {
        return 1
    }
    if second != 7
    {
        return 2
    }
    return 0
}
