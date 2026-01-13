// EXPECT_OUTPUT: Instance method 'Counter.add' requires an instance receiver
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
    Accumulator inc = Counter.add
    return 0
}
