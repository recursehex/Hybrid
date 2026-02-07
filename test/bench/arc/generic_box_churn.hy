// ARC benchmark: generic class instantiation + reassignment churn.

class Box<T>
{
    T value

    Box(T value)
    {
        this.value = value
    }

    T Read()
    {
        return this.value
    }
}

int runGenericBoxChurn(int rounds)
{
    Box<int> slot = (0)
    int total = 0
    int i = 0

    while i < rounds
    {
        Box<int> next = (i)
        slot = next
        total += slot.Read()
        i++
    }

    return total + slot.Read()
}

int expectedGenericBoxChurn(int rounds)
{
    return (((rounds - 1) * rounds) / 2) + (rounds - 1)
}

int main()
{
    int rounds = 6000
    int actual = runGenericBoxChurn(rounds)
    int expected = expectedGenericBoxChurn(rounds)
    assert actual == expected
    return 0
}
