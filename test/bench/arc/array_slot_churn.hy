// ARC benchmark: array overwrite churn with ARC-managed class elements.

class Cell
{
    int value

    Cell(int value)
    {
        this.value = value
    }
}

int runArraySlotChurn(int rounds)
{
    Cell[] slots = new[32]

    int i = 0
    while i < 32
    {
        slots[i] = (i)
        i++
    }

    int step = 0
    int total = 0
    while step < rounds
    {
        int slot = step % 32
        slots[slot] = (step + 5)
        total += slots[slot].value
        step++
    }

    return total + slots[0].value
}

int expectedArraySlotChurn(int rounds)
{
    int sum = (rounds * 5) + (((rounds - 1) * rounds) / 2)
    int lastSlotZero = ((rounds - 1) / 32) * 32
    int finalSlotZero = lastSlotZero + 5
    return sum + finalSlotZero
}

int main()
{
    int rounds = 8000
    int actual = runArraySlotChurn(rounds)
    int expected = expectedArraySlotChurn(rounds)
    assert actual == expected
    return 0
}
