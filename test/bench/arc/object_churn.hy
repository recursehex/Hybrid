// ARC benchmark: repeated class reassignment churn.

class Node
{
    int value

    Node(int value)
    {
        this.value = value
    }

    int read()
    {
        return this.value
    }
}

int runObjectChurn(int rounds)
{
    Node slot = (0)
    int total = 0
    int i = 0

    while i < rounds
    {
        Node next = (i + 1)
        slot = next
        total += slot.read()
        i++
    }

    return total + slot.read()
}

int expectedObjectChurn(int rounds)
{
    return ((rounds * (rounds + 1)) / 2) + rounds
}

int main()
{
    int rounds = 4000
    int actual = runObjectChurn(rounds)
    int expected = expectedObjectChurn(rounds)
    assert actual == expected
    return 0
}
