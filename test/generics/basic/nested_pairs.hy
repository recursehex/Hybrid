// Verifies nested generic structs instantiate correctly and can be stored/returned.

class Pair<T, U>
{
    T first
    U second

    Pair(T first, U second)
    {
        this.first = first
        this.second = second
    }
}

class Collector<T>
{
    T payload
    Pair<T, T> siblings

    Collector(T payload)
    {
        this.payload = payload
        this.siblings = Pair<T, T>(payload, payload)
    }

    void Sync(Pair<T, T> update)
    {
        this.siblings = update
    }
}

int SumPair(Pair<int, int> pair)
{
    return pair.first + pair.second
}

int main()
{
    Pair<int, int> firstPair = (1, 2)
    Pair<int, int> secondPair = (3, 4)

    Pair<Pair<int, int>, Pair<int, int>> nested = (firstPair, secondPair)
    assert nested.first.first == 1
    assert nested.first.second == 2
    assert nested.second.first == 3
    assert nested.second.second == 4

    Pair<Pair<Pair<int, int>, Pair<int, int>>, Pair<Pair<int, int>, Pair<int, int>>> lattice = (nested, nested)
    assert lattice.first.second.second == 4
    assert lattice.second.first.first == 1

    Collector<Pair<int, int>> collector = (Pair<int, int>(9, 10))
    collector.Sync(nested)
    assert collector.siblings.first.second == 2
    assert collector.siblings.second.first == 3

    assert SumPair(collector.siblings.first) == 3
    assert SumPair(collector.siblings.second) == 7

    return 0
}