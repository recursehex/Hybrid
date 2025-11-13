// First unit: defines Cache<T> and validates describeType() output.

class Cache<T>
{
    T latest

    Cache(T seed)
    {
        this.latest = seed
    }

    void Remember<U>(ref U slot, U candidate)
    {
        slot = candidate
    }
}

int verifyCache()
{
    Cache<int> store = Cache<int>(5)
    int slot = 0
    store.Remember<int>(ref slot, 73)

    string summary = describeType("Cache<int>")
    string expected = "type:Cache<int>|kind:class|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:T=int|genericMethodInstantiations:Cache.Remember=1"
    assert summary == expected

    return slot
}