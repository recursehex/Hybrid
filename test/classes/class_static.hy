class Counter
{
    static int active
    int id

    Counter(int id)
    {
        this.id = id
        this.active = this.active + 1
    }

    void Dispose()
    {
        this.active = this.active - 1
    }

    int ActiveCount()
    {
        return this.active
    }
}

Counter first = Counter(1)
Counter second = Counter(2)
assert first.ActiveCount() == 2
assert second.ActiveCount() == 2

second.Dispose()
assert first.ActiveCount() == 1

Counter third = Counter(3)
assert third.ActiveCount() == 2