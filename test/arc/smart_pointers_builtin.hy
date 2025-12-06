// Smart pointer helper API coverage: make_unique, make_shared, weak_from_shared, shared.weak

class Counter
{
    int id
    int value

    Counter(int identifier, int initial)
    {
        this.id = identifier
        this.value = initial
    }

    int getValue()
    {
        return this.value
    }
}

int main()
{
    // Helper builders construct payloads and wrap them in smart pointers
    unique<Counter> uniqueCounter = make_unique<Counter>(1, 10)
    unique<int> uniqueNumber = make_unique<int>(5)

    shared<Counter> owner = make_shared<Counter>(2, 20)
    shared<Counter> alias = owner
    weak<Counter> watcher = weak_from_shared<Counter>(owner)
    weak<Counter> watcher2 = owner.weak()

    shared<Counter> promoted = watcher.lock()
    shared<Counter> promoted2 = watcher2.lock()

    assert owner.use_count() == 4
    assert alias.use_count() == 4
    assert promoted.use_count() == 4
    assert promoted2.use_count() == 4
    assert (@promoted).value == 20
    assert (@owner).value == 20
    assert @uniqueNumber == 5

    // Keep locals live until the end of main to exercise teardown
    return 0
}