// Runtime smart pointer behavior validation
// Tests reference counting, ownership transfer, and weak zeroing at runtime

class Counter
{
    int id
    int value

    Counter(int identifier, int val)
    {
        this.id = identifier
        this.value = val
    }

    int getId()
    {
        return this.id
    }

    int getValue()
    {
        return this.value
    }

    void increment()
    {
        this.value = this.value + 1
    }
}

// Test unique<T> exclusive ownership
void testUniqueOwnership()
{
    unique<Counter> owner = unique<Counter>(Counter(1, 100))
    // After scope exit, owner should be destroyed and Counter freed
}

// Test shared<T> reference counting with multiple owners
void testSharedRefCounting()
{
    shared<Counter> primary = shared<Counter>(Counter(2, 200))
    shared<Counter> secondary = primary  // Increment strong count
    shared<Counter> tertiary = secondary // Increment strong count again
    // All three share ownership; object destroyed when last one goes out of scope
}

// Test weak<T> observation without ownership
void testWeakObservation()
{
    shared<Counter> strongOwner = shared<Counter>(Counter(3, 300))
    weak<Counter> observer = weak<Counter>(strongOwner)
    // observer does not keep Counter alive
    // When strongOwner is destroyed, observer should be zeroed
}

void testSharedUseCount()
{
    shared<Counter> primary = shared<Counter>(Counter(5, 500))
    int first = primary.use_count()
    assert first == 1

    shared<Counter> secondary = primary
    assert primary.use_count() == 2
    assert secondary.use_count() == 2
}

void testWeakLock()
{
    shared<Counter> owner = shared<Counter>(Counter(6, 600))
    weak<Counter> observer = weak<Counter>(owner)
    shared<Counter> promoted = observer.lock()
    assert promoted.use_count() == 2

    weak<Counter> dangling = weak<Counter>()
    if true
    {
        shared<Counter> temp = shared<Counter>(Counter(7, 700))
        dangling = weak<Counter>(temp)
    }

    shared<Counter> expired = dangling.lock()
    assert expired.use_count() == 0
}

// Test shared<T> with primitive types
void testSharedPrimitives()
{
    shared<int> sharedNum = shared<int>(777)
    shared<int> copy1 = sharedNum
    shared<int> copy2 = sharedNum
    shared<int> copy3 = copy1
    // All copies share the same control block
}

// Test weak<T> with multiple weak observers
void testMultipleWeakObservers()
{
    shared<Counter> owner = shared<Counter>(Counter(4, 400))
    weak<Counter> observer1 = weak<Counter>(owner)
    weak<Counter> observer2 = weak<Counter>(owner)
    weak<Counter> observer3 = weak<Counter>(owner)
    // All observers watch the same shared owner
}

void testSharedReassignmentLoop()
{
    shared<Counter> head = shared<Counter>(Counter(20, 2000))
    int i = 0
    while i < 20
    {
        shared<Counter> next = shared<Counter>(Counter(20 + i, 2000 + i))
        shared<Counter> copy = next
        head = copy
        i++
    }
}

void testWeakReassignmentLoop()
{
    shared<Counter> source = shared<Counter>(Counter(30, 3000))
    weak<Counter> watcher = weak<Counter>(source)
    int i = 0
    while i < 20
    {
        shared<Counter> next = shared<Counter>(Counter(30 + i, 3000 + i))
        weak<Counter> alias = weak<Counter>(next)
        watcher = alias
        source = next
        i++
    }
}

void testUniqueReassignmentLoop()
{
    unique<Counter> donor = unique<Counter>(Counter(40, 4000))
    unique<Counter> slot = unique<Counter>(Counter(41, 4100))
    int i = 0
    while i < 25
    {
        slot = donor
        donor = unique<Counter>(Counter(100 + i, 5000 + i))
        unique<Counter> scratch = unique<Counter>(Counter(200 + i, 6000 + i))
        slot = scratch
        donor = unique<Counter>(Counter(300 + i, 7000 + i))
        i++
    }
}

int main()
{
    testUniqueOwnership()
    testSharedRefCounting()
    testWeakObservation()
    testSharedPrimitives()
    testMultipleWeakObservers()
    testSharedUseCount()
    testWeakLock()
    testSharedReassignmentLoop()
    testWeakReassignmentLoop()
    testUniqueReassignmentLoop()

    // Basic smoke test with direct usage
    unique<int> u = unique<int>(10)
    shared<int> s = shared<int>(20)
    weak<int> w = weak<int>(s)

    return 0
}