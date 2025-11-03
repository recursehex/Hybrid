// Verify that struct copy constructors are invoked and preserve value semantics

struct Counter
{
    int value
    int copies

    Counter(int value)
    {
        this.value = value
        this.copies = 0
    }

    Counter(Counter other)
    {
        this.value = other.value
        this.copies = other.copies + 1
    }
}

void bump(ref Counter target, int delta)
{
    target.value += delta
}

Counter original = Counter(42)
Counter copy = (original)

assert original.value == 42
assert original.copies == 0

assert copy.value == 42
assert copy.copies == 1

bump(ref original, 8)

assert original.value == 50     // mutated through ref parameter
assert copy.value == 42         // copy remains unchanged
assert copy.copies == 1