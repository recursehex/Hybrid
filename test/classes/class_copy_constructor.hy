// Verify that class copy constructors are invoked and that copies are independent

class Buffer
{
    public int value
    public int version

    Buffer(int value)
    {
        this.value = value
        this.version = 0
    }

    Buffer(Buffer other)
    {
        this.value = other.value
        this.version = other.version + 1
    }

    void SetValue(int newValue)
    {
        this.value = newValue
    }
}

Buffer original = Buffer(7)
Buffer copy = (original)
Buffer secondCopy = (copy)

assert original.value == 7
assert original.version == 0

assert copy.value == 7
assert copy.version == 1

assert secondCopy.value == 7
assert secondCopy.version == 2

original.SetValue(13)

assert original.value == 13
assert copy.value == 7          // copy remains unchanged
assert secondCopy.value == 7    // second copy remains unchanged