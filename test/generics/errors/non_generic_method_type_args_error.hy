// EXPECT_DIAGNOSTIC: Method 'Counter.Increment' does not accept explicit type arguments
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// Expect error when supplying type arguments to a non-generic method

class Counter
{
    Counter(int start)
    {
        this.value = start
    }

    int Increment()
    {
        this.value = this.value + 1
        return this.value
    }

    int value
}

void main()
{
    Counter counter = Counter(0)
    int value = counter.Increment<int>()
}
