// EXPECT_DIAGNOSTIC: Cannot write to private member 'value' of class 'Counter'
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// EXPECT_DIAGNOSTIC: Constructor for class 'Fixed' must initialize member 'seed'
class Counter
{
    int value

    Counter()
    {
        this.value = 0
    }
}

void assignReadOnly()
{
    Counter c = Counter()
    c.value = 5  // expect: Member 'value' of class 'Counter' is read-only outside its definition
}

class Fixed
{
    const int seed

    Fixed(int seed)
    {
        this.seed = seed
    }

    void Reset()
    {
        this.seed = 0  // expect: Cannot write to const member 'seed' of class 'Fixed'
    }
}
