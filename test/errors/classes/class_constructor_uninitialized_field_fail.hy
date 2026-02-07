// EXPECT_DIAGNOSTIC: Constructor for class 'Incomplete' must initialize member 'b'
class Incomplete
{
    int a
    int b

    Incomplete(int a)
    {
        this.a = a
    }
}

// expect: Constructor for class 'Incomplete' must initialize member 'b'
