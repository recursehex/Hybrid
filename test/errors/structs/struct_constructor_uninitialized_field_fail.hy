// EXPECT_DIAGNOSTIC: Constructor for struct 'Incomplete' must initialize member 'height'
struct Incomplete
{
    int width
    int height

    Incomplete(int width)
    {
        this.width = width
    }
}

// expect: Constructor for struct 'Incomplete' must initialize member 'height'
