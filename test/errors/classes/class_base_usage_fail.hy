// EXPECT_DIAGNOSTIC: 'base' may only be used inside instance methods
class Root
{
    Root() {}

    int Value()
    {
        return 1
    }
}

class Derived inherits Root
{
    Derived() {}

    static int CallBase()
    {
        return base.Value()
    }
}

// expect: 'base' may only be used inside instance methods
