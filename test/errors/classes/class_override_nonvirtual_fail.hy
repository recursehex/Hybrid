class Base
{
    Base() {}

    int Value()
    {
        return 1
    }
}

class Derived inherits Base
{
    Derived() {}

    override int Value()
    {
        return 2
    }
}

// expect: Method 'Value' of class 'Derived' cannot override non-virtual member 'Value' of class 'Base'