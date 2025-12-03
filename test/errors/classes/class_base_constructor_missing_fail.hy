class Base
{
    Base(int value) {}
}

class Derived inherits Base
{
    Derived() {}
}

// expect: Constructor for class 'Derived' must invoke base constructor of 'Base'