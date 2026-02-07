// EXPECT_DIAGNOSTIC: Expected '{' after constructor declaration
class Base
{
    Base() {}
}

class Derived inherits Base
{
    Derived() : Base() {}
}

// expect: Expected '{' after constructor declaration
