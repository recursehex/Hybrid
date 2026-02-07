// EXPECT_DIAGNOSTIC: Use 'inherits' to declare base types
// EXPECT_DIAGNOSTIC: Expected identifier after type
// EXPECT_DIAGNOSTIC: Unknown function referenced: Derived
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
class Base
{
    Base() {}
}

class Derived : Base
{
    Derived() {}
}

// expect: Use 'inherits' to declare base types
