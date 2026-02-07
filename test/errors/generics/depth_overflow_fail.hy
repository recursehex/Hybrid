// EXPECT_DIAGNOSTIC: Expected identifier after type
// EXPECT_DIAGNOSTIC: Unknown variable name: chain
// EXPECT_DIAGNOSTIC: Unknown variable name: Wrapper<Wrapper<Wrapper<Wrapper<Wrapper<Wrapper<int>>>>>>
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: 'this' can only be used inside struct methods
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// RUN_OPTS: --max-generic-depth 4

class Wrapper<T>
{
    T value

    Wrapper(T value)
    {
        this.value = value
    }
}

class TooDeep
{
    Wrapper<Wrapper<Wrapper<Wrapper<Wrapper<Wrapper<int>>>>>>> chain

    TooDeep(Wrapper<Wrapper<Wrapper<Wrapper<Wrapper<Wrapper<int>>>>>>> chain)
    {
        this.chain = chain
    }
}
