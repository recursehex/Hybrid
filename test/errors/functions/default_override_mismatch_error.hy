// EXPECT_OUTPUT: Override of 'BaseCounter.Describe' in 'DerivedCounter' must use the same default for parameter 'delta'
class BaseCounter
{
    BaseCounter() {}

    virtual int Describe(int delta = 1)
    {
        return delta
    }
}

class DerivedCounter inherits BaseCounter
{
    DerivedCounter() {}

    override int Describe(int delta = 2)
    {
        return delta + 1
    }
}
