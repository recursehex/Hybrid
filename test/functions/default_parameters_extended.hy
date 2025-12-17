extern int __hybrid_string_equals(string lhs, string rhs)

int GenericCount<T>(int start, int step = 1)
{
    return start + step
}

string MaybeDefault(string value = null)
{
    return value
}

class BaseCounter
{
    protected int start

    BaseCounter(int start = 5)
    {
        this.start = start
    }

    virtual int Bump(int delta = 1)
    {
        return this.start + delta
    }
}

class DerivedCounter inherits BaseCounter
{
    DerivedCounter(int offset = 0)
    {
        base()
        this.start += offset
    }

    override int Bump(int delta = 1)
    {
        return this.start + delta + 10
    }
}

int main()
{
    assert GenericCount<int>(2) == 3
    assert GenericCount<int>(start = 4, step = 3) == 7

    string fromDefault = MaybeDefault()
    assert fromDefault == null
    assert __hybrid_string_equals(MaybeDefault("set"), "set") == 1

    DerivedCounter counter = DerivedCounter()
    assert counter.Bump() == 16
    assert counter.Bump(delta = 2) == 17

    BaseCounter asBase = counter
    assert asBase.Bump() == 16
    assert asBase.Bump(delta = 3) == 18

    DerivedCounter offset = DerivedCounter(2)
    assert offset.Bump() == 18

    return 0
}
