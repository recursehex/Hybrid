// Default parameters and named argument support
extern int __hybrid_string_equals(string lhs, string rhs)

int add(int a, int b = 2, int c = 3)
{
    return a + b + c
}

string greet(string name = "world")
{
    return name
}

class Accumulator
{
    int seed

    Accumulator(int start = 10)
    {
        this.seed = start
    }

    int sum(int extra = 1, int bonus = 0)
    {
        return this.seed + extra + bonus
    }
}

class Logger
{
    bool appendNewline

    Logger(bool appendNewline = true)
    {
        this.appendNewline = appendNewline
    }

    bool ShouldAppend()
    {
        return this.appendNewline
    }
}

int main()
{
    assert add(1) == 6
    assert add(1, 4) == 8
    assert add(1, b = 5, c = 6) == 12
    assert add(a = 2, c = 10) == 14
    assert add(c = 5, a = 1) == 8

    assert __hybrid_string_equals(greet(), "world") == 1
    assert __hybrid_string_equals(greet(name = "Hybrid"), "Hybrid") == 1

    Accumulator acc = Accumulator()
    assert acc.sum() == 11
    assert acc.sum(extra = 0, bonus = 5) == 15

    Accumulator other = Accumulator(3)
    assert other.sum(bonus = 2) == 6

    Logger logger = Logger()
    assert logger.ShouldAppend() == true

    Logger quiet = Logger(appendNewline = false)
    assert quiet.ShouldAppend() == false

    return 0
}
