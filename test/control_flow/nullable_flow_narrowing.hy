// Tests flow-sensitive nullable narrowing in conditionals and loops using reference types

struct NumberBox
{
    int value

    NumberBox(int value)
    {
        this.value = value
    }
}

NumberBox? makeBox(bool include)
{
    if include
    {
        return NumberBox(5)
    }
    return null
}

int test_if_narrowing()
{
    NumberBox? maybe = makeBox(true)
    int total = 0
    if maybe != null
    {
        total = maybe.value
        NumberBox confirmed = maybe
        total += confirmed.value
    }
    return total
}

int test_else_narrowing()
{
    NumberBox? maybe = makeBox(true)
    if maybe == null
    {
        return -1
    }
    else
    {
        return maybe.value
    }
}

int test_early_return(NumberBox? maybe)
{
    if maybe == null
    {
        return -1
    }
    return maybe.value
}

int test_assignment_after_guard()
{
    NumberBox? maybe = null
    if maybe == null
    {
        maybe = NumberBox(9)
    }
    if maybe != null
    {
        return maybe.value
    }
    return -1
}

int test_loop_progression()
{
    NumberBox? current = NumberBox(3)
    int total = 0
    while current != null {
        total += current.value
        if current.value == 1
        {
            current = null
        }
        else
        {
            current = NumberBox(current.value - 1)
        }
    }
    return total
}

int main()
{
    assert test_if_narrowing() == 10
    assert test_else_narrowing() == 5
    assert test_early_return(makeBox(true)) == 5
    assert test_early_return(null) == -1
    assert test_assignment_after_guard() == 9
    assert test_loop_progression() == 6
    return 0
}