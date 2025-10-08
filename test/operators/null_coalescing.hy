extern void print(int x)

int counter = 0

string computeName()
{
    counter++
    return "fallback"
}

string? maybeName(bool flag)
{
    return "real" if flag else null
}

int main()
{
    string? alias = "value"
    string resolved = alias ?? computeName()
    assert counter == 0
    print(counter)

    string? missing = maybeName(false)
    string ensured = missing ?? computeName()
    assert counter == 1
    print(counter)

    string? assignTarget = null
    assignTarget ??= computeName()
    assert counter == 2
    assert assignTarget != null
    assignTarget ??= computeName()
    assert counter == 2

    string? inlineAssign = maybeName(false)
    string direct = inlineAssign ??= "direct"
    assert inlineAssign != null
    assert counter == 2
    print(counter)

    return 0
}