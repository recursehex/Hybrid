extern void print(int x)

string[] names = ["Alpha", "Beta"]
int fallbackCalls = 0

string defaultName()
{
    fallbackCalls++
    return "Unknown"
}

string[]? maybeNames(bool flag)
{
    return names if flag else null
}

int main()
{
    string[]? present = maybeNames(true)
    string? first = present?[0]
    assert first != null
    string fromArray = present?[1] ?? defaultName()
    assert fallbackCalls == 0
    print(fallbackCalls)

    string[]? missing = maybeNames(false)
    string? none = missing?[0]
    assert none == null
    string fallback = missing?[0] ?? defaultName()
    assert fallbackCalls == 1
    print(fallbackCalls)

    fallback = missing?[0] ?? defaultName()
    assert fallbackCalls == 2
    print(fallbackCalls)

    return 0
}