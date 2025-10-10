// Tests for string interpolation feature

string echo(string value)
{
    return value
}

int add(int lhs, int rhs)
{
    return lhs + rhs
}

string formatPoint(int x, int y)
{
    return $"Point(`x`, `y`)"
}

int main()
{
    string name = "World"
    int age = 25
    float pi = 3.14159
    double e = 2.71828
    bool isActive = true
    char letter = 'A'
    byte small = 42
    long distance = 123456789
    ulong bigUnsigned = 9000000000
    schar tiny = 'z'

    string greet = $"Hello, `name`!"
    string summary = $"Name: `name`, Age: `age`"
    string computed = $"Next year: `age + 1`"
    string floatFmt = $"Pi approx: `pi:2`"
    string doubleFmt = $"Euler: `e:3`"
    string boolStr = $"Active: `isActive`"
    string charStr = $"Letter: `letter`"
    string byteStr = $"Byte: `small` Tiny: `tiny`"
    string longStr = $"Distance `distance` - unsigned `bigUnsigned`"
    string nested = echo($"Nested result `add(age, 5)`")
    string point = formatPoint(2, 3)
    string escaped = $"Literal backtick: \`inside\`"
    string consecutive = $"`name``age`"
   string passThrough = $"Existing string `point`"

    string combined = $"Summary: `summary`, bool `boolStr`, nested `nested`, direct `passThrough`"
    string onlyLiteral = $"Static text only"
    assert greet == "Hello, World!"
    assert summary == "Name: World, Age: 25"
    assert computed == "Next year: 26"
    assert floatFmt == "Pi approx: 3.14"
    assert doubleFmt == "Euler: 2.718"
    assert boolStr == "Active: true"
    assert charStr == "Letter: A"
    assert byteStr == "Byte: 42 Tiny: z"
    assert longStr == "Distance 123456789 - unsigned 9000000000"
    assert nested == "Nested result 30"
    assert point == "Point(2, 3)"
    assert escaped == "Literal backtick: `inside`"
    assert consecutive == "World25"
    assert passThrough == "Existing string Point(2, 3)"
    string expectedCombined = "Summary: " + summary + ", bool " + boolStr + ", nested " + nested + ", direct " + passThrough
    assert combined == expectedCombined
    assert onlyLiteral == "Static text only"

    return 0
}
