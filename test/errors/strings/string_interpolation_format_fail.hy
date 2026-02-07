// EXPECT_DIAGNOSTIC: Expected integer format specifier in interpolated string expression
// Error: format specifier must be an integer

string badFormat()
{
    float value = 1.23
    string result = $"Value: `value:abc`"
    return result
}
