// Error: format specifier must be an integer

string badFormat()
{
    float value = 1.23
    string result = $"Value: `value:abc`"
    return result
}
