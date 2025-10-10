// Test string type and string literals

// Basic string variable declaration
string greeting = "Hello, World!"
string name = "Hybrid"
string empty = ""

// String with escape sequences
string newline_test = "Line 1\nLine 2"
string tab_test = "Column1\tColumn2"
string quote_test = "He said \"Hello\" to me"
string backslash_test = "C:\\Users\\test\\file.txt"

// Function that returns a string
string getMessage()
{
    return "Hello from function"
}

// Function that takes strings as parameters
bool compareStrings(string a, string b)
{
    return a == b
}

// Test string operations
int testStringOps()
{
    string local_greeting = "Hello"
    string message = getMessage()
    bool same = compareStrings(local_greeting, "Hello")
    
    assert message == "Hello from function"
    assert same

    if same
    {
        return 1
    }
    else
    {
        return 0
    }
}

int main()
{
    int result = testStringOps()
    assert result == 1
    return result
}