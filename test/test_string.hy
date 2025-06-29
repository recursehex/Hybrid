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
string escaped = "Line 1\nLine 2\tTabbed"
string quotes = "She said \"Hello\" to me"
string backslash = "C:\\Users\\test"

// Function that returns a string
string getMessage() {
  return "Hello from function"
}

// Function that takes and returns strings
string concat(string a, string b) {
  return a + b
}

// Function with string parameters
void printMessage(string msg) {
  print(msg)
  return
}

// String in foreach loop
for string word in words {
  print(word)
}

// Complex string expressions
string result = concat("Hello", " World")
string combined = greeting + " from " + name

// String comparison (if implemented)
bool isEqual = greeting == "Hello, World!"