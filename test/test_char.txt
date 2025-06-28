// Test char type and character literals

// Basic character variable declarations
char letter = 'A'
char digit = '5'
char space = ' '
char newline = '\n'
char tab = '\t'
char backslash = '\\'
char quote = '\''
char null_char = '\0'

// Function that takes and returns char
char toUpper(char c) {
  return c
}

// Function with char parameters
void printChar(char c) {
  print(c)
  return
}

// Character in foreach loop
for char ch in text {
  print(ch)
}

// Character expressions
char result = toUpper('a')
bool isLetter = letter == 'A'