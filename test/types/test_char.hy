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

// Function that works with chars
int processChar(char c) {
  // Convert char to int for testing
  int value = c
  return value
}

// Test character operations
int testCharOps() {
  char local_letter = 'A'
  char result = toUpper('a')
  bool isLetter = local_letter == 'A'
  
  int char_value = processChar(local_letter)
  return char_value
}

int main() {
  int result = testCharOps()
  return result
}