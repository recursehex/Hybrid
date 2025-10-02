// Test type casting with colon operator

// Test int to float casting
int x = 42
float y = float: x
double z = double: x

// Test float to int casting
float pi = 3.14159
int truncated = int: pi

// Test different sized integer casts
int bigNum = 1000
short smallNum = short: bigNum
byte tinyNum = byte: 100
int expandedNum = int: tinyNum

// Test char to int and int to char
char letter = 'A'
int asciiValue = int: letter
char backToChar = char: 65

// Function to test casting in expressions
int testCasting()
{
    int a = 10
    float b = 2.5
    
    // Cast int to float for division
    float result = float: a / b
    
    // Cast float result back to int
    int intResult = int: result
    
    return intResult
}

// Test casting with different sized types
void testSizedCasting()
{
    long bigValue = 1234567890
    int medValue = int: bigValue
    short smallValue = short: medValue
    byte tinyValue = byte: smallValue
    
    // Cast back up
    short expandShort = short: tinyValue
    int expandInt = int: expandShort
    long expandLong = long: expandInt
}

// Test signed/unsigned casting
void testSignedUnsigned()
{
    sbyte signedByte = -10
    int signedInt = int: signedByte  // Should sign-extend
    
    byte unsignedByte = 250
    int unsignedInt = int: unsignedByte  // Should zero-extend
    
    // Cast between different character types
    schar smallChar = 'x'
    lchar wideChar = lchar: smallChar
}

// Main test
testCasting()
testSizedCasting()
testSignedUnsigned()