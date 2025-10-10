// Test type casting with colon operator

// Test int to float casting
int x = 42
float y = float: x
double z = double: x
assert y == 42.0
assert z == 42.0

// Test float to int casting
float pi = 3.14159
int truncated = int: pi
assert truncated == 3

// Test different sized integer casts
int bigNum = 1000
short smallNum = short: bigNum
byte tinyNum = byte: 100
int expandedNum = int: tinyNum
assert smallNum == short: 1000
assert tinyNum == 100
assert expandedNum == 100

// Test char to int and int to char
char letter = 'A'
int asciiValue = int: letter
char backToChar = char: 65
assert asciiValue == 65
assert backToChar == 'A'

// Function to test casting in expressions
int testCasting()
{
    int a = 10
    float b = 2.5
    
    // Cast int to float for division
    float result = float: a / b
    
    // Cast float result back to int
    int intResult = int: result
    assert result == 4.0
    assert intResult == 4
    
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
    assert medValue == 1234567890
    assert expandShort == short: tinyValue
    assert expandInt == int: tinyValue
    assert expandLong == long: tinyValue
}

// Test signed/unsigned casting
void testSignedUnsigned()
{
    sbyte signedByte = -10
    int signedInt = int: signedByte  // Should sign-extend
    assert signedInt == -10
    
    byte unsignedByte = 250
    int unsignedInt = int: unsignedByte  // Should zero-extend
    assert unsignedInt == 250
    
    // Cast between different character types
    schar smallChar = 'x'
    lchar wideChar = lchar: smallChar
    assert wideChar == 'x'
}

// Main test
testCasting()
testSizedCasting()
testSignedUnsigned()