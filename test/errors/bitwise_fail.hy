// Test that bitwise operators fail with float operands

// Test bitwise AND with floats
float testFloatAnd()
{
    float a = 12.5
    float b = 10.3
    float result = a & b    // Error: Bitwise AND requires integer operands
    return result
}

// Test left shift with float
float testFloatShift()
{
    float a = 5.5
    float result = a << 2   // Error: Left shift requires integer operands
    return result
}

testFloatAnd()
testFloatShift()