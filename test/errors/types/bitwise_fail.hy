// EXPECT_DIAGNOSTIC: Bitwise AND requires integer operands
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// EXPECT_DIAGNOSTIC: No matching overload found for call to 'testFloatAnd'
// EXPECT_DIAGNOSTIC: Unknown function referenced: testFloatShift
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
