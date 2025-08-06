// Test that bitwise operators fail with float operands
// This test should produce compilation errors

// Test bitwise AND with floats - should fail
float testFloatAnd() {
    float a = 12.5
    float b = 10.3
    float result = a & b  // Error: Bitwise AND requires integer operands
    return result
}

// Test left shift with float - should fail
float testFloatShift() {
    float a = 5.5
    float result = a << 2  // Error: Left shift requires integer operands
    return result
}

// This should cause compilation errors
testFloatAnd()
testFloatShift()