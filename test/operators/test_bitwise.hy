// Test bitwise operators

// Test bitwise AND
int testAnd() {
    int a = 12    // 1100 in binary
    int b = 10    // 1010 in binary
    int result = a & b  // Should be 8 (1000)
    return result
}

// Test bitwise OR
int testOr() {
    int a = 12    // 1100 in binary
    int b = 10    // 1010 in binary
    int result = a | b  // Should be 14 (1110)
    return result
}

// Test bitwise XOR
int testXor() {
    int a = 12    // 1100 in binary
    int b = 10    // 1010 in binary
    int result = a ^ b  // Should be 6 (0110)
    return result
}

// Test left shift
int testLeftShift() {
    int a = 5     // 0101 in binary
    int result = a << 2  // Should be 20 (10100)
    return result
}

// Test right shift
int testRightShift() {
    int a = 20    // 10100 in binary
    int result = a >> 2  // Should be 5 (00101)
    return result
}

// Test compound assignment operators
int testCompoundAssignments() {
    int x = 15    // 1111 in binary
    
    // Test &=
    x &= 7        // 0111 in binary, result should be 7
    
    // Test |=
    x |= 8        // 1000 in binary, result should be 15
    
    // Test ^=
    x ^= 3        // 0011 in binary, result should be 12
    
    // Test <<=
    x <<= 1       // Shift left by 1, result should be 24
    
    // Test >>=
    x >>= 2       // Shift right by 2, result should be 6
    
    return x
}

// Test bitwise operations with arrays
int testBitwiseArrays() {
    int[] values = [255, 128, 64, 32]
    
    // Test array element with bitwise operations
    int result = values[0] & values[1]  // 255 & 128 = 128
    result = result | values[2]         // 128 | 64 = 192
    result = result ^ values[3]         // 192 ^ 32 = 224
    
    // Test compound assignment on array elements
    values[0] &= 127    // 255 & 127 = 127
    values[1] |= 1      // 128 | 1 = 129
    values[2] ^= 255    // 64 ^ 255 = 191
    values[3] <<= 2     // 32 << 2 = 128
    
    return values[0] + values[1] + values[2] + values[3]  // 127 + 129 + 191 + 128 = 575
}

// Run all tests
testAnd()           // Should return 8
testOr()            // Should return 14
testXor()           // Should return 6
testLeftShift()     // Should return 20
testRightShift()    // Should return 5
testCompoundAssignments()  // Should return 6
testBitwiseArrays() // Should return 575