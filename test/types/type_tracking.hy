// Test type tracking in expressions

// Test unsigned to float conversion
void testUnsignedToFloat()
{
    byte b = 255
    float f = float: b      // Should use UIToFP (unsigned to float)
    
    // Test that large unsigned values convert correctly
    uint ui = 3000000000    // > INT32_MAX
    double d = double: ui   // Should use UIToFP
}

// Test signed to float conversion  
void testSignedToFloat()
{
    sbyte sb = -100
    float f = float: sb     // Should use SIToFP (signed to float)
    
    int i = -12345
    double d = double: i    // Should use SIToFP
}

// Test unsigned extension
void testUnsignedExtension()
{
    byte b = 200            // Would be negative if sign-extended
    int i = int: b          // Should use ZExt (zero extension)
    
    ushort us = 50000       // > INT16_MAX
    long l = long: us       // Should use ZExt
}

// Test signed extension
void testSignedExtension()
{
    sbyte sb = -50
    int i = int: sb         // Should use SExt (sign extension)
    
    short s = -1000
    long l = long: s        // Should use SExt
}

// Main test
testUnsignedToFloat()
testSignedToFloat()
testUnsignedExtension()
testSignedExtension()