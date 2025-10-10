// Test type tracking in expressions

// Test unsigned to float conversion
void testUnsignedToFloat()
{
    byte b = 255
    float f = float: b      // Should use UIToFP (unsigned to float)
    assert f == 255.0
    
    // Test that large unsigned values convert correctly
    uint ui = 3000000000    // > INT32_MAX
    double d = double: ui   // Should use UIToFP
    assert d == 3000000000.0
}

// Test signed to float conversion  
void testSignedToFloat()
{
    sbyte sb = -100
    float f = float: sb     // Should use SIToFP (signed to float)
    assert f == -100.0
    
    int i = -12345
    double d = double: i    // Should use SIToFP
    assert d == -12345.0
}

// Test unsigned extension
void testUnsignedExtension()
{
    byte b = 200            // Would be negative if sign-extended
    int i = int: b          // Should use ZExt (zero extension)
    assert i == 200
    
    ushort us = 50000       // > INT16_MAX
    long l = long: us       // Should use ZExt
    assert l == 50000
}

// Test signed extension
void testSignedExtension()
{
    sbyte sb = -50
    int i = int: sb         // Should use SExt (sign extension)
    assert i == -50
    
    short s = -1000
    long l = long: s        // Should use SExt
    assert l == -1000
}

// Main test
testUnsignedToFloat()
testSignedToFloat()
testUnsignedExtension()
testSignedExtension()