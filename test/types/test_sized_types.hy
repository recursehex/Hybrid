// Comprehensive test for all sized primitive types

// 8-bit types
byte b1 = 255          // max unsigned byte
byte b2 = 0            // min unsigned byte
sbyte sb1 = 127        // max signed byte
sbyte sb2 = 0          // zero
char c1 = 'A'          // standard char
schar sc1 = 'B'        // signed char

// 16-bit types  
short s1 = 32767       // max signed short
short s2 = 0           // zero
ushort us1 = 65535     // max unsigned short
ushort us2 = 0         // min unsigned short

// 32-bit types
int i1 = 2147483647    // max signed int
int i2 = 0             // zero
uint ui1 = 4294967295  // max unsigned int
uint ui2 = 0           // min unsigned int

// 64-bit types
long l1 = 1000000000   // large positive long
long l2 = 0            // zero
ulong ul1 = 2000000000 // large unsigned long
ulong ul2 = 0          // min unsigned long

// 32-bit Unicode character
lchar lc1 = 'Z'        // 32-bit character

// Test same-type operations (should work)
byte b_add = b1 + b2
short s_add = s1 + s2  
int i_add = i1 + i2
long l_add = l1 + l2

// Test arrays of sized types
byte[] bytes = [10, 20, 30]
short[] shorts = [100, 200, 300]
long[] longs = [1000, 2000, 3000]

// Test array access
byte b_elem = bytes[0]
short s_elem = shorts[1]
long l_elem = longs[2]

// Test functions with sized types
byte identity_byte(byte x) {
    return x
}

short add_shorts(short a, short b) {
    return a + b
}

long multiply_long(long x, long y) {
    return x * y
}

// Test function calls
identity_byte(b1)
add_shorts(s1, s2)
multiply_long(l1, l2)

// Test type promotion to float/double
float f1 = 3.14
double d1 = 2.718

// These should work (int to float promotion)
float f_result = i1 + f1
double d_result = i2 + d1

// Test character operations
char char_test(char c) {
    return c
}

schar schar_test(schar sc) {
    return sc
}

lchar lchar_test(lchar lc) {
    return lc
}

// Call character functions
char_test(c1)
schar_test(sc1)
lchar_test(lc1)