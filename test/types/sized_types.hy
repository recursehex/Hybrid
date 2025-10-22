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

assert b1 == 255
assert sb1 == 127
assert s1 == 32767
assert us1 == 65535

// Test arrays of sized types
byte[] bytes = [10, 20, 30]
short[] shorts = [100, 200, 300]
long[] longs = [1000, 2000, 3000]

// Test array access
byte b_elem = bytes[0]
short s_elem = shorts[1]
long l_elem = longs[2]

assert b_elem == 10
assert s_elem == 200
assert l_elem == 3000

// Test functions with sized types
byte identity_byte(byte x)
{
    return x
}

short add_shorts(short a, short b)
{
    return a + b
}

long multiply_long(long x, long y)
{
    return x * y
}

// Test function calls
assert identity_byte(b1) == 255
assert add_shorts(100, 200) == 300
assert multiply_long(10, 20) == 200

// Test type promotion to float/double
float f1 = 3.14
double d1 = 2.718

// These should work (int to float promotion)
float f_result = i1 + f1
double d_result = i2 + d1

// Test character operations
char char_test(char c)
{
    return c
}

schar schar_test(schar sc)
{
    return sc
}

lchar lchar_test(lchar lc)
{
    return lc
}

// Call character functions
char_test(c1)
schar_test(sc1)
lchar_test(lc1)

// ========================================
// Literal Type Inference Tests
// ========================================

// Test 1: Comparisons with literals (both directions)
byte b_cmp = 100
assert b_cmp == 100     // Literal adapts to byte
assert 100 == b_cmp     // Reversed also works
assert b_cmp != 99
assert 255 > b_cmp
assert b_cmp < 200

short s_cmp = 1000
assert s_cmp == 1000
assert 1000 == s_cmp
assert s_cmp >= 1000
assert 1000 <= s_cmp

long l_cmp = 50000
assert l_cmp == 50000
assert 50000 == l_cmp

// Test 2: Arithmetic with literals
byte b_arith = 50
byte b_sum = b_arith + 10      // 10 becomes byte
byte b_diff = 100 - b_arith    // 100 becomes byte
byte b_prod = b_arith * 2      // 2 becomes byte
byte b_quot = b_arith / 5      // 5 becomes byte
byte b_mod = b_arith % 7       // 7 becomes byte

assert b_sum == 60
assert b_diff == 50
assert b_prod == 100
assert b_quot == 10
assert b_mod == 1

// Test with reversed operands
byte b_rev = 5 + b_arith
assert b_rev == 55

short s_arith = 500
short s_sum = s_arith + 23
short s_prod = 2 * s_arith

assert s_sum == 523
assert s_prod == 1000

// Test 3: Function arguments with literals
void check_byte(byte x)
{
    assert x == 42
}

void check_short(short x)
{
    assert x == 1234
}

void check_long(long x)
{
    assert x == 99999
}

check_byte(42)          // 42 becomes byte
check_short(1234)       // 1234 becomes short
check_long(99999)       // 99999 becomes long

// Test 4: Nested expressions
byte b_nested = (10 + 20) * 2
assert b_nested == 60

short s_nested = (100 + 200) / 3
assert s_nested == 100

// Test 5: Complex mixed expressions
byte b_complex = 10
assert b_complex + 5 == 15
assert 20 - b_complex == 10
assert b_complex * 3 != 29
assert b_complex > 5 && b_complex < 20

short s_complex = 250
assert s_complex + 750 == 1000
assert 500 + s_complex == 750
assert s_complex * 2 > 400

// Test 6: All comparison operators
byte b_ops = 42
assert b_ops == 42
assert b_ops != 43
assert b_ops < 50
assert b_ops > 40
assert b_ops <= 42
assert b_ops >= 42

// Test 7: Literal inference with maximum values
byte b_max = 255
assert b_max == 255
assert b_max + 0 == 255

sbyte sb_max = 127
assert sb_max == 127
assert sb_max - 0 == 127

short s_max = 32767
assert s_max == 32767
assert s_max * 1 == 32767

// Test 8: Literal inference preserves type safety
// These operations work because literals adapt to the variable's type
byte safe_byte = 100
byte safe_result = safe_byte + 50
assert safe_result == 150

// Verify the result is still a byte by using it in byte context
byte verify = safe_result + 5
assert verify == 155

// Test 9: Runtime range checking for variable assignments
// Explicit casts required when assigning wider values to narrower slots
int runtime_val = 200
byte runtime_byte = byte: runtime_val
assert runtime_byte == 200

int runtime_short_val = 30000
short runtime_short = short: runtime_short_val
assert runtime_short == 30000

void test_unsigned_comparisons()
{
    uint big = 4000000000
    assert big > 2
    assert !(big < 2)

    ulong huge = 18446744073709551610
    assert huge > 100
    assert !(huge < 100)
}

void test_unsigned_division_and_mod()
{
    uint big = 4000000000
    uint half = big / 2
    assert half == 2000000000

    uint remainder = big % 3
    assert remainder == 1

    ulong huge = 18446744073709551614
    ulong halfHuge = huge / 2
    assert halfHuge == 9223372036854775807
}

test_unsigned_comparisons()
test_unsigned_division_and_mod()
