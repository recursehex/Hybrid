// Test type casting errors with sized types
// All operations in this file should fail

// Test 1: Mixing different sized integers
short s = 100
int i = 200
int bad1 = s + i        // Error: cannot mix short and int

// Test 2: Mixing unsigned and signed of different sizes
byte b = 255
short s2 = 100
int bad2 = b + s2       // Error: cannot mix byte and short

// Test 3: Bool isolation
bool flag = true
int num = 42
int bad3 = flag + num   // Error: cannot mix bool and int

// Test 4: Different sized assignments
long l = 1000000
int bad4 = l            // Error: cannot assign long to int

// Test 5: Function parameter type mismatch
int takes_int(int x)
{
    return x
}

short s3 = 100
takes_int(s3)           // Error: cannot pass short where int expected

// Test 6: Array type mismatches
byte[] bytes = [1, 2, 3]
short s4 = bytes[0]     // Error: cannot assign byte to short

// Test 7: Unsigned/signed mismatch of same size
uint ui = 1000
int i2 = ui             // Error: cannot assign uint to int (different types even though same size)

// Test 8: Character type mismatches  
lchar lc = 'A'
char c = lc             // Error: cannot assign lchar (32-bit) to char (8-bit)