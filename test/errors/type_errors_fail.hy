// Type error regression tests: every declaration or expression in this file
// must produce a compilation failure.

// Test 1: Bool isolation in arithmetic
int boolSum = true + 1          // Error: bool cannot participate in numeric addition

// Test 2: Bool assignment to wider numeric type
long boolAssign = false         // Error: bool cannot be assigned to long without cast

// Test 3: Bool passed where int expected
int takes_int(int x)
{
    return x
}

takes_int(true)                 // Error: bool cannot be passed to int parameter

// Test 4: Assigning larger integer to smaller without cast
long wideValue = 1000000
int narrowed = wideValue        // Error: requires explicit cast from long to int

// Test 5: Assigning string to numeric type
int stringToInt = "123"         // Error: string cannot be assigned to int

// Test 6: Mixing string with numeric addition
int mixedAdd = "foo" + 1        // Error: string cannot be added to int

// Test 7: Character narrowing without cast
lchar lc = 'A'
char narrowChar = lc            // Error: requires explicit cast from lchar to char