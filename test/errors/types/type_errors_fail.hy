// EXPECT_DIAGNOSTIC: Cannot implicitly convert 'byte' to 'int' in initializer for 'boolSum'; explicit cast required
// EXPECT_DIAGNOSTIC: Failed to generate IR for variable declaration
// EXPECT_DIAGNOSTIC: Unknown function referenced: takes_int
// EXPECT_DIAGNOSTIC: Cannot implicitly convert 'long' to 'int' in initializer for 'narrowed'; explicit cast required
// EXPECT_DIAGNOSTIC: Failed to generate IR for variable declaration
// EXPECT_DIAGNOSTIC: Unknown variable name: implicitSource
// EXPECT_DIAGNOSTIC: Unknown variable name: implicitSource
// EXPECT_DIAGNOSTIC: Failed to generate IR for variable declaration
// EXPECT_DIAGNOSTIC: Unknown variable name: unsignedAssign
// EXPECT_DIAGNOSTIC: Failed to generate IR for variable declaration
// EXPECT_DIAGNOSTIC: Cannot implicitly convert 'lchar' to 'char' in initializer for 'narrowChar'; explicit cast required
// EXPECT_DIAGNOSTIC: Failed to generate IR for variable declaration
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

// Test 4b: Assigning same-width signed value to smaller type
int implicitSource = 10
short implicitShort = implicitSource   // Error: requires explicit cast from int to short
uint unsignedAssign = implicitSource   // Error: requires explicit cast from int to uint

// Test 5: Assigning string to numeric type
int stringToInt = "123"         // Error: string cannot be assigned to int

// Test 6: Mixing string with numeric addition
int mixedAdd = "foo" + 1        // Error: string cannot be added to int

// Test 7: Character narrowing without cast
lchar lc = 'A'
char narrowChar = lc            // Error: requires explicit cast from lchar to char
