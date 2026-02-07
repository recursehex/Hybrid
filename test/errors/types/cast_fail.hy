// EXPECT_DIAGNOSTIC: Cannot cast to void
// EXPECT_DIAGNOSTIC: Failed to generate IR for variable declaration
// EXPECT_DIAGNOSTIC: Unknown function referenced: func
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unknown function referenced: func
// EXPECT_DIAGNOSTIC: Failed to generate IR for variable declaration
// EXPECT_DIAGNOSTIC: Cannot cast bool to int
// EXPECT_DIAGNOSTIC: Failed to generate IR for variable declaration
// Test cases that should fail for type casting

// Test casting to void (should fail)
int x = 10
void v = void: x            // Error: Cannot cast to void

// Test casting from void (should fail)
void func()
{
    return
}
int y = int: func()         // Error: Cannot cast void to int

// Test casting bool (currently not supported in explicit casts)
bool flag = true
int boolToInt = int: flag   // Error: Cannot cast bool to int
