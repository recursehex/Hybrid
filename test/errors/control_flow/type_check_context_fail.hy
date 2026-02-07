// EXPECT_DIAGNOSTIC: Type checks with 'is' are only supported in conditional expressions
// EXPECT_DIAGNOSTIC: Expected expression after '='
// EXPECT_OUTPUT: Type checks with 'is' are only supported in conditional expressions
int value = 5
bool ok = value is int
