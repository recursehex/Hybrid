// EXPECT_DIAGNOSTIC: Type checks with 'is' are only supported in conditional expressions
// EXPECT_DIAGNOSTIC: Expected expression after '='
int value = 10
bool ok = value is int
