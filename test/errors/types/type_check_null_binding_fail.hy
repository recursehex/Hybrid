// EXPECT_DIAGNOSTIC: Pattern binding is not supported for 'null' checks
// EXPECT_DIAGNOSTIC: Expected condition after 'if'
// EXPECT_DIAGNOSTIC: Failed to parse if statement
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_OUTPUT: Pattern binding is not supported for 'null' checks
int value = 0
if value is null temp
{
}
