// EXPECT_DIAGNOSTIC: Switch expressions must have a default case
// EXPECT_DIAGNOSTIC: Failed to generate IR for variable declaration
// Test for switch expression without default case (should fail)
int num = 5

// This should fail because switch expressions require a default case
int result = switch num
{
    1 => 10
    2 => 20
}
