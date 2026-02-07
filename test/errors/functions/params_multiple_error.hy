// EXPECT_DIAGNOSTIC: Only one params parameter is allowed
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// EXPECT_OUTPUT: Only one params parameter is allowed
int bad(params int[] first, params int[] second)
{
    return 0
}
