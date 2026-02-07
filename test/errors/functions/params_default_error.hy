// EXPECT_DIAGNOSTIC: params parameter 'values' cannot declare a default value
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// EXPECT_OUTPUT: cannot declare a default value
int bad(params int[] values = 0)
{
    return 0
}
