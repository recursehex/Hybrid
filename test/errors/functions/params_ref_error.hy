// EXPECT_DIAGNOSTIC: params parameter 'values' cannot be declared as ref
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// EXPECT_OUTPUT: cannot be declared as ref
int bad(params ref int[] values)
{
    return 0
}
