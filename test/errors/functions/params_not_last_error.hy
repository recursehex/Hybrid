// EXPECT_DIAGNOSTIC: params parameter must be the final parameter in the list
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// EXPECT_OUTPUT: params parameter must be the final parameter in the list
int bad(int a, params int[] rest, int b)
{
    return a
}
