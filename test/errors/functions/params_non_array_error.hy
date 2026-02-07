// EXPECT_DIAGNOSTIC: params parameter 'value' must be a single-dimensional array type
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// EXPECT_OUTPUT: must be a single-dimensional array type
int bad(params int value)
{
    return value
}
