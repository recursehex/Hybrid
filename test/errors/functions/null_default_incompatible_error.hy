// EXPECT_DIAGNOSTIC: Null default value is only allowed for reference or nullable parameter 'value'
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// EXPECT_OUTPUT: Null default value is only allowed for reference or nullable parameter 'value'
int bad(int value = null)
{
    return value
}

