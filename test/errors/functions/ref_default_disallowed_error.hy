// EXPECT_OUTPUT: Null default value is not allowed for ref parameter 'value'
int bad(ref int value = null)
{
    return value
}
