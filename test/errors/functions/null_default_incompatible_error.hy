// EXPECT_OUTPUT: Null default value is only allowed for reference or nullable parameter 'value'
int bad(int value = null)
{
    return value
}

