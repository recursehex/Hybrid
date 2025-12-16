// EXPECT_OUTPUT: Parameters passed by reference cannot [currently] declare default values
int bad(ref int value = 0)
{
    return value
}

