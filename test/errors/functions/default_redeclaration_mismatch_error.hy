// EXPECT_DIAGNOSTIC: Default value for parameter 'value' of 'add' must match previous declaration
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// EXPECT_OUTPUT: Default value for parameter 'value' of 'add' must match previous declaration
extern int add(int value = 1)

int add(int value = 2)
{
    return value
}
