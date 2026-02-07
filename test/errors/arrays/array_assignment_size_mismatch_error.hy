// EXPECT_DIAGNOSTIC: Array assignment does not match declared size for 'values'
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// Assigning an array literal with a different length should fail.
int main()
{
    int[] values = [1, 2, 3]
    values = [1, 2]
    return 0
}
