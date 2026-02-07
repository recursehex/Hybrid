// EXPECT_DIAGNOSTIC: Array bounds do not match declared rank for 'int[,]'
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// Rectangular arrays require one bound per dimension.
int main()
{
    int[,] grid = new[3]
    return 0
}
