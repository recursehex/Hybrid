// EXPECT_DIAGNOSTIC: Array bounds do not match declared rank for 'int[][]'
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// Jagged arrays allocate one dimension at a time.
int main()
{
    int[][] rows = new[2, 3]
    return 0
}
