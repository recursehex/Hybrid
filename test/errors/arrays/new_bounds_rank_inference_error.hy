// EXPECT_DIAGNOSTIC: Array bounds do not match declared rank for 'int[]'
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// `new int[rows, cols]` infers a rectangular array type, not a 1D array.
int main()
{
    int[] values = new int[1, 3]
    return 0
}
