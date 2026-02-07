// EXPECT_DIAGNOSTIC: Array initializer does not match declared size for 'arr'
// EXPECT_DIAGNOSTIC: Failed to generate IR for variable declaration
// Declared size should match array literal length
int[3] arr = [1, 2]

int main()
{
    return arr[0]
}
