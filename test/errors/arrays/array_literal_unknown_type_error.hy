// EXPECT_DIAGNOSTIC: Cannot infer element type for empty array literal
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// EXPECT_OUTPUT: Cannot infer element type for empty array literal
int main()
{
    print([])
    return 0
}
