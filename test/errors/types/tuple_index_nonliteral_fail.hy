// EXPECT_DIAGNOSTIC: Tuple index must be an integer literal
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
int main()
{
    (int, string) pair = (1, "hi")
    int i = 1
    int x = pair[i]
    return 0
}
