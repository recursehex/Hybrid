// EXPECT_DIAGNOSTIC: Tuple index out of range
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
int main()
{
    (int, string) pair = (1, "hi")
    int x = pair[2]
    return 0
}
