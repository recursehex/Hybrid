// EXPECT_DIAGNOSTIC: Null-coalescing operator '??' requires nullable left-hand side
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
int main()
{
    int value = 5
    int result = value ?? 10
    return result
}
