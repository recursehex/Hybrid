// EXPECT_DIAGNOSTIC: Null-safe array indexing requires nullable array type
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
int main()
{
    string[] names = ["one"]
    string? first = names?[0]
    return 0
}
