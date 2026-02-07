// EXPECT_DIAGNOSTIC: String literal contains an invalid UTF-8 sequence
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
int main()
{
    string invalid = "\uD800"
    return 0
}
