// EXPECT_DIAGNOSTIC: Unknown tuple field 'total'
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
int main()
{
    (int count, string greeting) named = (1, "hi")
    int x = named.total
    return 0
}
