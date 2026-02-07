// EXPECT_DIAGNOSTIC: Unexpected type name in expression
int main()
{
    string base = "x"
    lchar wide = 0
    string bad = base + wide
    return 0
}
