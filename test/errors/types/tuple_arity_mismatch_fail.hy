// EXPECT_DIAGNOSTIC: Tuple literal has 3 element(s), but expected 2
int main()
{
    (int, string) bad = (1, "hi", "extra")
    return 0
}
