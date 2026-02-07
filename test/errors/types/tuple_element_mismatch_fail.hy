// EXPECT_DIAGNOSTIC: Tuple element 1 expects 'string', got 'int'
int main()
{
    (int, string) bad = (1, 2)
    return 0
}
