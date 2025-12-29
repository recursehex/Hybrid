// EXPECT_FAIL: runtime
// EXPECT_EXIT: 1
// This should be treated as a failing program: main returns a non-zero code.
int main()
{
    return 1
}
