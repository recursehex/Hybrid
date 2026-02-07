// EXPECT_DIAGNOSTIC: Unterminated block comment
// Unterminated block comment should trigger a diagnostic
/* missing closing delimiter
int main()
{
    return 0
}
