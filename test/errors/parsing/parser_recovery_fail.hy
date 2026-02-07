// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// Regression: ensure parser consumes a token when a statement fails to parse
int main()
{
    default
    return 0
}
