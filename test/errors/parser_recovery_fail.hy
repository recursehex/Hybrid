// Regression: ensure parser consumes a token when a statement fails to parse
// This used to hang because the 'default' keyword would be retried forever
int main()
{
    default
    return 0
}
