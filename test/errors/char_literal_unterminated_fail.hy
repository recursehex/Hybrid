// Expect failure: unterminated character literal should raise a lexer error
int main() {
    char c = 'x
    return 0
}
