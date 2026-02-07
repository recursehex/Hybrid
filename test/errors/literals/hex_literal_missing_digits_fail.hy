// EXPECT_DIAGNOSTIC: Numeric literal '0x' is missing digits
// Expect failure: hex literal without digits
int main() {
    int value = 0x
    return value
}
