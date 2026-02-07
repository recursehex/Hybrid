// EXPECT_DIAGNOSTIC: Invalid digit '2' in numeric literal '0b102'
// Expect failure: binary literal contains invalid digit
int main() {
    int value = 0b102
    return value
}
