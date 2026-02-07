// EXPECT_DIAGNOSTIC: Integer literal '0x1FFFFFFFFFFFFFFFF' exceeds maximum supported 64-bit range
// Expect failure: literal exceeds 64-bit range
int main() {
    ulong tooBig = 0x1FFFFFFFFFFFFFFFF
    return 0
}
