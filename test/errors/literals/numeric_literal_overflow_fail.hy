// Expect failure: literal exceeds 64-bit range
int main() {
    ulong tooBig = 0x1FFFFFFFFFFFFFFFF
    return 0
}
