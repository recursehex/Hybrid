// EXPECT_DIAGNOSTIC: Array index cannot be negative
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// Test that array bounds checking catches negative indices - this should abort
extern void print(int x)

int main() {
    int[] arr = [10, 20, 30]

    // This should abort with bounds check error
    int bad = arr[-1]  // Negative index

    print(bad)
    return 0
}
