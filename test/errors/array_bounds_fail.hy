// Test that array bounds checking works - this should abort
extern void print(int x)

int main() {
    int[] arr = [10, 20, 30]

    // This should abort with bounds check error
    int bad = arr[3]  // Accessing index 3 when array has size 3 (indices 0, 1, 2)

    print(bad)
    return 0
}