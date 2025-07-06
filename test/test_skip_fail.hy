// Test skip statement outside of a loop - should fail compilation
int test_skip_fail() {
    int x = 5
    
    if x > 0 {
        skip  // Error: skip not within a loop
    }
    
    return x
}

int main() {
    return test_skip_fail()
}