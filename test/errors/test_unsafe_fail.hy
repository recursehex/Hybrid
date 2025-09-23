// Test that pointer operations fail outside unsafe contexts

// This should fail: pointer type outside unsafe context
void bad_function(int@ ptr) {
    int value = @ptr
}

// This should fail: address-of outside unsafe context
void test_address_fail() {
    int x = 42
    int@ ptr = #x  // Error: # operator outside unsafe
}

// This should fail: dereference outside unsafe context
void test_deref_fail() {
    unsafe {
        int x = 10
        int@ ptr = #x
    }
    // Outside unsafe block - this should fail
    int value = @ptr  // Error: @ operator outside unsafe
}

// This should fail: arrow operator outside unsafe context
struct Point {
    int x
    int y
}

void test_arrow_fail() {
    unsafe {
        Point p = Point(10, 20)
        Point@ ptr = #p
    }
    // Outside unsafe block - this should fail
    int x = ptr->x  // Error: -> operator outside unsafe
}