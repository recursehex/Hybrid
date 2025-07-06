// Test foreach loops with array literals directly

// Test 1: Simple foreach with direct array literal
for int n in [1, 2, 3, 4, 5] {
    n
}

// Test 2: Foreach in a function
int sum_direct() {
    int total = 0
    for int x in [10, 20, 30] {
        total = total + x
    }
    return total
}

sum_direct()