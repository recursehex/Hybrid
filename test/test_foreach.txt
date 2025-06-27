// Test foreach loops

// Simple foreach with array variable
int nums = 10
for int num in nums {
    num + 1
}

// Foreach with function call as collection
int get_list() {
    return 5
}

for float value in get_list() {
    value * 2.0
}

// Nested foreach loops
for int i in nums {
    for int j in nums {
        i + j
    }
}

// Foreach in a function
void process_items(int items) {
    for int item in items {
        item * item
    }
    return
}