// Test arrays of pointers

// Test 1: Array of pointers to integers
unsafe void test_int_pointer_array() {
    int x = 10
    int y = 20
    int z = 30

    // Create an array of int pointers
    int@[] ptrs = [#x, #y, #z]

    // Access through pointer array
    int first = @ptrs[0]    // Should be 10
    int second = @ptrs[1]   // Should be 20

    // Modify through pointer array
    @ptrs[0] = 100
    @ptrs[1] = 200
}

// Test 2: Function taking array of pointers
unsafe void sum_pointed_values(int@[] ptrs) {
    int sum = 0

    for int i in ptrs
    {
        sum += @ptrs[i]
    }
}

// Test 3: Returning pointer from array
unsafe int@ get_max_ptr(int@[] ptrs, int size) {
    // Use a loop to find the pointer to the maximum value
    if size <= 0 {
        return ptrs[0]  // Return first element for empty/invalid size
    }

    int@ maxPtr = ptrs[0]
    int maxVal = @ptrs[0]

    for int i = 1 to i < size {
        int currentVal = @ptrs[i]
        if currentVal > maxVal {
            maxVal = currentVal
            maxPtr = ptrs[i]
        }
    }

    return maxPtr
}

// Test 4: Nested - array of pointers to arrays
unsafe void test_nested_pointer_arrays() {
    int[] arr1 = [1, 2, 3]
    int[] arr2 = [4, 5, 6]
    int[] arr3 = [7, 8, 9]

    // Complex version: Create pointers to each element of each array
    // This creates a 2D-like structure using pointer arrays

    // Get pointers to all elements of arr1
    int@[] row1 = [#arr1[0], #arr1[1], #arr1[2]]

    // Get pointers to all elements of arr2
    int@[] row2 = [#arr2[0], #arr2[1], #arr2[2]]

    // Get pointers to all elements of arr3
    int@[] row3 = [#arr3[0], #arr3[1], #arr3[2]]

    // Can access like a 2D array through pointer dereferencing
    // Access element at "row 0, col 0" (should be 1)
    int val00 = @row1[0]

    // Access element at "row 1, col 2" (should be 6)
    int val12 = @row2[2]

    // Access element at "row 2, col 1" (should be 8)
    int val21 = @row3[1]

    // Can also modify through these pointers
    @row1[0] = 100  // arr1[0] is now 100
    @row2[1] = 200  // arr2[1] is now 200

    // Verify modifications
    int check1 = arr1[0]  // Should be 100
    int check2 = arr2[1]  // Should be 200
}

// Test 5: Struct with pointer array field
unsafe struct PtrHolder {
    int@[] pointers
    int count

    PtrHolder(int@[] ptrs, int c) {
        this.pointers = ptrs
        this.count = c
    }
}

unsafe void test_struct_with_ptr_array() {
    int a = 1
    int b = 2
    int@[] myPtrs = [#a, #b]

    PtrHolder holder = PtrHolder(myPtrs, 2)

    // Access through struct
    int val = @holder.pointers[0]
}