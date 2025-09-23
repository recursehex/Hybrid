// Test unsafe blocks and pointer operations

// Test 1: Unsafe function with pointer parameter
unsafe void swap(int@ a, int@ b)
{
    int temp = @a
    @a = @b
    @b = temp
}

// Test 2: Unsafe block with address-of operator
void test_address_of()
{
    int x = 42

    unsafe
    {
        int@ ptr = #x
        int value = @ptr
    }
}

// Test 3: Multi-level pointers
unsafe void test_multi_level_pointers()
{
    int x = 10
    int@ ptr1 = #x
    int@2 ptrToPtr = #ptr1

    // Dereference multi-level pointer
    int@ deref1 = @ptrToPtr
    int value = @deref1
}

// Test 4: Pointer member access with arrow operator
struct Point
{
    int x
    int y

    Point(int x, int y)
    {
        this.x = x
        this.y = y
    }
}

unsafe void test_arrow_operator()
{
    Point p = Point(10, 20)
    Point@ ptr = #p

    // Use arrow operator for pointer member access
    int x_val = ptr->x
    int y_val = ptr->y

    // Modify through pointer
    ptr->x = 30
    ptr->y = 40
}

// Test 5: Simple array element pointers
unsafe void test_array_element_pointers()
{
    int[] arr = [1, 2, 3, 4, 5]
    int@ firstPtr = #arr[0]  // Pointer to first element

    // Access element through pointer
    int first = @firstPtr
    @firstPtr = 10  // Modify through pointer
}

// Test 6: Nested unsafe blocks
void test_nested_unsafe()
{
    int a = 10

    unsafe
    {
        int@ ptr1 = #a

        unsafe
        {
            // Nested unsafe block
            int@2 ptr2 = #ptr1
            int@ deref1 = @ptr2  // First dereference
            int val = @deref1    // Second dereference
        }
    }
}