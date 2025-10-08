// Demonstrates that pointer types accept null assignments

unsafe void test_null_pointer_initialization()
{
    int@ ptr = null

    int value = 42
    ptr = #value
    int loaded = @ptr

    ptr = null
}
