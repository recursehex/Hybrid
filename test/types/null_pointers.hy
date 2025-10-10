// Demonstrates that pointer types accept null assignments

unsafe void test_null_pointer_initialization()
{
    int@ ptr = null
    assert ptr == null

    int value = 42
    ptr = #value
    int loaded = @ptr
    assert loaded == 42

    ptr = null
    assert ptr == null
}

int main()
{
    unsafe
    {
        test_null_pointer_initialization()
    }
    return 0
}