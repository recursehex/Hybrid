// Test unsafe structs with pointer array fields

// Test 1: Basic unsafe struct with pointer array field
unsafe struct PtrContainer
{
    int@[] pointers
    int count

    PtrContainer(int@[] ptrs, int c)
    {
        this.pointers = ptrs
        this.count = c
    }
}

// Test 2: Using the unsafe struct
unsafe void test_ptr_container()
{
    int x = 10
    int y = 20
    int z = 30

    // Create array of pointers
    int@[] ptrs = [#x, #y, #z]

    // Create container
    PtrContainer container = PtrContainer(ptrs, 3)

    int c = container.count
    assert c == 3
    assert @container.pointers[0] == 10
    assert @container.pointers[1] == 20
    assert @container.pointers[2] == 30
}

// Test 3: Unsafe struct with mixed pointer types
unsafe struct MixedPtrStruct
{
    int@ singlePtr
    int@[] arrayOfPtrs
    float@ floatPtr

    MixedPtrStruct(int@ sp, int@[] ap, float@ fp)
    {
        this.singlePtr = sp
        this.arrayOfPtrs = ap
        this.floatPtr = fp
    }
}

// Test 4: Nested unsafe structs
unsafe struct OuterStruct
{
    PtrContainer container
    int@ directPtr

    OuterStruct(PtrContainer c, int@ p)
    {
        this.container = c
        this.directPtr = p
    }
}

unsafe void test_nested_structs()
{
    int val = 42
    int@[] ptrs = [#val]

    PtrContainer inner = PtrContainer(ptrs, 1)
    OuterStruct outer = OuterStruct(inner, #val)

    // Access through direct pointer
    int result = @outer.directPtr
    assert result == 42
    assert inner.count == 1
}

// Test 5: Simple test without methods
unsafe struct SimplePtr
{
    int@ ptr

    SimplePtr(int@ p)
    {
        this.ptr = p
    }
}

unsafe void test_simple_ptr()
{
    int x = 100
    SimplePtr sp = SimplePtr(#x)
    int val = @sp.ptr  // Direct pointer access
    @sp.ptr = 200      // Modify through pointer
    assert val == 100
    assert @sp.ptr == 200
}