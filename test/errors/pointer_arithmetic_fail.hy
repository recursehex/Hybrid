// Invalid pointer arithmetic scenarios

unsafe void pointer_offset_float_fail()
{
    int value = 10
    int@ ptr = #value

    int@ next = ptr + 1.5  // Error: Pointer arithmetic requires an integer offset
}

unsafe void pointer_add_pointer_fail()
{
    int a = 1
    int b = 2
    int@ lhs = #a
    int@ rhs = #b

    int@ sum = lhs + rhs  // Error: Cannot add two pointer values
}

unsafe void pointer_subtraction_mismatch_fail()
{
    int x = 100
    float y = 200.0

    int@ ip = #x
    float@ fp = #y

    long diff = ip - fp  // Error: Pointer subtraction requires both operands to have the same pointer type
}

unsafe void pointer_compound_invalid_fail()
{
    int value = 5
    int@ ptr = #value

    ptr *= 2  // Error: Pointer compound assignment only supports '+=' and '-='
}
