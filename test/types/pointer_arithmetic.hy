// Pointer arithmetic in unsafe contexts

unsafe void test_pointer_arithmetic_linear()
{
    int[] data = [10, 20, 30, 40]
    int@ ptr = #data[0]
    int@ ptr2 = ptr + 2

    assert @ptr == 10
    assert @ptr2 == 30

    ptr = 1 + ptr
    assert @ptr == 20

    ptr2 -= 1
    assert @ptr2 == 20

    long diffSame = ptr2 - ptr
    assert diffSame == 0

    long diffAhead = (ptr + 2) - ptr
    assert diffAhead == 2

    ptr++
    assert @ptr == 30

    --ptr2
    assert @ptr2 == 10

    ptr -= 2
    assert @ptr == 10
}

unsafe void test_double_pointer_arithmetic()
{
    int a = 5
    int b = 7
    int@ first = #a
    int@ second = #b
    int@[] entries = [first, second]

    int@2 doublePtr = #entries[0]
    int@2 advanced = doublePtr + 1
    int@ valuePtr = @advanced
    assert @valuePtr == 7

    doublePtr += 1
    int@ afterIncrement = @doublePtr
    assert @afterIncrement == 7
    long diffSame = advanced - doublePtr
    assert diffSame == 0

    doublePtr -= 1
    int@ afterDecrement = @doublePtr
    assert @afterDecrement == 5

    long diffBack = doublePtr - advanced
    assert diffBack == -1
}

void test_pointer_subtraction_types()
{
    int[] data = [1, 2, 3, 4]

    unsafe
    {
        int@ start = #data[0]
        int@ end = #data[3]
        long diff = end - start
        assert diff == 3
    }
}

int main()
{
    unsafe
    {
        test_pointer_arithmetic_linear()
        test_double_pointer_arithmetic()
    }
    test_pointer_subtraction_types()
    return 0
}