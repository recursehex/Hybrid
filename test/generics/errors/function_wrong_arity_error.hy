// EXPECT_DIAGNOSTIC: Function 'Assign' expects 1 type argument(s)
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// Expect diagnostic when providing wrong number of type arguments

void Assign<T>(ref T target, T value)
{
    target = value
}

void main()
{
    int number = 0
    Assign<int, string>(ref number, 5)
}
