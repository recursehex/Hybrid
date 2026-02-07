// EXPECT_DIAGNOSTIC: Function 'Assign' requires explicit type arguments
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// Expect error when calling generic function without explicit type arguments

void Assign<T>(ref T target, T value)
{
    target = value
}

void main()
{
    int number = 0
    Assign(ref number, 7)
}
