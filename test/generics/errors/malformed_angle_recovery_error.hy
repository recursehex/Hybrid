// EXPECT_DIAGNOSTIC: destination of '=' must be a variable, array element, struct member, or dereferenced pointer
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// Expect the parser to reject stray '>' tokens around nested generic types.

class Box<T>
{
    T value

    Box(T value)
    {
        this.value = value
    }
}

void main()
{
    Box<int>> invalid = Box<int>(0)
}
