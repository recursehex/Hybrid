// EXPECT_DIAGNOSTIC: Expected identifier after type
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
