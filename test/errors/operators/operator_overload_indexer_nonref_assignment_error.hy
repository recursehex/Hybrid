// EXPECT_DIAGNOSTIC: Operator '[]' on type 'ReadOnlyBuffer' must return ref for indexed assignment
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
struct ReadOnlyBuffer
{
    int storage

    ReadOnlyBuffer()
    {
        this.storage = 0
    }

    int [](const ref int index)
    {
        return this.storage
    }
}

int main()
{
    ReadOnlyBuffer buffer = ReadOnlyBuffer()
    buffer[0] = 7
    return 0
}
