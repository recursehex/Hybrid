// EXPECT_DIAGNOSTIC: The 'free' keyword requires a reference value
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
class Box
{
    int value

    Box(int value)
    {
        this.value = value
    }
}

int main()
{
    Box instance = Box(42)
    int value = instance.value
    free value   // stack value cannot be freed
    return 0
}
