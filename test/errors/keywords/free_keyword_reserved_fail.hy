// EXPECT_DIAGNOSTIC: Value 'counter' was manually released after free and cannot be used afterwards
// EXPECT_DIAGNOSTIC: The 'free' keyword requires a reference value
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
int main()
{
    int counter = 7
    free counter
    return counter
}
