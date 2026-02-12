// EXPECT_DIAGNOSTIC: Cannot free smart pointer values directly
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// free on smart pointer handle should be rejected

class Thing
{
    Thing() {}
}

int main()
{
    shared<Thing> handle = shared<Thing>()
    free handle
    return 0
}
