// EXPECT_DIAGNOSTIC: Too many arguments provided to call 'Box'
// EXPECT_DIAGNOSTIC: Failed to generate IR for variable declaration
class Box
{
    int width
    int height

    Box(int size)
    {
        this.width = size
        this.height = size
    }

    Box(int width, int height)
    {
        this.width = width
        this.height = height
    }
}

Box bad = Box("wide", "tall")  // expect: No matching overload found for call to 'Box'
