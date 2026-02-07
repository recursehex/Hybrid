// EXPECT_DIAGNOSTIC: Property 'value' does not define a setter
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// EXPECT_OUTPUT: Property 'value' does not define a setter
class OnlyGet
{
    static int value = 0
    {
        get
    }

    OnlyGet()
    {
    }
}

int main()
{
    OnlyGet.value = 3   // cannot modify read-only property
    return 0
}
