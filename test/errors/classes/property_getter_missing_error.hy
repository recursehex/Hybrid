// EXPECT_DIAGNOSTIC: Property 'value' does not define a getter
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// EXPECT_OUTPUT: Property 'value' does not define a getter
class OnlySet
{
    static int value = 0
    {
        set
    }

    OnlySet()
    {
    }
}

int main()
{
    int current = OnlySet.value
    return current
}
