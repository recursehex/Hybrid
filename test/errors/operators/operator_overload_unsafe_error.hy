// EXPECT_DIAGNOSTIC: Operator '@' requires unsafe context
// EXPECT_DIAGNOSTIC: Failed to generate IR for variable declaration
struct UnsafeOperator
{
    int value

    UnsafeOperator()
    {
        this.value = 5
    }

    int @()
    {
        return this.value
    }
}

UnsafeOperator value = UnsafeOperator()
int current = @value
