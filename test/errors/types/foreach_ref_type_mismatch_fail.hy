// EXPECT_DIAGNOSTIC: ref foreach variable type must match collection element type
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
int[] numbers = [1, 2, 3]

void foreachRefTypeMismatch()
{
    for ref float value in numbers
    {
        value += 1.0
    }
}
