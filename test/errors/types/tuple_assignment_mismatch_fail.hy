// EXPECT_DIAGNOSTIC: Tuple element 1 type mismatch for assignment to variable 'left': expected 'string', got 'int'
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
int main()
{
    (int, string) left = (1, "hi")
    (int, int) right = (2, 3)
    left = right
    return 0
}
