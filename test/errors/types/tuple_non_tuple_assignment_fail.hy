// EXPECT_DIAGNOSTIC: Cannot use tuple value of type 'tuple<int,string>' for assignment to variable 'x' expecting 'int'
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
int main()
{
    (int, string) pair = (1, "hi")
    int x = 0
    x = pair
    return 0
}
