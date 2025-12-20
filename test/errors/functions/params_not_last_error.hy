// EXPECT_OUTPUT: params parameter must be the final parameter in the list
int bad(int a, params int[] rest, int b)
{
    return a
}
