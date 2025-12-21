// Assigning an array literal with a different length should fail.
int main()
{
    int[] values = [1, 2, 3]
    values = [1, 2]
    return 0
}
