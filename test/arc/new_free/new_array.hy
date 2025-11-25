int main()
{
    int[] values = new int[3]
    values[0] = 1
    values[1] = 2
    values[2] = 3

    int[] inferred = new[2]
    inferred[0] = 4
    inferred[1] = 5

    return values[0] + values[1] + values[2] + inferred[0] + inferred[1] - 15
}