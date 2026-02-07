int main()
{
    int[] numbers = [1, 2, 3, 4]
    int[][] jagged = [
        [10, 20],
        [30, 40, 50]
    ]
    int[,] multi = [[1, 2], [3, 4]]

    assert numbers.size == 4
    assert jagged.size == 2
    assert multi.size == 4
    return 0
}