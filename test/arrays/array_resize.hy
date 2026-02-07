int main()
{
    int[] values = [1, 2, 3]
    values = new[5]
    assert values.size == 5
    assert values[0] == 1
    assert values[2] == 3
    assert values[3] == 0

    values = new int[2]
    assert values.size == 2
    assert values[0] == 1
    assert values[1] == 2

    int[][] rows = new[2]
    rows[0] = new[2]
    rows[0][0] = 7        // jagged indexing: first row, first column
    rows[0][1] = 8        // jagged indexing: first row, second column
    rows[0] = new[3]
    assert rows[0].size == 3
    assert rows[0][0] == 7 // resized row retains earlier elements
    assert rows[0][1] == 8 // retained element after row resize
    assert rows[0][2] == 0

    int[,] grid = new[2, 2]
    grid[0, 0] = 11       // rectangular indexing: row, column
    grid[0, 1] = 12
    grid[1, 0] = 21
    grid[1, 1] = 22
    grid = new[3, 2]
    assert grid.size == 6
    assert grid[0, 0] == 11 // retained after rectangular resize
    assert grid[0, 1] == 12
    assert grid[1, 0] == 21
    assert grid[1, 1] == 22

    int[,] grid2 = new int[1, 3]
    grid2[0, 2] = 5
    assert grid2.size == 3

    return 0
}
