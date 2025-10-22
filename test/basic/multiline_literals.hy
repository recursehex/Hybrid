// Multiline literal parsing tests covering calls, definitions, arrays, and constructors
extern void print(int x)

int multilineAdd(
    int a,
    int b,
    int c
)
{
    return a + b + c
}

struct Vector2
{
    int x
    int y

    Vector2(
        int x,
        int y
    )
    {
        this.x = x
        this.y = y
    }
}

void configureVector(
    Vector2 target,
    int baseValue,
    int extraX,
    int extraY
)
{
    target.x = multilineAdd(
        baseValue,
        extraX,
        0
    )
    target.y = multilineAdd(
        baseValue,
        0,
        extraY
    )
}

int main()
{
    int computed = multilineAdd(
        1,
        2,
        3
    )
    assert computed == 6

    Vector2 offset = Vector2(
        0,
        0
    )

    configureVector(
        offset,
        computed,
        multilineAdd(
            0,
            1,
            1
        ),
        4
    )

    assert offset.x == 8
    assert offset.y == 10

    int[] values = [
        computed,
        offset.x,
        offset.y,
        multilineAdd(
            offset.x,
            offset.y,
            1
        )
    ]

    assert values[0] == 6
    assert values[1] == 8
    assert values[2] == 10
    assert values[3] == 19

    int[,] grid = [
        [
            values[0],
            values[1]
        ],
        [
            multilineAdd(
                1,
                1,
                1
            ),
            offset.y
        ]
    ]

    assert grid[0, 0] == 6
    assert grid[0, 1] == 8
    assert grid[1, 0] == 3
    assert grid[1, 1] == 10

    int[][] jagged = [
        [
            0,
            1
        ],
        [
            offset.x,
            multilineAdd(
                0,
                0,
                offset.y
            ),
            values[3]
        ]
    ]

    assert jagged[0][1] == 1
    assert jagged[1][0] == 8
    assert jagged[1][1] == 10
    assert jagged[1][2] == 19

    print(values[3])
    print(grid[1, 0])
    print(jagged[1][2])

    return 0
}