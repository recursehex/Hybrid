# Multidimensional vs Jagged Arrays

Hybrid supports both rectangular (multidimensional) arrays and jagged arrays. Use the form that best matches the shape of your data.

| Feature | Multidimensional Array | Jagged Array |
| ------- | ---------------------- | ------------ |
| Structure | A single, rectangular block of memory. | An array of arrays, where each inner array can be a different size. |
| Declaration | Uses commas inside the `[]` (e.g. `int[,]`, `float[,,]`). | Chains successive `[]` (e.g. `int[][]`, `string[][][]`). |
| Storage | One contiguous allocation that stores all elements. | Each inner array is allocated separately. |
| Size Consistency | Every row/plane must have identical length. | Each row decides its own length. |
| Size (`.size`) | Total number of elements across all dimensions. | Number of rows in the outer array (use `.size` on inner arrays for row lengths). |
| Indexing Syntax | Comma-separated indices: `matrix[row, column]`. | Nested brackets: `triangle[row][column]`. |
| Bounds Behaviour | Compiler tracks every dimension size; runtime checks guard each axis. | Each access first checks the outer array, then the chosen inner array. |
| Performance | Fastest traversal thanks to cache-friendly contiguous storage. | Slightly more indirection but ideal for ragged data. |

## Rectangular Arrays

```c
int[,] matrix = [[1, 2], [3, 4]]
float[,,] volume = [
    [[0.0, 0.1], [0.2, 0.3]],
    [[0.4, 0.5], [0.6, 0.7]]
]

int center = volume[0, 1, 1]
matrix[1, 0] = 9
```

- The compiler enforces rectangular literals. Mismatched row lengths produce `Multidimensional array initializer must be rectangular`.
- Every dimension length is recorded so both compile-time and runtime bounds checks understand each axis.

## Jagged Arrays

```c
int[][] triangle = [
    [1],
    [2, 3],
    [4, 5, 6]
]

triangle[2][2] = 42
```

- You can mix jagged and rectangular layers (e.g. `int[][,,]` is an array of 3D blocks).
- Bounds checking happens per level: `triangle[2]` validates the outer array, `triangle[2][2]` then validates the selected inner array.

Choose rectangular arrays when the shape is fixed-matrix math, chess boards, pixel buffers, and pick jagged arrays when rows vary in length, such as adjacency lists, ragged time-series logs, or grouped results.
