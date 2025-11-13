// Parser stress test: nested templates interleaved with shift/comparison operators.

int decrease(int value)
{
    Pair<int, int> coords = Pair<int, int>(value << 1, value >> 1)
    return coords.first - coords.second
}

int main()
{
    Pair<int, string> cell = Pair<int, string>(3, "row")
    List<Pair<int, string>> rows = List<Pair<int, string>>(cell)
    Matrix<List<Pair<int, string>>> grid = Matrix<List<Pair<int, string>>>(rows)
    BitBox<Matrix<List<Pair<int, string>>>> wrapper = BitBox<Matrix<List<Pair<int, string>>>>(grid)

    int pivot = (cell.first << 2) + (cell.first >> 1)
    assert pivot == (3 << 2) + (3 >> 1)

    // Comparison with nested template closings and a shift that follows immediately.
    bool ordered = cell.first < BitBox<List<Pair<int, string>>>(rows).Value().head.first >> 1
    assert ordered == (3 < (rows.head.first >> 1))

    // Use a wider shift so the subsequent >> 1 comparison still observes a larger rhs.
    Matrix<Pair<int, int>> magnified = Matrix<Pair<int, int>>(Pair<int, int>(pivot, pivot << 2))
    bool nested = magnified.entry.first < magnified.entry.second >> 1
    assert nested

    // Mix template closings and shifts in a single expression to stress token replay.
    int cascade = decrease(wrapper.Value().Echo().head.first << 1)
    assert cascade == decrease((rows.head.first << 1))

    return 0
}