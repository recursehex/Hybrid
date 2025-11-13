// High-volume generics stress test that exercises nested instantiations.

class Pair<T, U>
{
    T first
    U second

    Pair(T first, U second)
    {
        this.first = first
        this.second = second
    }
}

class Bucket<T>
{
    T value

    Bucket(T value)
    {
        this.value = value
    }
}

class Matrix<T>
{
    T upperLeft
    T lowerRight

    Matrix(T upperLeft, T lowerRight)
    {
        this.upperLeft = upperLeft
        this.lowerRight = lowerRight
    }
}

class Chain<T, U, V>
{
    T first
    U second
    V third

    Chain(T first, U second, V third)
    {
        this.first = first
        this.second = second
        this.third = third
    }
}

int main()
{
    Pair<int, int> pairA = (1, 2)
    assert pairA.first + pairA.second == 3

    Pair<int, string> pairLabel = (3, "three")
    assert pairLabel.first == 3

    Bucket<int> bucketA = (11)
    assert bucketA.value == 11

    Matrix<int> matrixA = (7, 9)
    assert matrixA.lowerRight == 9

    Chain<int, int, int> chainA = (1, 2, 3)
    assert chainA.third == 3

    Pair<Pair<int, int>, Pair<int, int>> nestedPair = (Pair<int, int>(5, 6), Pair<int, int>(7, 8))
    assert nestedPair.first.second == 6
    assert nestedPair.second.first == 7

    Bucket<Pair<Pair<int, int>, Pair<int, int>>> nestedBucket = (nestedPair)
    assert nestedBucket.value.first.first == 5

    Matrix<Pair<Pair<int, int>, Pair<int, int>>> nestedMatrix = (nestedPair, nestedPair)
    assert nestedMatrix.upperLeft.first.second == 6

    Chain<Pair<int, string>, Pair<Pair<int, int>, Pair<int, int>>, Matrix<Pair<Pair<int, int>, Pair<int, int>>>> cascade = (Pair<int, string>(9, "nine"), nestedPair, nestedMatrix)
    assert cascade.second.second.second == 8
    assert cascade.third.lowerRight.second.first == 7

    return 0
}