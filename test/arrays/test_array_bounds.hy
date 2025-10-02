extern void print(int x)

void testBasicBounds()
{
    int[] small = [1]
    assert small[0] == 1
    print(small[0])

    int[] arr = [10, 20, 30, 40, 50]
    assert arr[0] == 10
    assert arr[4] == 50
    print(arr[0])
    print(arr[4])
}

void testLoopBounds()
{
    int[] numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    
    for int i = 0 to 9
    {
        print(numbers[i])
    }
    
    for int i = 9 to 0
    {
        print(numbers[i])
    }
}

void testExpressionIndexes()
{
    int[] data = [100, 200, 300, 400, 500]
    int index = 2

    assert data[index] == 300
    assert data[index + 1] == 400
    assert data[index - 1] == 200
    assert data[index * 2] == 500
    assert data[index / 2] == 200
    print(data[index])
    print(data[index + 1])
    print(data[index - 1])
    print(data[index * 2])
    print(data[index / 2])
}

void testArrayInFunction(int[] arr)
{
    for int i = 0 to 4
    {
        print(arr[i])
        arr[i] += 10
    }
}

// TODO: Add multidimensional arrays
// void testNestedArrayAccess()
// {
//     int[][] matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
//     
//     print(matrix[0][0])
//     print(matrix[1][2])
//     print(matrix[2][1])
// }

void testArrayWithDifferentTypes()
{
    byte[] bytes = [1, 2, 3, 4, 5]
    short[] shorts = [100, 200, 300]
    long[] longs = [1000, 2000, 3000, 4000]
    float[] floats = [1.1, 2.2, 3.3]
    
    print(bytes[4])
    print(shorts[2])
    print(int: longs[3])
    print(int: floats[1])
}

void testArrayModification()
{
    int[] arr = [0, 0, 0, 0, 0]
    
    for int i = 0 to 4
    {
        arr[i] = i * 10
        print(arr[i])
    }
    
    arr[2] *= 2
    arr[3] += 5
    arr[4] /= 2
    
    for int i = 0 to 4
    {
        print(arr[i])
    }
}

void testArrayInControlFlow()
{
    int[] values = [1, 5, 3, 8, 2, 9, 4, 7, 6]
    
    for int i = 0 to 8
    {
        if values[i] > 5
        {
            print(values[i])
        }
        else if values[i] < 3
        {
            print(-values[i])
        }
    }
}

void testLargeArray()
{
    // TODO: Add multiline literals
    int[] large = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
    
    print(large[0])
    print(large[10])
    print(large[19])
    
    int sum = 0
    for int i = 0 to 19
    {
        sum += large[i]
    }
    print(sum)
}

int main()
{
    testBasicBounds()
    testLoopBounds()
    testExpressionIndexes()
    
    int[] testArray = [1, 2, 3, 4, 5]
    testArrayInFunction(testArray)
    
    for int i = 0 to 4
    {
        print(testArray[i])
    }
    
    // testNestedArrayAccess()
    testArrayWithDifferentTypes()
    testArrayModification()
    testArrayInControlFlow()
    testLargeArray()
    
    return 0
}