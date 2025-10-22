extern void print(int x)

struct Point
{
    int x
    int y
    
    Point(int x, int y)
    {
        this.x = x
        this.y = y
    }
}

struct Shape
{
    Point center
    int size
    bool visible
    
    Shape(Point c, int s, bool v)
    {
        this.center = c
        this.size = s
        this.visible = v
    }
}

int fibonacci(int n)
{
    if n <= 1
    {
        return n
    }
    return fibonacci(n - 1) + fibonacci(n - 2)
}

void processArray(int[] arr)
{
    for int i = 0 to i < 5
    {
        arr[i] *= 2
        arr[i] += 1
    }
}

int main()
{
    byte b = 255
    short s = 1000
    int i = 42
    long l = 1000000
    float f = 3.14
    double d = 2.718
    bool flag = true
    string msg = "Hybrid"
    char c = 'A'
    
    int[] numbers = [1, 2, 3, 4, 5]
    float[] temps = [98.6, 100.0, 99.2]
    
    processArray(numbers)
    assert numbers[0] == 3
    assert numbers[1] == 5
    assert numbers[2] == 7
    assert numbers[3] == 9
    assert numbers[4] == 11
    
    Point origin = Point(0, 0)
    Point target = Point(10, 20)
    Shape box = Shape(origin, 50, true)
    
    int distance = target.x - origin.x + target.y - origin.y
    assert distance == 30
    
    int fibSum = 0
    for int n = 0 to 8
    {
        int fib = fibonacci(n)
        fibSum += fib
        print(fib)
    }
    assert fibonacci(8) == 21
    assert fibSum == 54
    
    int highTemps = 0
    for float temp in temps
    {
        if temp > 99.0
        {
            print(int: temp)
            highTemps++
        }
    }
    assert highTemps == 2
    
    int sum = 0
    int processedCount = 0
    for int num in numbers
    {
        sum += num
        processedCount++
        if sum > 10
        {
            break
        }
    }
    assert sum == 15
    assert processedCount == 3
    
    int counter = 0
    int oddCount = 0
    while counter < 10
    {
        counter++
        if counter % 2 == 0
        {
            skip
        }
        oddCount++
        print(counter)
    }
    assert oddCount == 5
    assert counter == 10

    int bitwise = 0b1010
    bitwise &= 0b1100
    bitwise |= 0b0011
    bitwise ^= 0b1111
    bitwise = 15
    
    int result = (i + int: s) * int: f / (int: b + 1)
    bool complex_condition = flag && (result > 100) || (!flag && sum < 5)
    
    if complex_condition
    {
        return 1
    }
    else
    {
        assert result == 12
        assert complex_condition == false
        return 0
    }
}