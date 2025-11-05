extern void print(int x)
extern void putchar(char c)
extern int abs(int x)
extern double sin(double x)
extern int hybrid_strlen(string s)

void testBasicExtern()
{
    int total = 0
    total += 42
    print(total)
    total += -17
    print(total)
    total += 0
    print(total)
    assert total == 25
}

void testExternWithExpressions()
{
    int a = 10
    int b = 20
    int sum = a + b
    int product = a * b
    int diff = b - a
    print(sum)
    print(product)
    print(diff)
    assert sum == 30
    assert product == 200
    assert diff == 10
}

void testExternWithFunctionCalls()
{
    int negative = -15
    int positive = abs(negative)
    print(positive)
    assert positive == 15
    
    int zero = abs(0)
    print(zero)
    assert zero == 0
    
    int alreadyPositive = abs(25)
    print(alreadyPositive)
    assert alreadyPositive == 25
}

void testExternWithArrays()
{
    int[] numbers = [1, 2, 3, 4, 5]
    int sum = 0
    for int i = 0 to 4
    {
        int current = numbers[i]
        print(current)
        sum += current
    }
    assert sum == 15
}

void testExternWithStrings()
{
    string hello = "Hello"
    string world = "World!"
    string empty = ""
    
    int len1 = hybrid_strlen(hello)
    int len2 = hybrid_strlen(world)
    int len3 = hybrid_strlen(empty)
    
    print(len1)
    print(len2)
    print(len3)
    assert len1 == 5
    assert len2 == 6
    assert len3 == 0
}

void testExternWithCharacters()
{
    char letter = 'A'
    char digit = '7'
    char space = ' '
    
    putchar(letter)
    putchar(digit)
    putchar(space)
    putchar('\n')
    assert letter == 'A'
    assert digit == '7'
    assert space == ' '
}

void testExternWithFloats()
{
    double pi = 3.14159
    double angle = 0.5
    
    double result1 = sin(pi)
    double result2 = sin(angle)
    
    int converted1 = int: (result1 * 1000)
    int converted2 = int: (result2 * 1000)
    print(converted1)
    print(converted2)
    assert converted1 == 0
    assert converted2 == 479
}

void testExternInControlFlow()
{
    int sum = 0
    for int i = 1 to 5
    {
        if i % 2 == 0
        {
            print(i)
            sum += i
        }
        else
        {
            print(-i)
            sum -= i
        }
    }
    assert sum == -3
}

void testExternWithComplexExpressions()
{
    int initial = 10
    int offset = 5
    
    int first = abs(initial - 20) + offset
    int second = abs(-initial) * 2
    
    string text = "Testing"
    int third = hybrid_strlen(text) + initial
    print(first)
    print(second)
    print(third)
    assert first == 15
    assert second == 20
    assert third == 17
}

int main()
{
    testBasicExtern()
    testExternWithExpressions()
    testExternWithFunctionCalls()
    testExternWithArrays()
    testExternWithStrings()
    testExternWithCharacters()
    testExternWithFloats()
    testExternInControlFlow()
    testExternWithComplexExpressions()
    
    return 0
}
