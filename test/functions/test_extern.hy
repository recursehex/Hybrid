extern void print(int x)
extern void putchar(char c)
extern int abs(int x)
extern double sin(double x)
extern int strlen(string s)

void testBasicExtern()
{
    print(42)
    print(-17)
    print(0)
}

void testExternWithExpressions()
{
    int a = 10
    int b = 20
    print(a + b)
    print(a * b)
    print(b - a)
}

void testExternWithFunctionCalls()
{
    int negative = -15
    int positive = abs(negative)
    print(positive)
    
    int zero = abs(0)
    print(zero)
    
    int alreadyPositive = abs(25)
    print(alreadyPositive)
}

void testExternWithArrays()
{
    int[] numbers = [1, 2, 3, 4, 5]
    for int i = 0 to 4
    {
        print(numbers[i])
    }
}

void testExternWithStrings()
{
    string hello = "Hello"
    string world = "World!"
    string empty = ""
    
    int len1 = strlen(hello)
    int len2 = strlen(world)
    int len3 = strlen(empty)
    
    print(len1)
    print(len2)
    print(len3)
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
}

void testExternWithFloats()
{
    double pi = 3.14159
    double angle = 0.5
    
    double result1 = sin(pi)
    double result2 = sin(angle)
    
    print(int: (result1 * 1000))
    print(int: (result2 * 1000))
}

void testExternInControlFlow()
{
    for int i = 1 to 5
    {
        if i % 2 == 0
        {
            print(i)
        }
        else
        {
            print(-i)
        }
    }
}

void testExternWithComplexExpressions()
{
    int base = 10
    int offset = 5
    
    print(abs(base - 20) + offset)
    print(abs(-base) * 2)
    
    string text = "Testing"
    print(strlen(text) + base)
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