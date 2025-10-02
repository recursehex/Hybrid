extern void print(int x)

void testIntToFloat()
{
    int i = 42
    float f = 3.14

    float result1 = i + f
    float result2 = f * i
    float result3 = i / f

    // Assert expected results of type promotion
    assert int: result1 == 45  // 42 + 3.14 ≈ 45.14, truncated to 45
    assert int: result2 == 131  // 3.14 * 42 ≈ 131.88, truncated to 131
    assert int: (result3 * 10) == 133  // 42 / 3.14 ≈ 13.375, * 10 ≈ 133.75, truncated to 133

    print(int: result1)
    print(int: result2)
    print(int: (result3 * 10))
}

void testIntToDouble()
{
    int i = 25
    double d = 2.718
    
    double result1 = i + d
    double result2 = d - i
    double result3 = i * d
    
    print(int: result1)
    print(int: result2)
    print(int: result3)
}

void testFloatToDouble()
{
    float f = 1.5
    double d = 2.5
    
    double result1 = f + d
    double result2 = d / f
    double result3 = f * d
    
    print(int: (result1 * 10))
    print(int: (result2 * 10))
    print(int: (result3 * 10))
}

void testSameTypeOperations()
{
    int i1 = 10
    int i2 = 20
    int i_result = i1 + i2

    float f1 = 1.5
    float f2 = 2.5
    float f_result = f1 * f2

    double d1 = 3.14
    double d2 = 2.718
    double d_result = d1 - d2

    // Assert expected results for same-type operations
    assert i_result == 30
    assert int: f_result == 3  // 1.5 * 2.5 = 3.75, truncated to 3
    assert int: (d_result * 100) == 42  // (3.14 - 2.718) * 100 = 42.2, truncated to 42

    print(i_result)
    print(int: f_result)
    print(int: (d_result * 100))
}

void testPromotionInExpressions()
{
    int i = 10
    float f = 2.5
    double d = 1.5
    
    double complex1 = i + f + d
    double complex2 = i * f / d
    double complex3 = (i + f) * d
    
    print(int: complex1)
    print(int: complex2)
    print(int: complex3)
}

float mixedAdd(int a, float b)
{
    return a + b
}

double mixedMultiply(float f, double d)
{
    return f * d
}

void testPromotionInFunctions()
{
    float result1 = mixedAdd(5, 2.5)
    double result2 = mixedMultiply(3.0, 4.5)
    
    print(int: result1)
    print(int: result2)
}

void testPromotionInArrays()
{
    int[] ints = [1, 2, 3]
    float[] floats = [1.5, 2.5, 3.5]
    
    float mixed1 = ints[0] + floats[0]
    float mixed2 = ints[1] * floats[1]
    float mixed3 = floats[2] - ints[2]
    
    print(int: (mixed1 * 10))
    print(int: (mixed2 * 10))
    print(int: (mixed3 * 10))
}

void testPromotionInLoops()
{
    int[] ints = [1, 2, 3, 4, 5]
    float multiplier = 1.5
    
    for int i = 0 to 4
    {
        float result = ints[i] * multiplier
        print(int: (result * 10))
    }
}

void testPromotionWithCasting()
{
    int i = 42
    float f = 3.14
    double d = 2.718
    
    int back_to_int1 = int: (i + f)
    int back_to_int2 = int: (f * d)
    float to_float = float: (i + d)
    
    print(back_to_int1)
    print(back_to_int2)
    print(int: (to_float * 10))
}

void testPromotionInComparisons()
{
    int i = 5
    float f = 5.0
    double d = 5.1

    bool eq1 = (i == f)
    bool eq2 = (f < d)
    bool eq3 = (i <= d)

    // Assert expected comparison results with type promotion
    assert eq1 == true   // int 5 should equal float 5.0
    assert eq2 == true   // float 5.0 should be less than double 5.1
    assert eq3 == true   // int 5 should be less than or equal to double 5.1

    print(int: eq1)
    print(int: eq2)
    print(int: eq3)
}

int main()
{
    testIntToFloat()
    testIntToDouble()
    testFloatToDouble()
    testSameTypeOperations()
    testPromotionInExpressions()
    testPromotionInFunctions()
    testPromotionInArrays()
    testPromotionInLoops()
    testPromotionWithCasting()
    testPromotionInComparisons()
    
    return 0
}