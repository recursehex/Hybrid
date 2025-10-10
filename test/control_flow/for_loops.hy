// Comprehensive test for for loops in Hybrid
// Tests all phases: basic, anonymous, expressions, by keyword, operations, conditions, floats

extern void print(int x)

// Helper to print float as int (multiply by 10 for display)
void printF(float f)
{
    int i = int: (f * 10.0)
    print(i)
}

// =============================================================================
// PHASE 1: Basic for-to loops
// =============================================================================

void test_basic_forward()
{
    int expected = 1
    int sum = 0
    int count = 0
    for int i = 1 to 5
    {
        print(i)    // 1, 2, 3, 4, 5
        assert i == expected
        sum += i
        expected++
        count++
    }
    assert sum == 15
    assert count == 5
}

void test_basic_reverse()
{
    int expected = 10
    int sum = 0
    int count = 0
    for int i = 10 to 1
    {
        print(i)    // 10, 9, 8, 7, 6, 5, 4, 3, 2, 1
        assert i == expected
        sum += i
        expected--
        count++
    }
    assert sum == 55
    assert count == 10
}

void test_basic_zero_start()
{
    int expected = 0
    int sum = 0
    int count = 0
    for int i = 0 to 3
    {
        print(i)    // 0, 1, 2, 3
        assert i == expected
        sum += i
        expected++
        count++
    }
    assert sum == 6
    assert count == 4
}

void test_basic_variable_limit()
{
    int limit = 7
    int expected = 1
    int sum = 0
    int count = 0
    for int i = 1 to limit
    {
        print(i)  // 1, 2, 3, 4, 5, 6, 7
        assert i == expected
        sum += i
        expected++
        count++
    }
    assert sum == 28
    assert count == 7
}

void test_basic_nested()
{
    int sum = 0
    int count = 0
    for int i = 1 to 3
    {
        for int j = 1 to 2
        {
            print(i * 10 + j)  // 11, 12, 21, 22, 31, 32
            sum += i * 10 + j
            count++
        }
    }
    assert sum == 129
    assert count == 6
}

void test_basic_break()
{
    int expected = 1
    int sum = 0
    int count = 0
    for int i = 1 to 10
    {
        if i == 5
        {
            break
        }
        print(i)  // 1, 2, 3, 4
        assert i == expected
        sum += i
        expected++
        count++
    }
    assert sum == 10
    assert count == 4
}

void test_basic_skip() {
    int sum = 0
    int count = 0
    for int i = 1 to 6
    {
        if i % 2 == 0
        {
            skip
        }
        print(i)  // 1, 3, 5
        assert i % 2 == 1
        sum += i
        count++
    }
    assert sum == 9
    assert count == 3
}

// =============================================================================
// PHASE 2: Anonymous counter loops
// =============================================================================

void test_anonymous_simple()
{
    int iterations = 0
    int total = 0
    for 0 to 5
    {
        print(1111)  // prints 6 times (0, 1, 2, 3, 4, 5)
        iterations++
        total += 1111
    }
    assert iterations == 6
    assert total == 6666
}

void test_anonymous_range()
{
    int iterations = 0
    int total = 0
    for 1 to 3
    {
        print(2222)  // prints 3 times (1, 2, 3)
        iterations++
        total += 2222
    }
    assert iterations == 3
    assert total == 6666
}

void test_anonymous_reverse()
{
    int iterations = 0
    int total = 0
    for 5 to 1
    {
        print(3333)  // prints 5 times (5, 4, 3, 2, 1)
        iterations++
        total += 3333
    }
    assert iterations == 5
    assert total == 16665
}

void test_anonymous_expressions()
{
    int start = 2
    int end = 4
    int iterations = 0
    int total = 0
    for start to end
    {
        print(4444)  // prints 3 times (2, 3, 4)
        iterations++
        total += 4444
    }
    assert iterations == 3
    assert total == 13332
}

void test_anonymous_calculated()
{
    int iterations = 0
    int total = 0
    for 1 * 2 to 3 + 2
    {
        print(5555)  // From 2 to 5, prints 4 times
        iterations++
        total += 5555
    }
    assert iterations == 4
    assert total == 22220
}

// =============================================================================
// PHASE 4: 'by' keyword for custom steps
// =============================================================================

void test_step_by_2()
{
    int sum = 0
    int count = 0
    for int i = 0 to 10 by 2
    {
        print(i)  // 0, 2, 4, 6, 8, 10
        sum += i
        count++
    }
    assert sum == 30
    assert count == 6
}

void test_step_by_3()
{
    int sum = 0
    int count = 0
    for int i = 1 to 10 by 3
    {
        print(i)  // 1, 4, 7, 10
        sum += i
        count++
    }
    assert sum == 22
    assert count == 4
}

void test_negative_step()
{
    int sum = 0
    int count = 0
    for int i = 10 to 0 by -2
    {
        print(i)  // 10, 8, 6, 4, 2, 0
        sum += i
        count++
    }
    assert sum == 30
    assert count == 6
}

void test_variable_step()
{
    int step = 5
    int sum = 0
    int count = 0
    for int i = 0 to 20 by step
    {
        print(i)  // 0, 5, 10, 15, 20
        sum += i
        count++
    }
    assert sum == 50
    assert count == 5
}

void test_expression_step()
{
    int sum = 0
    int count = 0
    for int i = 0 to 15 by 2 + 1
    {
        print(i)  // 0, 3, 6, 9, 12, 15
        sum += i
        count++
    }
    assert sum == 45
    assert count == 6
}

void test_anonymous_with_step()
{
    int iterations = 0
    int total = 0
    for 0 to 12 by 4
    {
        print(6666)  // prints 4 times (0, 4, 8, 12)
        iterations++
        total += 6666
    }
    assert iterations == 4
    assert total == 26664
}

// =============================================================================
// PHASE 5: Multiplicative/divisive steps
// =============================================================================

void test_multiply_step()
{
    int sum = 0
    int count = 0
    for int i = 1 to 100 by * 2
    {
        print(i)  // 1, 2, 4, 8, 16, 32, 64
        sum += i
        count++
    }
    assert sum == 127
    assert count == 7
}

void test_divide_step()
{
    int sum = 0
    int count = 0
    for int i = 100 to 1 by / 2
    {
        print(i)  // 100, 50, 25, 12, 6, 3, 1
        sum += i
        count++
    }
    assert sum == 197
    assert count == 7
}

void test_multiply_by_3()
{
    int sum = 0
    int count = 0
    for int i = 1 to 81 by * 3
    {
        print(i)  // 1, 3, 9, 27, 81
        sum += i
        count++
    }
    assert sum == 121
    assert count == 5
}

void test_multiply_variable()
{
    int factor = 2
    int sum = 0
    int count = 0
    for int i = 1 to 64 by * factor
    {
        print(i)  // 1, 2, 4, 8, 16, 32, 64
        sum += i
        count++
    }
    assert sum == 127
    assert count == 7
}

void test_divide_by_10()
{
    int sum = 0
    int count = 0
    for int i = 1000 to 1 by / 10
    {
        print(i)  // 1000, 100, 10, 1
        sum += i
        count++
    }
    assert sum == 1111
    assert count == 4
}

void test_subtract_step()
{
    int sum = 0
    int count = 0
    for int i = 20 to 5 by - 3
    {
        print(i)  // 20, 17, 14, 11, 8, 5
        sum += i
        count++
    }
    assert sum == 75
    assert count == 6
}

// =============================================================================
// PHASE 6: Exclusive bounds with comparison
// =============================================================================

void test_exclusive_less_than()
{
    int size = 5
    int sum = 0
    int count = 0
    for int i = 0 to i < size
    {
        print(i)  // 0, 1, 2, 3, 4 (not 5)
        sum += i
        count++
    }
    assert sum == 10
    assert count == 5
}

void test_exclusive_less_equal()
{
    int limit = 5
    int sum = 0
    int count = 0
    for int i = 0 to i <= limit
    {
        print(i)  // 0, 1, 2, 3, 4, 5
        sum += i
        count++
    }
    assert sum == 15
    assert count == 6
}

void test_exclusive_greater()
{
    int sum = 0
    int count = 0
    for int i = 10 to i > 5
    {
        print(i)  // 10, 9, 8, 7, 6 (not 5)
        sum += i
        count++
    }
    assert sum == 40
    assert count == 5
}

void test_exclusive_greater_equal()
{
    int sum = 0
    int count = 0
    for int i = 10 to i >= 5
    {
        print(i)  // 10, 9, 8, 7, 6, 5
        sum += i
        count++
    }
    assert sum == 45
    assert count == 6
}

void test_exclusive_with_step()
{
    int size = 10
    int sum = 0
    int count = 0
    for int i = 0 to i < size by 2
    {
        print(i)  // 0, 2, 4, 6, 8
        sum += i
        count++
    }
    assert sum == 20
    assert count == 5
}

void test_exclusive_expression()
{
    int limit = 5
    int sum = 0
    int count = 0
    for int i = 0 to i < limit * 2
    {
        print(i)  // 0 through 9
        sum += i
        count++
    }
    assert sum == 45
    assert count == 10
}

void test_not_equal()
{
    int sum = 0
    int count = 0
    for int i = 0 to i != 5
    {
        print(i)  // 0, 1, 2, 3, 4
        sum += i
        count++
    }
    assert sum == 10
    assert count == 5
}

void test_exclusive_multiply_step()
{
    int sum = 0
    int count = 0
    for int i = 1 to i < 100 by * 2
    {
        print(i)  // 1, 2, 4, 8, 16, 32, 64
        sum += i
        count++
    }
    assert sum == 127
    assert count == 7
}

// =============================================================================
// PHASE 7: Float support
// =============================================================================

void test_float_basic()
{
    int expected = 0
    int sum = 0
    int count = 0
    for float f = 0.0 to 1.0 by 0.1
    {
        printF(f)  // 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
        int scaled = int: ((f * 10.0) + 0.1)
        assert scaled == expected
        sum += scaled
        count++
        expected++
    }
    assert sum == 55
    assert count == 11
}

void test_float_step()
{
    int sum = 0
    int count = 0
    for float f = 0.0 to 2.0 by 0.5
    {
        printF(f)  // 0, 5, 10, 15, 20
        int scaled = int: ((f * 10.0) + 0.1)
        sum += scaled
        count++
    }
    assert sum == 50
    assert count == 5
}

void test_float_reverse()
{
    int sum = 0
    int count = 0
    for float f = 5.0 to 0.0 by -1.0
    {
        printF(f)  // 50, 40, 30, 20, 10, 0
        int scaled = int: ((f * 10.0) + 0.1)
        sum += scaled
        count++
    }
    assert sum == 150
    assert count == 6
}

void test_float_variable()
{
    float limit = 3.0
    int sum = 0
    int count = 0
    for float f = 0.0 to limit by 0.75
    {
        printF(f)  // 0, 7, 15, 22, 30
        int scaled = int: ((f * 10.0) + 0.1)
        sum += scaled
        count++
    }
    assert sum == 74
    assert count == 5
}

void test_float_expression_step()
{
    float step = 0.25
    int sum = 0
    int count = 0
    for float f = 0.0 to 1.0 by step * 2.0
    {
        printF(f)  // 0, 5, 10
        int scaled = int: ((f * 10.0) + 0.1)
        sum += scaled
        count++
    }
    assert sum == 15
    assert count == 3
}

void test_float_exclusive()
{
    int sum = 0
    int count = 0
    for float f = 0.0 to f < 1.0 by 0.2
    {
        printF(f)  // 0, 2, 4, 6, 8
        int scaled = int: ((f * 10.0) + 0.1)
        sum += scaled
        count++
    }
    assert sum == 20
    assert count == 5
}

void test_float_multiply()
{
    int sum = 0
    int count = 0
    for float f = 1.0 to 32.0 by * 2.0
    {
        printF(f)  // 10, 20, 40, 80, 160, 320
        int scaled = int: ((f * 10.0) + 0.1)
        sum += scaled
        count++
    }
    assert sum == 630
    assert count == 6
}

void test_float_divide()
{
    int sum = 0
    int count = 0
    for float f = 100.0 to 1.0 by / 2.0
    {
        printF(f)  // 1000, 500, 250, 125, 62, 31, 15, 7, 3, 1
        int scaled = int: ((f * 10.0) + 0.1)
        sum += scaled
        count++
    }
    assert sum == 1994
    assert count == 10
}

void test_double()
{
    int sum = 0
    int count = 0
    for double d = 0.0 to 1.0 by 0.25
    {
        int i = int: (d * 100.0)
        print(i)  // 0, 25, 50, 75, 100
        sum += i
        count++
    }
    assert sum == 250
    assert count == 5
}

// =============================================================================
// COMPLEX COMBINATIONS
// =============================================================================

void test_mixed_int_float()
{
    int sum = 0
    int count = 0
    for int i = 0 to 3 {
        for float f = 0.0 to 1.0 by 0.5
        {
            int scaled = int: ((f * 10.0) + 0.1)
            int value = i * 100 + scaled
            print(value)  // 0, 5, 10, 100, 105, 110, etc.
            sum += value
            count++
        }
    }
    assert sum == 1860
    assert count == 12
}

void test_nested_operations()
{
    int sum = 0
    int count = 0
    for int i = 1 to 8 by * 2
    {
        for int j = 100 to 10 by / 2
        {
            int value = i * 1000 + j
            print(value)  // 1100, 1050, 1025, 1012, 2100, 2050, etc.
            sum += value
            count++
        }
    }
    assert sum == 105788
    assert count == 28
}

void test_all_types()
{
    // Test different integer types
    int sum = 0
    int count = 0
    for byte b = 0 to 3
    {
        print(b)
        sum += int: b
        count++
    }
    
    for short s = 0 to 2
    {
        print(s)
        sum += int: s
        count++
    }
    
    for long l = 0 to 2
    {
        int converted = int: l
        print(converted)
        sum += converted
        count++
    }
    assert sum == 12
    assert count == 10
}

void test_complex_conditions()
{
    int max = 10
    int sum = 0
    int count = 0
    for int i = 0 to i < max && i != 5
    {
        if i == 3
        {
            skip
        }
        print(i)  // 0, 1, 2, 4
        sum += i
        count++
    }
    assert sum == 7
    assert count == 4
}

// =============================================================================
// MAIN TEST RUNNER
// =============================================================================

int main()
{
    // Phase 1: Basic loops
    test_basic_forward()
    test_basic_reverse()
    test_basic_zero_start()
    test_basic_variable_limit()
    test_basic_nested()
    test_basic_break()
    test_basic_skip()
    
    // Phase 2: Anonymous loops
    test_anonymous_simple()
    test_anonymous_range()
    test_anonymous_reverse()
    test_anonymous_expressions()
    test_anonymous_calculated()
    
    // Phase 4: Steps
    test_step_by_2()
    test_step_by_3()
    test_negative_step()
    test_variable_step()
    test_expression_step()
    test_anonymous_with_step()
    
    // Phase 5: Operations
    test_multiply_step()
    test_divide_step()
    test_multiply_by_3()
    test_multiply_variable()
    test_divide_by_10()
    test_subtract_step()
    
    // Phase 6: Conditions
    test_exclusive_less_than()
    test_exclusive_less_equal()
    test_exclusive_greater()
    test_exclusive_greater_equal()
    test_exclusive_with_step()
    test_exclusive_expression()
    test_not_equal()
    test_exclusive_multiply_step()
    
    // Phase 7: Floats
    test_float_basic()
    test_float_step()
    test_float_reverse()
    test_float_variable()
    test_float_expression_step()
    test_float_exclusive()
    test_float_multiply()
    test_float_divide()
    test_double()
    
    // Complex combinations
    test_mixed_int_float()
    test_nested_operations()
    test_all_types()
    test_complex_conditions()
    
    return 0
}