extern void print(int x)

int main()
{
    int a = 2
    int b = 3
    int c = 4
    int d = 5
    
    int result1 = a + b * c
    print(result1)
    
    int result2 = (a + b) * c
    print(result2)
    
    int result3 = a * b + c * d
    print(result3)
    
    int result4 = a + b - c + d
    print(result4)
    
    int result5 = a * b / c * d
    print(result5)
    
    bool flag1 = true
    bool flag2 = false
    bool flag3 = true
    
    bool logical1 = flag1 && flag2 || flag3
    print(int: logical1)
    
    bool logical2 = flag1 || flag2 && flag3
    print(int: logical2)
    
    bool comparison1 = a < b && c > d
    print(int: comparison1)
    
    bool comparison2 = a + b < c * d
    print(int: comparison2)
    
    bool comparison3 = a == b || c != d
    print(int: comparison3)
    
    int bitwise1 = 12 & 10 | 5
    print(bitwise1)
    
    int bitwise2 = 12 | 10 & 5
    print(bitwise2)
    
    int bitwise3 = 8 << 1 + 1
    print(bitwise3)
    
    int bitwise4 = (8 << 1) + 1
    print(bitwise4)
    
    int shift1 = a + b << c - d
    print(shift1)
    
    int shift2 = (a + b) << (c - d)
    print(shift2)
    
    int complex1 = a + b * c - d / 2
    print(complex1)
    
    int complex2 = a * b + c << 1 & 15
    print(complex2)
    
    bool complex3 = a < b && c > 0 || d == 5
    print(int: complex3)
    
    bool complex4 = (a < b) && (c > 0 || d == 5)
    print(int: complex4)
    
    int unary1 = -a + b
    print(unary1)
    
    int unary2 = a + -b
    print(unary2)
    
    bool unary3 = !flag1 || flag2
    print(int: unary3)
    
    bool unary4 = !(flag1 || flag2)
    print(int: unary4)
    
    int increment = 10
    int inc_result1 = ++increment * 2
    print(inc_result1)
    
    int inc_result2 = increment++ * 2
    print(inc_result2)
    
    int mixed = a + b * c > d && flag1 || !flag2
    print(int: mixed)
    
    return 0
}