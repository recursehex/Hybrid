int add(int a, int b)
{
    return a + b
}

float add(float a, float b)
{
    return a + b
}

delegate int IntAdder(int a, int b)
delegate float FloatAdder(float a, float b)

int main()
{
    IntAdder ia = add
    FloatAdder fa = add
    int i = ia(2, 3)
    float f = fa(1.5, 2.5)
    if i != 5
    {
        return 1
    }
    if f != 4.0
    {
        return 2
    }
    return 0
}
