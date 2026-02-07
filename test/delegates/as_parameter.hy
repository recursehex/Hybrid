delegate int BinaryOp(int left, int right)

int add(int a, int b)
{
    return a + b
}

int apply(BinaryOp op, int a, int b)
{
    return op(a, b)
}

int main()
{
    int result = apply(add, 3, 4)
    if result != 7
    {
        return 1
    }
    return 0
}
