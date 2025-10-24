// Exercise function overloading by arity and type.
int add(int a, int b)
{
    return a + b
}

double add(double a, double b)
{
    return a + b
}

int add(int a, int b, int c)
{
    return a + b + c
}

int main() 
{
    int twoArgInt = add(3, 4)
    double twoArgDouble = add(1.5, 2.0)
    int threeArgInt = add(1, 2, 3)

    if twoArgInt != 7
    {
        return 1
    }

    if twoArgDouble != 3.5 
    {
        return 2
    }

    if threeArgInt != 6 
    {
        return 3
    }

    return 0
}