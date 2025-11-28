// Array literal should infer element types from expressions instead of defaulting to int.
int pick(int[] values)
{
    return 1
}

int pick(string[] values)
{
    return 0
}

int main()
{
    string name = "hybrid"
    return pick([name])
}