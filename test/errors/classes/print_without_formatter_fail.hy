class Plain
{
    int value

    Plain(int value)
    {
        this.value = value
    }
}

void printPlain()
{
    Plain p = (1)
    print(p)  // expect: Cannot print value of type 'Plain' because it does not implement string this()
}