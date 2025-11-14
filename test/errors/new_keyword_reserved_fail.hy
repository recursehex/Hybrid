class Box
{
    int value

    Box(int value)
    {
        this.value = value
    }
}

int main()
{
    Box instance = new Box(42)
    return instance.value
}