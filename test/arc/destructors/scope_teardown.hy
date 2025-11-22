int drops = 0

struct Box
{
    int weight

    Box(int w)
    {
        this.weight = w
    }

    ~Box()
    {
        drops += this.weight
    }
}

int run(bool fast)
{
    Box local = Box(3)
    if (fast)
    {
        return drops
    }

    drops++
    return drops
}

int main()
{
    run(true)
    assert drops == 3

    run(false)
    assert drops == 7

    return 0
}