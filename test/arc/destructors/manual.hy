int drops = 0

class Manual
{
    Manual() {}

    ~Manual()
    {
        drops = drops + 1
    }
}

int main()
{
    Manual temp = Manual()
    temp.~Manual()
    assert drops == 1
    return 0
}