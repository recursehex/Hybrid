int drops = 0

class Manual
{
    Manual() {}

    ~Manual()
    {
        drops++
    }
}

int main()
{
    Manual temp = ()
    temp.~Manual()
    assert drops == 1
    return 0
}