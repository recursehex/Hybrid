int freed = 0

class Payload
{
    int id

    Payload(int value)
    {
        this.id = value
    }

    ~Payload()
    {
        freed += this.id
    }
}

void freeRange(int start, int endExclusive)
{
    int i = start
    while i < endExclusive
    {
        Payload value = new Payload(i)
        free value
        i++
    }
}

int main()
{
    freeRange(1, 6)
    assert freed == 15

    freeRange(6, 11)
    assert freed == 55
    return 0
}
