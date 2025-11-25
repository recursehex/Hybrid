int drops = 0

class Payload
{
    int weight

    Payload(int w)
    {
        this.weight = w
    }

    ~Payload()
    {
        drops = drops + this.weight
    }
}

void runOnce(int weight)
{
    Payload temp = new Payload(weight)
    free temp
}

int main()
{
    runOnce(3)
    runOnce(7)
    assert drops == 10
    return 0
}