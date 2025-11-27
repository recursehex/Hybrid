// use-after-free should be diagnosed at compile time

class Payload
{
    int value

    Payload(int v)
    {
        this.value = v
    }

    ~Payload() {}
}

void main()
{
    Payload p = Payload(5)
    free p
    int i = p.value
}