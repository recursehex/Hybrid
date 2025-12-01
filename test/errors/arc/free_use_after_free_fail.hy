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
    Payload p = new Payload(5)
    free p
    int i = p.value
}