// double free should be diagnosed at compile time

class Payload
{
    Payload() {}
    ~Payload() {}
}

int main()
{
    Payload value = Payload()
    free value
    free value
    return 0
}