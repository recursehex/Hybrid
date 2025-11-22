// manual destructor followed by free should be diagnosed as a double release

class Payload
{
    Payload() {}
    ~Payload() {}
}

int main()
{
    Payload value = Payload()
    value.~Payload()
    free value
    return 0
}