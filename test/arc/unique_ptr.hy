int drops = 0

class Resource
{
    int id

    Resource(int id)
    {
        this.id = id
    }

    ~Resource()
    {
        drops += this.id
    }
}

int main()
{
    unique<Resource> primary = (1)
    Resource peek = primary.get()
    assert peek.id == 1

    unique<Resource> spare = (2)
    spare = primary

    unique<Resource> temp = (3)
    primary = temp

    return 0
}
