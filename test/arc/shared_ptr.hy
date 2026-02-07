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
    shared<Resource> survivor = (22)
    {
        shared<Resource> owner = (11)
        shared<Resource> alias = owner
        assert owner.arcUseCount() == 2

        alias = survivor
        assert owner.arcUseCount() == 1
        assert survivor.arcUseCount() == 2
    }

    assert drops == 11
    return 0
}
