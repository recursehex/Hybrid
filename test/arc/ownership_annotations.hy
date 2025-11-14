struct Node
{
    weak Node? parent
    unowned Node? owner

    Node()
    {
        this.parent = null
        this.owner = null
    }
}

int main()
{
    weak Node? parent = null
    unowned Node? owner = null

    Node node = Node()
    node.parent = parent
    node.owner = owner

    return 0
}