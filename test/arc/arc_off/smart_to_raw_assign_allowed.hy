// RUN_OPTS: --arc-enabled=false

class Node
{
    int value

    Node(int v)
    {
        this.value = v
    }
}

int main()
{
    shared<Node> owner = make_shared<Node>(9)
    Node copy = (1)
    copy = owner

    assert copy.value == 9
    assert owner.arcUseCount() == 0
    return 0
}
