// Mixing smart pointers and raw ARC-managed references should trigger diagnostics.

struct Node
{
    int value
}

int main()
{
    shared<Node> owner
    // Attempt to assign the shared handle to a raw strong reference.
    Node copy = owner
    return 0
}