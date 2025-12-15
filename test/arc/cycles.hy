// EXPECT_OUTPUT: Potential retain cycle detected: Node --(next)--> Node -- back to Node
// EXPECT_OUTPUT: Wrap back-references like 'next' in 'weak<T>' smart pointers to avoid retain cycles. Destructors will not run while the cycle remains.

class Node
{
    public shared<Node> next

    Node()
    {
        this.next = shared<Node>()
    }
}

int main()
{
    Node head = ()
    return 0
}
