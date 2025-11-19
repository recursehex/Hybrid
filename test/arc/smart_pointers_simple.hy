// Minimal smart pointer test without potential overload conflicts

class Node
{
    int data

    Node(int d)
    {
        this.data = d
    }
}

int main()
{
    // Test unique pointer with class
    unique<Node> u1 = unique<Node>(Node(10))

    // Test shared pointer with class
    shared<Node> s1 = shared<Node>(Node(20))

    // Test weak pointer from shared
    weak<Node> w1 = weak<Node>(s1)

    // Test with primitives
    unique<int> u2 = unique<int>(42)
    shared<int> s2 = shared<int>(99)
    weak<int> w2 = weak<int>(s2)

    return 0
}