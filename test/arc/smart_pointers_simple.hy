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
    unique<Node> u1 = (10)

    // Test shared pointer with class
    shared<Node> s1 = (20)

    // Test weak pointer from shared
    weak<Node> w1 = (s1)

    // Test with primitives
    unique<int> u2 = (42)
    shared<int> s2 = (99)
    weak<int> w2 = (s2)

    return 0
}