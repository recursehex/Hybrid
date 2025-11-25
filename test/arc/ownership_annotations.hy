// Smart-pointer based ownership annotations without bare ownership keywords

struct Node
{
    weak<int> watcher

    Node()
    {
        this.watcher = weak<int>()
    }
}

int main()
{
    shared<int> payload = shared<int>(7)

    Node node = Node()
    node.watcher = weak<int>(payload)

    weak<int> copy = node.watcher

    return 0
}