// unique<T> values should not be reused after a move assignment.

struct Gadget
{
    int value
}

int main()
{
    unique<Gadget> head
    unique<Gadget> tail
    tail = head

    // This should fail: 'head' was moved into 'tail' already.
    unique<Gadget> spare = head
    return 0
}