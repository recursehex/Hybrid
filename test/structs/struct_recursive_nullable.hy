// Validate that structs can contain nullable self-references.

struct Node
{
    int value
    Node? next

    Node(int value, Node? next)
    {
        this.value = value
        this.next = next
    }
}

Node tail = Node(2, null)
Node head = Node(1, tail)

assert head.value == 1
assert head.next != null

assert tail.value == 2

Node? tailRef = head.next
assert tailRef != null
assert tailRef == tail

head.next = null
assert head.next == null

head.next = tail
Node? roundTrip = head.next
assert roundTrip != null
assert roundTrip == tail