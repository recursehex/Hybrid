// Non-nullable self references should be rejected with a clear diagnostic.

struct Loop
{
    Loop next
}