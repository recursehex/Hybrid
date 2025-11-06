// Expect generic arity mismatch diagnostic
class Box<T>
{
    Box()
    {
    }
}

class Uses
{
    Box<int, string> invalidField
}

void main()
{
}