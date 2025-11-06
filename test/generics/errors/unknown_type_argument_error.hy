// Expect unknown type argument diagnostic
class List<T>
{
    List()
    {
    }
}

class Uses<T>
{
    List<U> invalidField
}

void main()
{
}