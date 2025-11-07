// Nested generic type arguments should round-trip through the parser
class List<T>
{
    List() {}
}

interface MatrixBuilder<TElement>
{
    void Fill(List<List<TElement>> values)
}

void main() {}