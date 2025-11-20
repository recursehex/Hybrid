// destructors cannot declare generic parameters

class GenericDestructor<T>
{
    GenericDestructor() {}
    ~GenericDestructor<T>() {}
}

int main()
{
    GenericDestructor<int> value = GenericDestructor<int>()
    return 0
}