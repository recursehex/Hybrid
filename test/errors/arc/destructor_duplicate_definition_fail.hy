// duplicate destructor definitions should be rejected

class DuplicateDestructor
{
    DuplicateDestructor() {}
    ~DuplicateDestructor() {}
    ~DuplicateDestructor() {}
}

int main()
{
    DuplicateDestructor value = DuplicateDestructor()
    return 0
}