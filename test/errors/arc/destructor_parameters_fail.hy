// destructors must not declare parameters

class DestructorWithParams
{
    DestructorWithParams() {}
    ~DestructorWithParams(int code) {}
}

int main()
{
    DestructorWithParams value = DestructorWithParams()
    return 0
}