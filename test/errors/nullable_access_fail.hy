struct Person
{
    string? nickname

    Person()
    {
        this.nickname = null
    }
}

int failNullableAccess()
{
    Person? maybePerson = null
    // This should fail because maybePerson is nullable and requires ?. for access
    string? alias = maybePerson.nickname
    return 0
}
