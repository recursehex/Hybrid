// EXPECT_DIAGNOSTIC: Cannot access nullable type 'Person?' without null-safe operator
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
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
