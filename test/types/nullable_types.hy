struct Address
{
    string city

    Address(string city)
    {
        this.city = city
    }
}

struct User
{
    string name
    Address? address
    string? alias

    User(string name)
    {
        this.name = name
        this.address = null
        this.alias = null
    }
}

string? getAlias(User? maybeUser)
{
    return maybeUser?.alias
}

void setAlias(User user, string? newAlias)
{
    user.alias = newAlias
}

int runNullableDemo()
{
    User user = User("Alice")
    setAlias(user, "ace")

    string? alias = user.alias
    alias = null

    string? city = user.address?.city

    user.address = Address("New York")
    string? confirmedCity = user.address?.city

    User? nobody = null
    string? missingAlias = getAlias(nobody)

    string actualName = user.name
    return 0
}
