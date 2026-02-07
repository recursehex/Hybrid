class User
{
    int id
    string name

    User(int id, string name)
    {
        this.id = id
        this.name = name
    }

    string this()
    {
        return $"User `this.id`:`this.name`"
    }
}

User user = (7, "Ada")
string label = $"Hello, `user`!"
assert label == "Hello, User 7:Ada!"