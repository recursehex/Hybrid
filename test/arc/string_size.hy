extern int hybrid_strlen(string s)

int main()
{
    string greeting = "hello"
    string alias = greeting
    string exclaim = greeting + "!"

    assert greeting.size == 5
    assert alias.size == 5
    assert exclaim.size == 6
    assert hybrid_strlen(exclaim) == 6
    return 0
}