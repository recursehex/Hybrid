extern int hybrid_strlen(string s)

void check_basic_lengths()
{
    string ascii = "cafe"
    string accented = "café"
    string emoji = "😀"
    string mixed = "A😀B"
    string rocket = "🚀🚀"

    assert hybrid_strlen(ascii) == 4
    assert hybrid_strlen(accented) == 4
    assert hybrid_strlen(emoji) == 2
    assert hybrid_strlen(mixed) == 4
    assert hybrid_strlen(rocket) == 4
}

int main()
{
    check_basic_lengths()
    return 0
}