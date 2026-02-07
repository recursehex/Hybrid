int main()
{
    string text = "hi"
    string suffix = text + '!'
    string prefix = '>' + text

    assert suffix == "hi!"
    assert prefix == ">hi"
    assert suffix.size == 3
    assert prefix.size == 3
    return 0
}