extern string __hybrid_string_slice(string source, int start, int length)
extern string __hybrid_string_append_mut(string original, string suffix)
extern int __hybrid_debug_strong_count(string value)

int main()
{
    string original = "abcde"
    string view = __hybrid_string_slice(original, 1, 3)

    assert view == "bcd"
    assert original.size == 5
    assert view.size == 3
    assert __hybrid_debug_strong_count(original) == 2

    string mutated = __hybrid_string_append_mut(view, "!")
    assert mutated == "bcd!"
    assert original == "abcde"
    return 0
}