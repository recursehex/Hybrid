extern string __hybrid_string_append_mut(string original, string suffix)
extern int __hybrid_debug_strong_count(string value)

int main()
{
    string original = "cow"
    string copy = original

    string mutated = __hybrid_string_append_mut(original, "s")

    assert original == "cow"
    assert copy == "cow"
    assert mutated == "cows"
    assert __hybrid_debug_strong_count(copy) >= 1
    return 0
}