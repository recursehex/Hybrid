// Validate UTF-8/UTF-16 conversions for runtime string handling
// EXPECT_RUNTIME: héllo
// EXPECT_RUNTIME: 你好
// EXPECT_RUNTIME: 🌊
// EXPECT_RUNTIME: 🚀
// EXPECT_RUNTIME: (null)

extern string __hybrid_string_from_char32(int codepoint)
extern void print_string(string value)

ushort[] invalidUnits = [0xD800, 0]

int main()
{
    string accented = "héllo"
    string cjk = "你好"
    string wave = "🌊"
    string rocket = __hybrid_string_from_char32(0x1F680)

    print_string(accented)
    print_string(cjk)
    print_string(wave)
    print_string(rocket)

    unsafe
    {
        string invalid = string: #invalidUnits[0]
        print_string(invalid)
    }

    return 0
}