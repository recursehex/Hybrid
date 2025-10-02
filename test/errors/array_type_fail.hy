// Test array type compatibility errors - these should fail compilation

// Test 1: Mixing incompatible types (bool and int)
// Error: Cannot mix bool and int in array literal
int[] bad_bool_int = [1, 2, true, 4]

// Test 2: Mixing incompatible types (string and char)
// Error: Cannot mix string and char in array literal
string[] bad_string_char = ["hello", 'x', "world"]

// Test 3: Mixing incompatible types (bool and string)
// Error: Cannot mix bool and string in array literal
bool[] bad_bool_string = [true, "false", true]

// Test 4: Mixing incompatible types (int and string)
// Error: Cannot mix int and string in array literal
int[] bad_int_string = [1, 2, "three", 4]

// Test 5: Mixing incompatible types (char and int)
// Error: Cannot mix char and int in array literal
char[] bad_char_int = ['a', 65, 'c']

int main() {
    return 0
}