// Test cases that should fail for type casting

// Test casting to void (should fail)
int x = 10
void v = void: x            // Error: Cannot cast to void

// Test casting from void (should fail)
void func()
{
    return
}
int y = int: func()         // Error: Cannot cast void to int

// Test casting bool (currently not supported in explicit casts)
bool flag = true
int boolToInt = int: flag   // Error: Cannot cast bool to int
