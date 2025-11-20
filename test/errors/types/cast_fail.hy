// Test cases that should fail for type casting

// Test casting to void (should fail)
int x = 10
void v = void: x            // Error: can't cast to void

// Test casting from void (should fail)
void func()
{
    return
}
int y = int: func()         // Error: can't cast void to int

// Test casting bool (currently not supported in explicit casts)
bool flag = true
int boolToInt = int: flag   // Error: bool casting not supported