// Test for complex switch statements with multiple statements in cases
int testValue = 1

switch testValue
{
    case 1
    {
        int x = 10
        int y = 20
        int sum = x + y
    }

    case 2, 3
    {
        bool flag = true
        if flag
        {
            int temp = 100
        }
    }

    default
    {
        string msg = "default case"
        char c = 'z'
    }
}

// Test nested switch expressions
int nested = switch 1
{
    1 => switch 2 
    {
        2 => 42
        default => 0
    }
    default => -1
}