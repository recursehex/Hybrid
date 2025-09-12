// Test for switch statements with block-style cases
int num = 2

switch num
{
    case 1
    {
        // Code to execute when num equals 1
        int result = 10
    }

    case 2
    {
        // Code to execute when num equals 2  
        int result = 20
    }

    default
    {
        // Code to execute when num doesn't match any case
        int result = 0
    }
}

// Test for switch expressions with arrow syntax
char letter = 'a'

string message = switch letter
{
    'a' => "Alpha"          // If letter is 'a', returns "Alpha"
    'b' => "Beta"           // If letter is 'b', returns "Beta"
    default => "Unknown"    // For any other value, returns "Unknown"
}

// Test with integer cases and multiple values
int value = 3

int output = switch value
{
    1, 2 => 10          // Multiple values for same case
    3 => 30
    4, 5, 6 => 100
    default => -1
}