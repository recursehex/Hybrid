// Test for switch statements with block-style cases
int num = 2
int blockResult = -1

switch num
{
    case 1
    {
        // Code to execute when num equals 1
        blockResult = 10
    }

    case 2
    {
        // Code to execute when num equals 2  
        blockResult = 20
    }

    default
    {
        // Code to execute when num doesn't match any case
        blockResult = 0
    }
}
assert blockResult == 20

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
assert message == "Alpha"
assert output == 30