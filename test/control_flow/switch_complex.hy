// Test for complex switch statements with multiple statements in cases
int testValue = 1
int caseSum = 0
bool multiCaseEntered = false
string defaultMsgResult = ""
char defaultCharResult = '\0'

switch testValue
{
    case 1
    {
        int x = 10
        int y = 20
        int sum = x + y
        caseSum = sum
    }

    case 2, 3
    {
        bool flag = true
        if flag
        {
            int temp = 100
            multiCaseEntered = true
        }
    }

    default
    {
        string msg = "default case"
        char c = 'z'
        defaultMsgResult = msg
        defaultCharResult = c
    }
}
assert caseSum == 30
assert multiCaseEntered == false
assert defaultMsgResult == ""
assert defaultCharResult == '\0'

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
assert nested == 42