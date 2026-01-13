// EXPECT_OUTPUT: Cannot call nullable delegate 'Logger' without a null check
delegate void Logger(string message)

void log(string message)
{
    print(message)
}

int main()
{
    Logger? sink = null
    sink("ready")
    return 0
}
