class AccessLog
{
    private int totalAttempts
    private int deniedAttempts
    string resource

    AccessLog(string resource)
    {
        this.resource = resource
        this.totalAttempts = 0
        this.deniedAttempts = 0
    }

    private void Record(bool allowed)
    {
        this.totalAttempts++
        if (allowed == false)
        {
            this.deniedAttempts++
        }
    }

    void Allow()
    {
        this.Record(true)
    }

    void Deny()
    {
        this.Record(false)
    }

    int TotalAttempts()
    {
        return this.totalAttempts
    }

    int SuccessfulAttempts()
    {
        return this.totalAttempts - this.deniedAttempts
    }

    int DeniedAttempts()
    {
        return this.deniedAttempts
    }
}

AccessLog log = ("payments")
log.Allow()
log.Allow()
log.Deny()
log.Deny()

assert log.TotalAttempts() == 4
assert log.SuccessfulAttempts() == 2
assert log.DeniedAttempts() == 2