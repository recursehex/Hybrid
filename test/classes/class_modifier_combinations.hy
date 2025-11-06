class FeatureFlag
{
    public static int toggleCount = 0
    static const int maxFlags = 3

    FeatureFlag(bool startEnabled)
    {
        if startEnabled
        {
            toggleCount++
        }
    }

    void Toggle()
    {
        if toggleCount < maxFlags
        {
            toggleCount++
        }
    }
}

assert FeatureFlag.maxFlags == 3

FeatureFlag alpha = (true)
assert FeatureFlag.toggleCount == 1

FeatureFlag beta = (false)
beta.Toggle()
beta.Toggle()
assert FeatureFlag.toggleCount == 3