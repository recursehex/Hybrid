class PricingRules
{
    string planName
    const int baseFee
    const int perUnitFee

    PricingRules(string planName, int baseFee, int perUnitFee)
    {
        this.planName = planName
        this.baseFee = baseFee
        this.perUnitFee = perUnitFee
    }

    int Quote(int units)
    {
        return this.baseFee + this.perUnitFee * units
    }
}

PricingRules standard = ("Standard", 500, 75)
PricingRules volume = ("Volume", 300, 40)

assert standard.Quote(3) == 725
assert standard.baseFee == 500
assert standard.perUnitFee == 75

assert volume.baseFee == 300
assert volume.perUnitFee == 40
assert volume.Quote(5) == 500