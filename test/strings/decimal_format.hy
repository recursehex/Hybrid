decimal price = 12.3456
decimal whole = 7

string price2 = $"price=`price:2`"
string price3 = $"price=`price:3`"
string whole0 = $"whole=`whole:0`"

assert price2 == "price=12.35"
assert price3 == "price=12.346"
assert whole0 == "whole=7"
