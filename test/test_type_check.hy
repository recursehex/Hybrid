class Animal
{
    int id

    Animal()
    {
        this.id = 0
    }
}

class Dog inherits Animal
{
    Dog(int id)
    {
        this.id = id
    }
}

class Tree
{
    Tree()
    {
    }
}

Animal pet = new Dog(7)

bool sawAnimal = false
if pet is Animal
{
    sawAnimal = true
}
assert sawAnimal == true

bool notTree = false
if pet is not Tree
{
    notTree = true
}
assert notTree == true

Animal? maybePet = pet
int steps = 0
while maybePet is not null
{
    steps++
    maybePet = null
}
assert steps == 1

int ternaryHit = 1 if pet is Animal else 2
assert ternaryHit == 1

int ternaryMiss = 1 if pet is not Animal else 3
assert ternaryMiss == 3

Animal fallback = pet if pet is Animal else new()
bool fallbackOk = false
if fallback is Animal
{
    fallbackOk = true
}
assert fallbackOk == true
