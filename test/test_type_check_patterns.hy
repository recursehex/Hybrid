class Animal
{
    int id

    Animal(int id)
    {
        this.id = id
    }

    int Id()
    {
        return this.id
    }
}

class Dog inherits Animal
{
    Dog(int id)
    {
        base(id)
    }
}

Animal pet = new Dog(42)

int tag = 0
if pet is Dog dog
{
    tag = dog.Id()
}
assert tag == 42

Animal? maybePet = pet
int total = 0
while maybePet is Animal current
{
    total += current.Id()
    maybePet = null
}
assert total == 42
