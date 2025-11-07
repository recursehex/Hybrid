// Expect: invariant generic assignment should be rejected

class Animal
{
    Animal() {}
}

class Dog inherits Animal
{
    Dog() {}
}

class Crate<T>
{
    T occupant

    Crate(T occupant)
    {
        this.occupant = occupant
    }
}

void main()
{
    Crate<Dog> dogCrate = Crate<Dog>(Dog())
    Crate<Animal> animalCrate = Crate<Animal>(Animal())

    // Should fail: Crate<Dog> is not assignable to Crate<Animal>
    animalCrate = dogCrate
}