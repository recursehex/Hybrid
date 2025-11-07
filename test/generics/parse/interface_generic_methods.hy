// Interface method prototypes should accept generic parameters
interface Repository<TItem>
{
    void Add(TItem item)
    void Clear()
}

void main() {}