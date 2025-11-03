class InventoryItem
{
    string sku
    public int quantityOnHand
    public int reorderPoint

    InventoryItem(string sku, int quantity, int reorderPoint)
    {
        this.sku = sku
        this.quantityOnHand = quantity
        this.reorderPoint = reorderPoint
    }

    bool NeedsReorder()
    {
        return this.quantityOnHand <= this.reorderPoint
    }

    void Receive(int amount)
    {
        this.quantityOnHand += amount
    }
}

InventoryItem bolts = ("B-100", 18, 10)
assert bolts.quantityOnHand == 18
bolts.quantityOnHand = 9  // public field writable externally
assert bolts.NeedsReorder() == true

bolts.reorderPoint = 12  // public field updated by caller
assert bolts.reorderPoint == 12

bolts.Receive(7)
assert bolts.quantityOnHand == 16
assert bolts.NeedsReorder() == false