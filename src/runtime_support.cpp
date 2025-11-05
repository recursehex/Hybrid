#include <cstdint>

extern "C" {

struct HybridTypeDescriptor;

struct HybridInterfaceEntry {
  const HybridTypeDescriptor *interfaceType;
  const void **methodTable;
};

struct HybridTypeDescriptor {
  const char *typeName;
  const HybridTypeDescriptor *baseType;
  const void **vtable;
  std::uint32_t vtableSize;
  const HybridInterfaceEntry *interfaces;
  std::uint32_t interfaceCount;
};

int hybrid_strlen(const uint16_t *str) {
  if (!str)
    return 0;

  const uint16_t *p = str;
  while (*p != 0) {
    ++p;
  }
  return static_cast<int>(p - str);
}

const void **hybrid_lookup_interface_table(const HybridTypeDescriptor *typeDesc,
                                           const HybridTypeDescriptor *interfaceDesc) {
  const HybridTypeDescriptor *current = typeDesc;
  while (current) {
    for (std::uint32_t idx = 0; idx < current->interfaceCount; ++idx) {
      const HybridInterfaceEntry &entry = current->interfaces[idx];
      if (entry.interfaceType == interfaceDesc)
        return entry.methodTable;
    }
    current = current->baseType;
  }
  return nullptr;
}

} // extern "C"
