#include <cstdint>

extern "C" int hybrid_strlen(const uint16_t *str) {
  if (!str)
    return 0;

  const uint16_t *p = str;
  while (*p != 0) {
    ++p;
  }
  return static_cast<int>(p - str);
}
