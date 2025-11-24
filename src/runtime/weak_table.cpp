// Weak reference registry used to zero observer slots when a payload is released.

#include "hybrid_runtime.h"
#include "memory/ref_count.h"

#include <algorithm>
#include <map>
#include <mutex>
#include <vector>

using hybrid::memory::RefCount;

namespace {

std::mutex WeakTableMutex;
std::map<void *, std::vector<void **>> WeakTable;

} // namespace

extern "C" {

void hybrid_register_weak_slot(void *object, void **slot) {
  if (!object || !slot)
    return;
  RefCount counts = RefCount::fromObject(object);
  counts.retainWeak();
  std::lock_guard<std::mutex> lock(WeakTableMutex);
  WeakTable[object].push_back(slot);
}

void hybrid_unregister_weak_slot(void *object, void **slot) {
  if (!object || !slot)
    return;
  std::lock_guard<std::mutex> lock(WeakTableMutex);
  auto it = WeakTable.find(object);
  if (it == WeakTable.end())
    return;
  auto &slots = it->second;
  auto newEnd = std::remove(slots.begin(), slots.end(), slot);
  if (newEnd != slots.end()) {
    slots.erase(newEnd, slots.end());
    RefCount counts = RefCount::fromObject(object);
    counts.releaseWeak();
  }
  if (slots.empty())
    WeakTable.erase(it);
}

void hybrid_zero_weak_refs(void *object) {
  if (!object)
    return;
  std::vector<void **> slots;
  {
    std::lock_guard<std::mutex> lock(WeakTableMutex);
    auto it = WeakTable.find(object);
    if (it == WeakTable.end())
      return;
    slots.swap(it->second);
    WeakTable.erase(it);
  }

  if (slots.empty())
    return;

  RefCount counts = RefCount::fromObject(object);
  for (void **slot : slots) {
    if (slot)
      *slot = nullptr;
    counts.releaseWeak();
  }
}

} // extern "C"
