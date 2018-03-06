#include "../../../hashmap/hashmap.h"

#define TILE_DIM 1024

extern "C"
__global__ void bincount(const int* vector, int* result, const int length) {

  HashMap<int, int, IntHash>* map = new HashMap<int, int, IntHash>(10);
  int index = threadIdx.x;
  int partLength = (length + TILE_DIM - 1) / TILE_DIM;

  //calculate thread bincount
  for (int i = 0; i < partLength; i++) {
    int valueIndex = i * TILE_DIM + index;
    if (valueIndex < length) {
      int value = vector[valueIndex];
      Entry<int, int>* entry = map->entry(value);
      if (entry == NULL) {
        map->put(value, 1);
      } else {
        entry->setValue(entry->value() + 1);
      }
    }
  }

  //fill result vector
  for (HashMap<int, int, IntHash>::EntriesIterator* it = map->entriesIterator(); it->hasNext(); ) {
    Entry<int, int>* entry = it->next();
    atomicAdd(result + entry->key(), entry->value());
  }

}

