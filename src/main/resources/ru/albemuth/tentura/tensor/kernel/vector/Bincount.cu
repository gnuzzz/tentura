#include "../../../hashmap/hashmap.h"

#define TILE_DIM 1024

extern "C"
__global__ void bincount(const int* vector, const int maxValue, int* result, const int length) {

  int index = threadIdx.x;

  //clean result vector
  int resultLength = maxValue + 1;
  int resultPartLength = (resultLength + TILE_DIM - 1) / TILE_DIM; //result length is maxValue + 1
  for (int i = 0; i < resultPartLength; i++) {
    int valueIndex = i * TILE_DIM + index;
    if (valueIndex < resultLength) {
      result[valueIndex] = 0;
    }
  }
  __syncthreads();

  //calculate thread bincount
  int partLength = (length + TILE_DIM - 1) / TILE_DIM;
  for (int i = 0; i < partLength; i++) {
    int valueIndex = i * TILE_DIM + index;
    if (valueIndex < length) {
      int value = vector[valueIndex];
      atomicAdd(result + value, 1);
    }
  }

}

