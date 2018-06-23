#include "../../../hashmap/hashmap.h"

#define TILE_DIM 1024

extern "C"
__global__ void bincount(const int* matrix, const int maxValue, int* result, const int numRows, const int numColumns) {

  int index = threadIdx.x;
  int rowStride = blockDim.x;
  int resultNumColumns = maxValue + 1;
  int resultPartLength = (resultNumColumns + TILE_DIM - 1) / TILE_DIM; //result columns number is maxValue + 1
  int partLength = (numColumns + TILE_DIM - 1) / TILE_DIM;

  for (int row = blockIdx.x; row < numRows; row += rowStride) {

    //clean result matrix row
    for (int i = 0; i < resultPartLength; i++) {
      int column = i * TILE_DIM + index;
      if (column < resultNumColumns) {
        int valueIndex = row * resultNumColumns + column;
        result[valueIndex] = 0;
      }
    }
    __syncthreads();

    //calculate thread bincount
    for (int i = 0; i < partLength; i++) {
      int column = i * TILE_DIM + index;
      if (column < numColumns) {
        int valueIndex = row * numColumns + column;
        int value = matrix[valueIndex];
        atomicAdd(result + row * resultNumColumns + value, 1);
      }
    }
  }

}

