#define TILE_DIM 1024

#include <limits>

template<typename T>
__device__ void argminColumn(const T* matrix, int* result, const int numRows, const int numColumns) {

  __shared__ T partsVals[TILE_DIM];
  __shared__ int partsArgs[TILE_DIM];

  int index = threadIdx.x;
  int rowStride = blockDim.x;
  int partLength = (numColumns + TILE_DIM - 1) / TILE_DIM;
  int limit = numColumns < TILE_DIM ? numColumns : TILE_DIM;

  for (int row = blockIdx.x; row < numRows; row += rowStride) {

    T min = std::numeric_limits<T>::max();
    int argmin = -1;
    for (int i = 0; i < partLength; i++) {
      int columnIndex = i * TILE_DIM + index;
      if (columnIndex < numColumns) {
        T value = matrix[row * numColumns + columnIndex];
        if (value < min) {
          min = value;
          argmin = columnIndex;
        }
      }
    }
    partsVals[index] = min;
    partsArgs[index] = argmin;

    for (int d = 1; d < limit; d <<= 1) {
      __syncthreads();
      if (index % (d << 1) == 0) {
        int valueIndex = index + d;
        if (valueIndex < limit) {
          T value = partsVals[valueIndex];
          int arg = partsArgs[valueIndex];
          if (value < min) {
            min = value;
            partsVals[index] = min;
            argmin = arg;
            partsArgs[index] = argmin;
          }
        }
      }
    }

    if (index == 0) {
      result[row] = argmin;
    }

  }
}