#define TILE_DIM 1024

#include <limits>

template<typename T>
__device__ void argmaxColumn(const T* matrix, int* result, const int numRows, const int numColumns) {

  __shared__ T partsVals[TILE_DIM];
  __shared__ int partsArgs[TILE_DIM];

  int index = threadIdx.x;
  int rowStride = blockDim.x;
  int partLength = (numColumns + TILE_DIM - 1) / TILE_DIM;
  int limit = numColumns < TILE_DIM ? numColumns : TILE_DIM;

  for (int row = blockIdx.x; row < numRows; row += rowStride) {

    T max = std::numeric_limits<T>::min();;
    int argmax = -1;
    for (int i = 0; i < partLength; i++) {
      int columnIndex = i * TILE_DIM + index;
      if (columnIndex < numColumns) {
        T value = matrix[row * numColumns + columnIndex];
        if (value > max) {
          max = value;
          argmax = columnIndex;
        }
      }
    }
    partsVals[index] = max;
    partsArgs[index] = argmax;

    for (int d = 1; d < limit; d <<= 1) {
      __syncthreads();
      if (index % (d << 1) == 0) {
        int valueIndex = index + d;
        if (valueIndex < limit) {
          T value = partsVals[valueIndex];
          int arg = partsArgs[valueIndex];
          if (value > max) {
            max = value;
            partsVals[index] = max;
            argmax = arg;
            partsArgs[index] = argmax;
          }
        }
      }
    }

    if (index == 0) {
      result[row] = argmax;
    }

  }
}