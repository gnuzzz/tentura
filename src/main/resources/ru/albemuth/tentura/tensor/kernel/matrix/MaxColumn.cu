#define TILE_DIM 1024

#include <limits>

template<typename T>
__device__ void maxColumn(const T* matrix, T* result, const int numRows, const int numColumns) {

  __shared__ T threadMax[TILE_DIM];

  int index = threadIdx.x;
  int rowStride = blockDim.x;
  int partLength = (numColumns + TILE_DIM - 1) / TILE_DIM;
  int limit = numColumns < TILE_DIM ? numColumns : TILE_DIM;

  for (int row = blockIdx.x; row < numRows; row += rowStride) {

    T max = std::numeric_limits<T>::min();
    for (int i = 0; i < partLength; i++) {
      int columnIndex = i * TILE_DIM + index;
      if (columnIndex < numColumns) {
        T value = matrix[row * numColumns + columnIndex];
        if (value > max) {
          max = value;
        }
      }
    }
    threadMax[index] = max;

    for (int d = 1; d < limit; d <<= 1) {
      __syncthreads();
      if (index % (d << 1) == 0) {
        int valueIndex = index + d;
        if (valueIndex < limit) {
          T value = threadMax[valueIndex];
          if (value > max) {
            max = value;
            threadMax[index] = max;
          }
        }
      }
    }

    if (index == 0) {
      result[row] = max;
    }
  }
}