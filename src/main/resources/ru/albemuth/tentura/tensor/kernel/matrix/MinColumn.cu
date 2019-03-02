#define TILE_DIM 1024

#include <limits>

template<typename T>
__device__ void minColumn(const T* matrix, T* result, const int numRows, const int numColumns) {

  __shared__ T threadMin[TILE_DIM];

  int index = threadIdx.x;
  int rowStride = blockDim.x;
  int partLength = (numColumns + TILE_DIM - 1) / TILE_DIM;
  int limit = numColumns < TILE_DIM ? numColumns : TILE_DIM;

  for (int row = blockIdx.x; row < numRows; row += rowStride) {

    T min = std::numeric_limits<T>::max();
    for (int i = 0; i < partLength; i++) {
      int columnIndex = i * TILE_DIM + index;
      if (columnIndex < numColumns) {
        T value = matrix[row * numColumns + columnIndex];
        if (value < min) {
          min = value;
        }
      }
    }
    threadMin[index] = min;

    for (int d = 1; d < limit; d <<= 1) {
      __syncthreads();
      if (index % (d << 1) == 0) {
        int valueIndex = index + d;
        if (valueIndex < limit) {
          T value = threadMin[valueIndex];
          if (value < min) {
            min = value;
            threadMin[index] = min;
          }
        }
      }
    }

    if (index == 0) {
      result[row] = min;
    }
  }
}