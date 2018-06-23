#define TILE_DIM 1024

#include <limits>

template<typename T>
__device__ void max(const T* matrix, T* result, const int numRows, const int numColumns) {

  __shared__ T threadMax[TILE_DIM];

  int index = threadIdx.x;

  int length = numRows * numColumns;
  int partLength = (length + TILE_DIM - 1) / TILE_DIM;

  T max = std::numeric_limits<T>::min();
  for (int i = 0; i < partLength; i++) {
    int valueIndex = i * TILE_DIM + index;
    if (valueIndex < length) {
      T value = matrix[valueIndex];
      if (value > max) {
        max = value;
      }
    }
  }
  threadMax[index] = max;

  int limit = length < TILE_DIM ? length : TILE_DIM;
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
    result[0] = max;
  }
}
