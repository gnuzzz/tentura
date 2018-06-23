#define TILE_DIM 1024

#include <limits>

template<typename T>
__device__ void min(const T* matrix, T* result, const int numRows, const int numColumns) {

  __shared__ T threadMin[TILE_DIM];

  int index = threadIdx.x;

  int length = numRows * numColumns;
  int partLength = (length + TILE_DIM - 1) / TILE_DIM;

  T min = std::numeric_limits<T>::max();
  for (int i = 0; i < partLength; i++) {
    int valueIndex = i * TILE_DIM + index;
    if (valueIndex < length) {
      T value = matrix[valueIndex];
      if (value < min) {
        min = value;
      }
    }
  }
  threadMin[index] = min;

  int limit = length < TILE_DIM ? length : TILE_DIM;
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
    result[0] = min;
  }
}
