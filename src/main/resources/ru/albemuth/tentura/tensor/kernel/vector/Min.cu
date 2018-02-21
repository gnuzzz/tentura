#define TILE_DIM 1024

template<typename T>
__device__ void min(const T* vector, T* result, const int length) {

  __shared__ T parts[TILE_DIM];

  int index = threadIdx.x;

  int partLength = (length + TILE_DIM - 1) / TILE_DIM;

  T min;
  if (index < length) {
    min = vector[index];
  }
  for (int i = 1; i < partLength; i++) {
    int valueIndex = i * TILE_DIM + index;
    if (valueIndex < length) {
      T value = vector[valueIndex];
      if (value < min) {
        min = value;
      }
    }
  }
  parts[index] = min;

  int limit = length < TILE_DIM ? length : TILE_DIM;
  for (int d = 1; d < limit; d <<= 1) {
    __syncthreads();
    if (index % (d << 1) == 0) {
      int valueIndex = index + d;
      if (valueIndex < limit) {
        T value = parts[valueIndex];
        if (value < min) {
          min = value;
          parts[index] = min;
        }
      }
    }
  }

  if (index == 0) {
    result[0] = min;
  }
}
