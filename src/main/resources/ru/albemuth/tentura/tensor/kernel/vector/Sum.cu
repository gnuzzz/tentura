#define TILE_DIM 1024

template<typename T>
__device__ void sum(const T* vector, T* result, const int length) {
  __shared__ T threadSum[TILE_DIM];

  int index = threadIdx.x;

  int partLength = (length + TILE_DIM - 1) / TILE_DIM;

  T sum = 0;
  for (int i = 0; i < partLength; i++) {
    int valueIndex = i * TILE_DIM + index;
    if (valueIndex < length) {
      T value = vector[valueIndex];
      sum += value;
    }
  }
  threadSum[index] = sum;

  for (int d = 1; d < TILE_DIM && d < length; d <<= 1) {
    __syncthreads();
    if (index % (d << 1) == 0) {
      int valueIndex = index + d;
      if (valueIndex < TILE_DIM) {
        T value = threadSum[valueIndex];
        sum += value;
        threadSum[index] = sum;
      }
    }
  }

  if (index == 0) {
    result[0] = sum;
  }
}
