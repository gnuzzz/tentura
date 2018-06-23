#define TILE_DIM 1024

template<typename T>
__device__ void sum(const T* matrix, T* result,
                    const int rows, const int cols) {

  __shared__ T tile[TILE_DIM];
  
  int index = threadIdx.x;

  int length = rows * cols;
  int partLength = (length + TILE_DIM - 1) / TILE_DIM;

  T sum = 0;
  for (int i = 0; i < partLength; i++) {
    int valueIndex = i * TILE_DIM + index;
    if (valueIndex < length) {
      T value = matrix[valueIndex];
      sum += value;
    }
  }
  tile[index] = sum;

  for (int d = 1; d < TILE_DIM && d < length; d <<= 1) {
    __syncthreads();
    if (index % (d << 1) == 0) {
      int valueIndex = index + d;
      if (valueIndex < TILE_DIM) {
        T value = tile[valueIndex];
        sum += value;
        tile[index] = sum;
      }
    }
  }

  if (index == 0) {
    result[0] = sum;
  }
}

