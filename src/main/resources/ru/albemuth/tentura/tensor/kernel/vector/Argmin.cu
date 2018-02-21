#define TILE_DIM 1024

template<typename T>
__device__ void argmin(const T* vector, int* result, const int length) {

  __shared__ T partsVals[TILE_DIM];
  __shared__ int partsArgs[TILE_DIM];

  int index = threadIdx.x;

  int partLength = (length + TILE_DIM - 1) / TILE_DIM;

  T min;
  int argmin;
  if (index < length) {
    min = vector[index];
    argmin = index;
  }
  for (int i = 1; i < partLength; i++) {
    int valueIndex = i * TILE_DIM + index;
    if (valueIndex < length) {
      T value = vector[valueIndex];
      if (value < min) {
        min = value;
        argmin = valueIndex;
      }
    }
  }
  partsVals[index] = min;
  partsArgs[index] = argmin;

  int limit = length < TILE_DIM ? length : TILE_DIM;
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
    result[0] = argmin;
  }
}