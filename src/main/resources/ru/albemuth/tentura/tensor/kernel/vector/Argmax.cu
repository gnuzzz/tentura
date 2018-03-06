#define TILE_DIM 1024

template<typename T>
__device__ void argmax(const T* vector, int* result, const int length) {

  __shared__ T partsVals[TILE_DIM];
  __shared__ int partsArgs[TILE_DIM];

  int index = threadIdx.x;

  int partLength = (length + TILE_DIM - 1) / TILE_DIM;

  T max;
  int argmax;
  if (index < length) {
    max = vector[index];
    argmax = index;
  }
  for (int i = 1; i < partLength; i++) {
    int valueIndex = i * TILE_DIM + index;
    if (valueIndex < length) {
      T value = vector[valueIndex];
      if (value > max) {
        max = value;
        argmax = valueIndex;
      }
    }
  }
  partsVals[index] = max;
  partsArgs[index] = argmax;

  int limit = length < TILE_DIM ? length : TILE_DIM;
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
    result[0] = argmax;
  }
}