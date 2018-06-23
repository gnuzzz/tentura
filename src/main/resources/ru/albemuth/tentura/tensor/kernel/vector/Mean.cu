#define TILE_DIM 1024

template<typename T, typename R>
__device__ void common_mean(const T* vector, R* result, const int length) {
//  __shared__ R threadMean[TILE_DIM];
//  __shared__ int threadNumber[TILE_DIM];
//
//  int index = threadIdx.x;
//
//  int partLength = (length + TILE_DIM - 1) / TILE_DIM;
//
//  R mean = 0;
//  int number = 0;
//  for (int i = 0; i < partLength; i++) {
//    int valueIndex = i * TILE_DIM + index;
//    if (valueIndex < length) {
//      T value = vector[valueIndex];
//      mean = (value + number * mean) / (++number);
//    }
//  }
//  threadMean[index] = mean;
//  threadNumber[index] = number;
//
//  for (int d = 1; d < TILE_DIM && d < length; d <<= 1) {
//    __syncthreads();
//    if (index % (d << 1) == 0) {
//      int meanIndex = index + d;
//      if (meanIndex < TILE_DIM && meanIndex < length) {
//        R m = threadMean[meanIndex];
//        int n = threadNumber[meanIndex];
//        mean = mean * (number / (R)(number + n)) + m * (n / (R)(number + n));
//        number += n;
//        threadMean[index] = mean;
//        threadNumber[index] = number;
//      }
//    }
//  }
//
//  if (index == 0) {
//    result[0] = mean;
//  }

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
    result[0] = sum / (R)length;
  }
}

template<typename T>
__device__ void mean(const T* vector, float* result, const int length) {
  common_mean<T, float>(vector, result, length);
}

template<typename T>
__device__ void meand(const T* vector, double* result, const int length) {
  common_mean<T, double>(vector, result, length);
}
