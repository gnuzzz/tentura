#define TILE_DIM 1024

//template<typename T, typename R>
//__device__ void common_math_std(const T* vector, R* result, const int length) {
//
//  __shared__ R threadSum[TILE_DIM];
//  __shared__ R threadSquareSum[TILE_DIM];
//
//  int index = threadIdx.x;
//
//  int partLength = (length + TILE_DIM - 1) / TILE_DIM;
//
//  R sum = 0;
//  R squareSum = 0;
//  for (int i = 0; i < partLength; i++) {
//    int valueIndex = i * TILE_DIM + index;
//    if (valueIndex < length) {
//      T value = vector[valueIndex];
//      sum += value;
//      squareSum += value * value;
//    }
//  }
//  threadSum[index] = sum;
//  threadSquareSum[index] = squareSum;
//
//  for (int d = 1; d < TILE_DIM && d < length; d <<= 1) {
//    __syncthreads();
//    if (index % (d << 1) == 0) {
//      int valueIndex = index + d;
//      if (valueIndex < TILE_DIM) {
//        R value = threadSum[valueIndex];
//        R square = threadSquareSum[valueIndex];
//        sum += value;
//        squareSum += square;
//        threadSum[index] = sum;
//        threadSquareSum[index] = squareSum;
//      }
//    }
//  }
//
//  if (index == 0) {
//    result[0] = sqrt((squareSum - (sum * sum) / length) / length);
//  }
//
//}

template<typename T>
__device__ void math_std(const T* vector, float* result, const int length) {

  __shared__ float threadSum[TILE_DIM];
  __shared__ float threadSquareSum[TILE_DIM];

  int index = threadIdx.x;

  int partLength = (length + TILE_DIM - 1) / TILE_DIM;

  float sum = 0;
  float squareSum = 0;
  for (int i = 0; i < partLength; i++) {
    int valueIndex = i * TILE_DIM + index;
    if (valueIndex < length) {
      float value = vector[valueIndex];
      sum += value;
      squareSum += value * value;
    }
  }
  threadSum[index] = sum;
  threadSquareSum[index] = squareSum;

  for (int d = 1; d < TILE_DIM && d < length; d <<= 1) {
    __syncthreads();
    if (index % (d << 1) == 0) {
      int valueIndex = index + d;
      if (valueIndex < TILE_DIM) {
        float value = threadSum[valueIndex];
        float square = threadSquareSum[valueIndex];
        sum += value;
        squareSum += square;
        threadSum[index] = sum;
        threadSquareSum[index] = squareSum;
      }
    }
  }

  if (index == 0) {
    result[0] = sqrt((squareSum - (sum * sum) / length) / length);
  }

}

