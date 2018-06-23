#define TILE_DIM 1024

template<typename T>
__device__ void math_stdd(const T* vector, double* result, const int length) {

  __shared__ double threadSum[TILE_DIM];
  __shared__ double threadSquareSum[TILE_DIM];

  int index = threadIdx.x;

  int partLength = (length + TILE_DIM - 1) / TILE_DIM;

  double sum = 0;
  double squareSum = 0;
  for (int i = 0; i < partLength; i++) {
    int valueIndex = i * TILE_DIM + index;
    if (valueIndex < length) {
      double value = vector[valueIndex];
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
        double value = threadSum[valueIndex];
        double square = threadSquareSum[valueIndex];
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