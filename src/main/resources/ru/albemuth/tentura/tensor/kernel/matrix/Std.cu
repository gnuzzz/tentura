#define TILE_DIM 32

template<typename T, typename R>
__device__ void common_std(const T* matrix, R* result,
                           const int numRows, const int numColumns) {

  __shared__ R sumTile[TILE_DIM][TILE_DIM];
  __shared__ R squareSumTile[TILE_DIM][TILE_DIM];

  int tx = threadIdx.x;
  int ty = threadIdx.y;
  sumTile[ty][tx] = 0;
  squareSumTile[ty][tx] = 0;

  #pragma unroll
  for (int tr = 0; tr < (numRows - 1) / TILE_DIM + 1; tr++) {
    for (int tc = 0; tc < (numColumns - 1) / TILE_DIM + 1; tc++) {
      int r = tr * TILE_DIM + ty;
      int c = tc * TILE_DIM + tx;
      if (r < numRows && c < numColumns) {
        T value = matrix[r * numColumns + c];
        sumTile[ty][tx] += value;
        squareSumTile[ty][tx] += value * value;
      }
      __syncthreads();
    }
  }

  if (tx == 0 && ty == 0) {
    R sum = 0;
    R squareSum = 0;
    #pragma unroll
    for (int i = 0; i < TILE_DIM; i++) {
      #pragma unroll
      for (int j = 0; j < TILE_DIM; j++) {
        sum += sumTile[i][j];
        squareSum += squareSumTile[i][j];
      }
    }
    int length = numRows * numColumns;
    result[0] = sqrt((squareSum - (sum * sum) / length) / length);
  }
}

template<typename T>
__device__ void math_std(const T* matrix, float* result,
                         const int numRows, const int numColumns) {
  common_std<T, float>(matrix, result, numRows, numColumns);
}

template<typename T>
__device__ void math_stdd(const T* matrix, double* result,
                     const int numRows, const int numColumns) {
  common_std<T, double>(matrix, result, numRows, numColumns);
}

template<typename T, typename R>
__device__ void common_rowsStd(const T* matrix, R* result,
                               const int numRows, const int numColumns) {

  __shared__ T tile[TILE_DIM][TILE_DIM];

  int by = blockIdx.y;
  int ty = threadIdx.y;
  int row = by * blockDim.y + ty;
  R sum = 0;
  R squareSum = 0;

  #pragma unroll
  for (int t = 0; t < (numColumns - 1) / TILE_DIM + 1; t++) {
    #pragma unroll
    for (int i = 0; i < TILE_DIM; i++) {
      int r = by * TILE_DIM + i;
      int c = t * TILE_DIM + ty;
      if (r < numRows && c < numColumns) {
        tile[i][ty] = matrix[r * numColumns + c];
      } else {
        tile[i][ty] = 0;
      }
    }
    __syncthreads();

    #pragma unroll
    for (int j = 0; j < TILE_DIM; j++) {
      T value = tile[ty][j];
      sum += value;
      squareSum += value * value;
    }
    __syncthreads();
  }

  if (row < numRows) {
    result[row] = sqrt((squareSum - (sum * sum) / numColumns) / numColumns);
  }
}

template<typename T>
__device__ void rowsStd(const T* matrix, float* result,
                        const int numRows, const int numColumns) {
  common_rowsStd<T, float>(matrix, result, numRows, numColumns);
}

template<typename T>
__device__ void rowsStdd(const T* matrix, double* result,
                         const int numRows, const int numColumns) {
  common_rowsStd<T, double>(matrix, result, numRows, numColumns);
}

template<typename T, typename R>
__device__ void common_columnsStd(const T* matrix, R* result,
                                  const int numRows, const int numColumns) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int col = bx * blockDim.x + tx;
  if (col < numColumns) {
    R sum = 0;
    R squareSum = 0;
    #pragma unroll
    for (int i = 0; i < numRows; i++) {
      int index = i * numColumns + col;
      T value = matrix[index];
      sum += value;
      squareSum += value * value;
    }
    result[col] = sqrt((squareSum - (sum * sum) / numRows) / numRows);
  }
}

template<typename T>
__device__ void columnsStd(const T* matrix, float* result,
                           const int numRows, const int numColumns) {
  common_columnsStd<T, float>(matrix, result, numRows, numColumns);
}

template<typename T>
__device__ void columnsStdd(const T* matrix, double* result,
                            const int numRows, const int numColumns) {
  common_columnsStd<T, double>(matrix, result, numRows, numColumns);
}
