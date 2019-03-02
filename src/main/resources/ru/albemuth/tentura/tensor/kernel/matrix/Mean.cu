#define TILE_DIM 32

template<typename T, typename R>
__device__ void common_mean(const T* matrix, R* result,
                     const int numRows, const int numColumns) {

  __shared__ T tile[TILE_DIM][TILE_DIM];

  int tx = threadIdx.x;
  int ty = threadIdx.y;
  tile[ty][tx] = 0;

  #pragma unroll
  for (int tr = 0; tr < (numRows - 1) / TILE_DIM + 1; tr++) {
    for (int tc = 0; tc < (numColumns - 1) / TILE_DIM + 1; tc++) {
      int r = tr * TILE_DIM + ty;
      int c = tc * TILE_DIM + tx;
      if (r < numRows && c < numColumns) {
        tile[ty][tx] += matrix[r * numColumns + c];
      } else {
        tile[ty][tx] += 0;
      }
      __syncthreads();
    }
  }

  if (tx == 0 && ty == 0) {
    T sum = 0;
    #pragma unroll
    for (int i = 0; i < TILE_DIM; i++) {
      #pragma unroll
      for (int j = 0; j < TILE_DIM; j++) {
        sum += tile[i][j];
      }
    }
    result[0] = sum / (R) (numRows * numColumns);
  }
}

template<typename T>
__device__ void mean(const T* matrix, float* result,
                     const int numRows, const int numColumns) {
  common_mean<T, float>(matrix, result, numRows, numColumns);
}

template<typename T>
__device__ void meand(const T* matrix, double* result,
                     const int numRows, const int numColumns) {
  common_mean<T, double>(matrix, result, numRows, numColumns);
}

template<typename T, typename R>
__device__ void common_rowsMean(const T* matrix, R* result,
                                const int numRows, const int numColumns) {

  __shared__ T tile[TILE_DIM][TILE_DIM];

  int by = blockIdx.y;
  int ty = threadIdx.y;
  int row = by * blockDim.y + ty;
  T sum = 0;

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
      sum += tile[ty][j];
    }
    __syncthreads();
  }

  if (row < numRows) {
    result[row] = sum / (R) numColumns;
  }
}

template<typename T>
__device__ void rowsMean(const T* matrix, float* result,
                         const int numRows, const int numColumns) {
  common_rowsMean<T, float>(matrix, result, numRows, numColumns);
}

template<typename T>
__device__ void rowsMeand(const T* matrix, double* result,
                          const int numRows, const int numColumns) {
  common_rowsMean<T, double>(matrix, result, numRows, numColumns);
}

template<typename T, typename R>
__device__ void common_columnsMean(const T* matrix, R* result,
                                   const int numRows, const int numColumns) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int col = bx * blockDim.x + tx;
  if (col < numColumns) {
    T sum = 0;
    #pragma unroll
    for (int i = 0; i < numRows; i++) {
      int index = i * numColumns + col;
      sum += matrix[index];
    }
    result[col] = sum / (R) numRows;
  }
}

template<typename T>
__device__ void columnsMean(const T* matrix, float* result,
                            const int numRows, const int numColumns) {
  common_columnsMean<T, float>(matrix, result, numRows, numColumns);
}

template<typename T>
__device__ void columnsMeand(const T* matrix, double* result,
                             const int numRows, const int numColumns) {
  common_columnsMean<T, double>(matrix, result, numRows, numColumns);
}
