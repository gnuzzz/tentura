#define TILE_DIM 32

template<typename T>
__device__ void vectorDotVector(const T* A, const T* B, T* result, const int length) {
  __shared__ T a_tile[TILE_DIM];
  __shared__ T b_tile[TILE_DIM];

  __shared__ T result_tile[TILE_DIM];
  for (int i = 0; i < TILE_DIM; i++) {
    result_tile[i] = 0;
  }

  int tx = threadIdx.x;

  for (int t = 0; t < (length - 1) / TILE_DIM + 1; t++) {
    int index = t * TILE_DIM + tx;
    if (index < length) {
      a_tile[tx] = A[index];
      b_tile[tx] = B[index];
    } else {
      a_tile[tx] = 0;
      b_tile[tx] = 0;
    }
    __syncthreads();

    result_tile[tx] += a_tile[tx] * b_tile[tx];
    __syncthreads();
  }

  T resultValue = 0;
  if (tx == 0) {
    for (int i = 0; i < TILE_DIM; i++) {
      resultValue += result_tile[i];
    }
    result[0] = resultValue;
  }
}

template<typename T>
__device__ void columnDotRow(const T* vectorA, const T* vectorB, T* resultMatrix,
                             const int lengthA, const int lengthB) {
  __shared__ T vectorA_tile[TILE_DIM];
  __shared__ T vectorB_tile[TILE_DIM];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (ty == 0) {
    if (row + tx < lengthA) {
      vectorA_tile[tx] = vectorA[row + tx];
    }
    if (col < lengthB) {
      vectorB_tile[tx] = vectorB[col];
    }
  }
  __syncthreads();

  if (row < lengthA && col < lengthB) {
    resultMatrix[row * lengthB + col] = vectorA_tile[ty] * vectorB_tile[tx];
  }
}