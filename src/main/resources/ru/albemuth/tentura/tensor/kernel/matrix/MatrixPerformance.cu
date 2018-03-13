#define TILE_DIM 64
#define BLOCK_ROWS 4

extern "C"
__global__ void matrixAddMatrix(float* A, float* B, float* C, int rows, int columns) {

  int row = blockIdx.y * blockDim.y + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;

  if (col < columns) {
    #pragma unroll
    for (int i = 0; i < TILE_DIM && row + i < rows; i += BLOCK_ROWS) {
      int ij = (row + i) * columns + col;
      C[ij] = A[ij] + B[ij];
    }
  }
}

extern "C"
__global__ void matrixAddScalar(float* A, float scalar, float* C, int rows, int columns) {

  int row = blockIdx.y * blockDim.y + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;

//  if (row < rows && col < columns) {
  if (col < columns) {
    #pragma unroll
    for (int i = 0; i < TILE_DIM && row + i < rows; i += BLOCK_ROWS) {
      int ij = (row + i) * columns + col;
      C[ij] = A[ij] + scalar;
    }
  }
}

extern "C"
__global__ void matrixSigmoid(float* matrix, float* result, int rows, int columns) {

  int row = blockIdx.y * blockDim.y + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;

  if (col < columns) {
    #pragma unroll
    for (int i = 0; i < TILE_DIM && row + i < rows; i += BLOCK_ROWS) {
      int ij = (row + i) * columns + col;
      result[ij] = 1.0f / (1.0f + exp(-matrix[ij]));
    }
  }
}

extern "C"
__global__ void matrixMulMatrix(float* A, float* B, float* C,
                                int aRows, int aColumns,
                                int bRows, int bColumns,
                                int cRows, int cColumns) {

  __shared__ float aTile[TILE_DIM][TILE_DIM];
  __shared__ float bTile[TILE_DIM][TILE_DIM];

  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = blockIdx.y * TILE_DIM + ty;
  int col = blockIdx.x * TILE_DIM + tx;
  float cValue[TILE_DIM / BLOCK_ROWS];
  #pragma unroll
  for (int i = 0; i < TILE_DIM / BLOCK_ROWS; i++) {
    cValue[i] = 0;
  }

  #pragma unroll
  for (int t = 0; t < (aColumns - 1) / TILE_DIM + 1; t++) {
    #pragma unroll
    for (int i = 0; i < TILE_DIM; i += BLOCK_ROWS) {
      if (row + i < aRows && t * TILE_DIM + tx < aColumns) {
        aTile[ty + i][tx] = A[(row + i) * aColumns + t * TILE_DIM + tx];
      } else {
        aTile[ty + i][tx] = 0;
      }
    }
    #pragma unroll
    for (int i = 0; i < TILE_DIM; i += BLOCK_ROWS) {
      if (t * TILE_DIM + ty + i < bRows && col < bColumns) {
        bTile[ty + i][tx] = B[(t * TILE_DIM + ty + i) * bColumns + col];
      } else {
        bTile[ty + i][tx] = 0;
      }
    }
    __syncthreads();

    #pragma unroll
    for (int i = 0, j = 0; i < TILE_DIM && row + i < cRows; i += BLOCK_ROWS, j++) {
      #pragma unroll
      for (int k = 0; k < TILE_DIM; k++) {
        cValue[j] += aTile[ty + i][k] * bTile[k][tx];
      }
    }
    __syncthreads();
  }

  if (col < cColumns) {
    #pragma unroll
    for (int i = 0, j = 0; i < TILE_DIM && row + i < cRows; i += BLOCK_ROWS, j++) {
      C[(row + i) * cColumns + col] = cValue[j];
    }
  }
}