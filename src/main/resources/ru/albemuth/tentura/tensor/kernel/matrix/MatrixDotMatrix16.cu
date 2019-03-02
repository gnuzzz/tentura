#define TILE_DIM 16

template<typename T>
__device__ void matrixDotMatrix(const T* matrixA, const T* matrixB, T* result,
                                const int rowsA, const int colsA, const int rowsB, const int colsB) {

  __shared__ T tileA[TILE_DIM][TILE_DIM];
  __shared__ T tileB[TILE_DIM][TILE_DIM];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;
  T value = 0;

  #pragma unroll
  for (int t = 0; t < (colsA - 1) / TILE_DIM + 1; t++) {
    if (row < rowsA && t * TILE_DIM + tx < colsA) {
      tileA[ty][tx] = matrixA[row * colsA + t * TILE_DIM + tx];
    } else {
      tileA[ty][tx] = 0;
    }
    if (t * TILE_DIM + ty < rowsB && col < colsB) {
      tileB[ty][tx] = matrixB[(t * TILE_DIM + ty) * colsB + col];
    } else {
      tileB[ty][tx] = 0;
    }
    __syncthreads();

    #pragma unroll
    for (int i = 0; i < TILE_DIM; i++) {
      value += tileA[ty][i] * tileB[i][tx];
    }
    __syncthreads();
  }

  if (row < rowsA && col < colsB) {
    result[row * colsB + col] = value;
  }
}
