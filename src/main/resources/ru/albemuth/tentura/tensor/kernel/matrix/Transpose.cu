#define TILE_DIM 32
#define BLOCK_ROWS 4

template<typename T>
__device__ void transpose(const T* matrix, T* result,
                                const int rows, const int cols) {
  __shared__ T tile[TILE_DIM][TILE_DIM + 1];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * TILE_DIM + ty;
  int col = bx * TILE_DIM + tx;
  int srcRow = bx * TILE_DIM + ty;
  int srcCol = by * TILE_DIM + tx;

  if (srcCol < cols) {
    #pragma unroll
    for (int i = 0; i < TILE_DIM && srcRow + i < rows; i += BLOCK_ROWS) {
      tile[ty + i][tx] = matrix[(srcRow + i) * cols + srcCol];
    }
  }
  __syncthreads();

  if (col < rows) {
    #pragma unroll
    for (int i = 0; i < TILE_DIM && row + i < cols; i += BLOCK_ROWS) {
      result[(row + i) * rows + col] = tile[tx][ty + i];
    }
  }
}