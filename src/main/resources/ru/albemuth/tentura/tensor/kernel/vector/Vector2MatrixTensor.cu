#define TILE_DIM 32

template<typename T>
__device__ void vectorDotMatrix(const T* vector, const T* matrix, T* result,
                                const int rows, const int columns) {

  __shared__ T vector_tile[TILE_DIM];
  __shared__ T matrix_tile[TILE_DIM][TILE_DIM];

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;
  T resultValue = 0;

  for (int t = 0; t < (rows - 1) / TILE_DIM + 1; t++) {
    int idx = t * TILE_DIM + tx;
    if (idx < rows) {
      vector_tile[tx] = vector[idx];
    } else {
      vector_tile[tx] = 0;
    }
    if (index < columns) {
      int firstTileRow = t * TILE_DIM;
      for (int i = 0; i < TILE_DIM; i++) {
        int row = firstTileRow + i;
        if (row < rows) {
          matrix_tile[i][tx] = matrix[row * columns + index];
        } else {
          matrix_tile[i][tx] = 0;
        }
      }
    } else {
      for (int i = 0; i < TILE_DIM; i++) {
        matrix_tile[i][tx] = 0;
      }
    }
    __syncthreads();

    for (int i = 0; i < TILE_DIM; i++) {
      resultValue += vector_tile[i] * matrix_tile[i][tx];
    }
    __syncthreads();
  }

  if (index < columns) {
    result[index] = resultValue;
  }

}
