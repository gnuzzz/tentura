#define TILE_DIM 32

template<typename T>
__device__ void matrixDotVector(const T* matrix, const T* vector, T* result,
                                const int matrixRows, const int matrixColumns) {

  __shared__ T matrix_tile[TILE_DIM][TILE_DIM];
  __shared__ T vector_tile[TILE_DIM];

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int baseRow = bx * blockDim.x;
  int index = baseRow + tx;
  T resultValue = 0;

  for (int t = 0; t < (matrixColumns - 1) / TILE_DIM + 1; t++) {
    int column = t * TILE_DIM + tx;
    if (column < matrixColumns) {
      vector_tile[tx] = vector[column];
      for (int i = 0; i < TILE_DIM; i++) {
        int row = baseRow + i;
        if (row < matrixRows) {
          matrix_tile[i][tx] = matrix[row * matrixColumns + column];
        } else {
          matrix_tile[i][tx] = 0;
        }
      }
    } else {
      vector_tile[tx] = 0;
      for (int i = 0; i < TILE_DIM; i++) {
        matrix_tile[i][tx] = 0;
      }
    }
    __syncthreads();

    for (int i = 0; i < TILE_DIM; i++) {
      resultValue += matrix_tile[tx][i] * vector_tile[i];
    }
    __syncthreads();
  }

  if (index < matrixRows) {
    result[index] = resultValue;
  }
}