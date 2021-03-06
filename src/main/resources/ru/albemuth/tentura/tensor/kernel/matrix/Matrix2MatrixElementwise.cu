template<typename T>
__device__ void plus(const T* matrix1, const T* matrix2, T* result,
                     const int rows, const int cols) {

  int gridHeight = gridDim.y;
  int tileHeight = rows / gridHeight + (rows % gridHeight == 0 ? 0 : 1);
  int blockRows = blockDim.y;

  int row = blockIdx.y * tileHeight + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;

  if (col < cols) {
    #pragma unroll
    for (int current_row = row; current_row < row + tileHeight && current_row < rows ; current_row += blockRows) {
      int ij = current_row * cols + col;
      result[ij] = matrix1[ij] + matrix2[ij];
    }
  }
}

template<typename T>
__device__ void minus(const T* matrix1, const T* matrix2, T* result,
                      const int rows, const int cols) {

  int gridHeight = gridDim.y;
  int tileHeight = rows / gridHeight + (rows % gridHeight == 0 ? 0 : 1);
  int blockRows = blockDim.y;

  int row = blockIdx.y * tileHeight + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;

  if (col < cols) {
    #pragma unroll
    for (int current_row = row; current_row < row + tileHeight && current_row < rows ; current_row += blockRows) {
      int ij = current_row * cols + col;
      result[ij] = matrix1[ij] - matrix2[ij];
    }
  }
}

template<typename T>
__device__ void times(const T* matrix1, const T* matrix2, T* result,
                      const int rows, const int cols) {

  int gridHeight = gridDim.y;
  int tileHeight = rows / gridHeight + (rows % gridHeight == 0 ? 0 : 1);
  int blockRows = blockDim.y;

  int row = blockIdx.y * tileHeight + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;

  if (col < cols) {
    #pragma unroll
    for (int current_row = row; current_row < row + tileHeight && current_row < rows ; current_row += blockRows) {
      int ij = current_row * cols + col;
      result[ij] = matrix1[ij] * matrix2[ij];
    }
  }
}

template<typename T>
__device__ void div(const T* matrix1, const T* matrix2, T* result,
                    const int rows, const int cols) {

  int gridHeight = gridDim.y;
  int tileHeight = rows / gridHeight + (rows % gridHeight == 0 ? 0 : 1);
  int blockRows = blockDim.y;

  int row = blockIdx.y * tileHeight + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;

  if (col < cols) {
    #pragma unroll
    for (int current_row = row; current_row < row + tileHeight && current_row < rows ; current_row += blockRows) {
      int ij = current_row * cols + col;
      result[ij] = matrix1[ij] / matrix2[ij];
    }
  }
}