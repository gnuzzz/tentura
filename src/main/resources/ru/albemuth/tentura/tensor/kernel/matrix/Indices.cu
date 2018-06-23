template<typename T>
__device__ void columnsIndices(const T* matrix, int* result,
                               const int rows, const int cols) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < rows && col < cols) {
    int ij = row * cols + col;
    result[ij] = col;
  }
}

template<typename T>
__device__ void rowsIndices(const T* matrix, int* result,
                            const int rows, const int cols) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < rows && col < cols) {
    int ij = row * cols + col;
    result[ij] = row;
  }
}