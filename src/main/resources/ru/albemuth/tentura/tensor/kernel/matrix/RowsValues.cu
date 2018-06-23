template<typename T>
__device__ void getRowsValues(const T* matrix, const int* indices, T* result,
                              const int rows, const int cols) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int col = bx * blockDim.x + tx;

  if (col < cols) {
    int row = indices[col];
    result[col] = matrix[row * cols + col];
  }

}

template<typename T>
__device__ void setRowsValues(T* matrix, const int* indices, const T* values,
                              const int rows, const int cols) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int col = bx * blockDim.x + tx;

  if (col < cols) {
    int row = indices[col];
    matrix[row * cols + col] = values[col];
  }

}
