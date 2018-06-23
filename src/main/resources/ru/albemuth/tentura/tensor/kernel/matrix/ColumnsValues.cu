template<typename T>
__device__ void getColumnsValues(const T* matrix, const int* indices, T* result,
                                 const int rows, const int cols) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int row = bx * blockDim.x + tx;

  if (row < rows) {
    int col = indices[row];
    result[row] = matrix[row * cols + col];
  }

}

template<typename T>
__device__ void setColumnsValues(T* matrix, const int* indices, const T* values,
                                 const int rows, const int cols) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int row = bx * blockDim.x + tx;

  if (row < rows) {
    int col = indices[row];
    matrix[row * cols + col] = values[row];
  }

}

template<typename T>
__device__ void setColumnsValue(T* matrix, const int* indices, const T value,
                                const int rows, const int cols) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int row = bx * blockDim.x + tx;

  if (row < rows) {
    int col = indices[row];
    matrix[row * cols + col] = value;
  }

}