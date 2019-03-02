template<typename T>
__device__ void getIndexedValues(const T* matrix, const int* rowsIndices, const int* colsIndices, T* result,
                                 const int rows, const int cols, const int indices_length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < indices_length) {
    int row = rowsIndices[index];
    int col = colsIndices[index];
    result[index] = matrix[row * cols + col];
  }

}

template<typename T>
__device__ void setIndexedValues(T* matrix, const int* rowsIndices, const int* colsIndices, const T* values,
                                 const int rows, const int cols, const int indices_length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < indices_length) {
    int row = rowsIndices[index];
    int col = colsIndices[index];
    matrix[row * cols + col] = values[index];
  }

}

template<typename T>
__device__ void setIndexedValue(T* matrix, const int* rowsIndices, const int* colsIndices, const T value,
                                const int rows, const int cols, const int indices_length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < indices_length) {
    int row = rowsIndices[index];
    int col = colsIndices[index];
    matrix[row * cols + col] = value;
  }

}
