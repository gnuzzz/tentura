template<typename T>
__device__ void getMatrixColumnsValues(const T* matrix, const int* columnsIndices, T* result,
                                       const int numRows, const int numColumns) {

  int by = blockIdx.y;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;

  if (row < numRows) {
    int col = columnsIndices[row];
    result[row] = matrix[row * numColumns + col];
  }

}

template<typename T>
__device__ void setMatrixColumnsValues(T* matrix, const int* columnsIndices, const T* values,
                                       const int numRows, const int numColumns) {

  int by = blockIdx.y;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;

  if (row < numRows) {
    int col = columnsIndices[row];
    matrix[row * numColumns + col] = values[row];
  }

}