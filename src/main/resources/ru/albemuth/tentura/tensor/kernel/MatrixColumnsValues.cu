template<typename T>
__device__ void matrixColumnsValues(const T* matrix, const int* columnsIndices, T* result,
                                    const int numRows, const int numColumns) {

  int by = blockIdx.y;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;

  if (row < numRows) {
    int col = columnsIndices[row];
    result[row] = matrix[row * numColumns + col];
  }

}