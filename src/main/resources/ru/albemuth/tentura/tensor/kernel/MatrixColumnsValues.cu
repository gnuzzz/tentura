template<typename T>
__device__ void matrixColumnsValues(T* matrix, int* columnsIndices, T* result,
                                    int numRows, int numColumns) {

  int by = blockIdx.y;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;

  if (row < numRows) {
    int col = columnsIndices[row];
    result[row] = matrix[row * numColumns + col];
  }

}