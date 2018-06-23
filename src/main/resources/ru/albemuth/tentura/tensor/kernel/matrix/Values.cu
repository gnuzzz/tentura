template<typename T>
__device__ void matrixRows(const T* matrix, const int* indices, T* result, const int numRows, const int numColumns, const int indicesLength) {
  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < indicesLength && col < numColumns) {
    int resultIndex = row * numColumns + col;
    int matrixIndex = indices[row] * numColumns + col;
    result[resultIndex] = matrix[matrixIndex];
  }
}

template<typename T>
__device__ void matrixColumns(const T* matrix, const int* indices, T* result, const int numRows, const int numColumns, const int indicesLength) {
  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < indicesLength) {
    int resultIndex = row * indicesLength + col;
    int matrixIndex = row * numColumns + indices[col];
    result[resultIndex] = matrix[matrixIndex];
  }
}

template<typename T>
__device__ void vectorValuesMatrix(const T* vector, const int* indices, T* result, const int length, const int indicesNumRows, const int indicesNumColumns) {
  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < indicesNumRows && col < indicesNumColumns) {
    int ij = row * indicesNumColumns + col;
    result[ij] = vector[indices[ij]];
  }
}