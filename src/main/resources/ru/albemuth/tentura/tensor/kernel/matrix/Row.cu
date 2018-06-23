template<typename T>
__device__ void getRow(const T* matrix, const int row, T* result,
                    const int rows, const int cols) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  result[index] = matrix[row * cols + index];
}

template<typename T>
__device__ void matrixAddRow(const T* matrix, const T* row, T* result,
                             const int rows, const int cols) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int rowIndex = by * blockDim.y + ty;
  int colIndex = bx * blockDim.x + tx;

  if (rowIndex < rows && colIndex < cols) {
    T value = matrix[rowIndex * cols + colIndex] + row[colIndex];
    result[rowIndex * cols + colIndex] = value;
  }
}

template<typename T>
__device__ void rowAddMatrix(const T* row, const T* matrix, T* result,
                             const int rows, const int cols) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int rowIndex = by * blockDim.y + ty;
  int colIndex = bx * blockDim.x + tx;

  if (rowIndex < rows && colIndex < cols) {
    T value = matrix[rowIndex * cols + colIndex] + row[colIndex];
    result[rowIndex * cols + colIndex] = value;
  }
}

template<typename T>
__device__ void matrixSubRow(const T* matrix, const T* row, T* result,
                             const int rows, const int cols) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int rowIndex = by * blockDim.y + ty;
  int colIndex = bx * blockDim.x + tx;

  if (rowIndex < rows && colIndex < cols) {
    T value = matrix[rowIndex * cols + colIndex] - row[colIndex];
    result[rowIndex * cols + colIndex] = value;
  }
}

template<typename T>
__device__ void rowSubMatrix(const T* row, const T* matrix, T* result,
                             const int rows, const int cols) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int rowIndex = by * blockDim.y + ty;
  int colIndex = bx * blockDim.x + tx;

  if (rowIndex < rows && colIndex < cols) {
    T value = row[colIndex] - matrix[rowIndex * cols + colIndex];
    result[rowIndex * cols + colIndex] = value;
  }
}

template<typename T>
__device__ void matrixTimesRow(const T* matrix, const T* row, T* result,
                               const int rows, const int cols) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int rowIndex = by * blockDim.y + ty;
  int colIndex = bx * blockDim.x + tx;

  if (rowIndex < rows && colIndex < cols) {
    T value = matrix[rowIndex * cols + colIndex] * row[colIndex];
    result[rowIndex * cols + colIndex] = value;
  }
}

template<typename T>
__device__ void rowTimesMatrix(const T* row, const T* matrix, T* result,
                               const int rows, const int cols) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int rowIndex = by * blockDim.y + ty;
  int colIndex = bx * blockDim.x + tx;

  if (rowIndex < rows && colIndex < cols) {
    T value = matrix[rowIndex * cols + colIndex] * row[colIndex];
    result[rowIndex * cols + colIndex] = value;
  }
}

template<typename T>
__device__ void matrixDivRow(const T* matrix, const T* row, T* result,
                             const int rows, const int cols) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int rowIndex = by * blockDim.y + ty;
  int colIndex = bx * blockDim.x + tx;

  if (rowIndex < rows && colIndex < cols) {
    T value = matrix[rowIndex * cols + colIndex] / row[colIndex];
    result[rowIndex * cols + colIndex] = value;
  }
}

template<typename T>
__device__ void rowDivMatrix(const T* row, const T* matrix, T* result,
                             const int rows, const int cols) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int rowIndex = by * blockDim.y + ty;
  int colIndex = bx * blockDim.x + tx;

  if (rowIndex < rows && colIndex < cols) {
    T value = row[colIndex] / matrix[rowIndex * cols + colIndex];
    result[rowIndex * cols + colIndex] = value;
  }
}
