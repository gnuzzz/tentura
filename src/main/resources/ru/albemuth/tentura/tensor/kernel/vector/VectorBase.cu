template<typename T>
__device__ void vectorMulVector(const T* A, const T* B, T* result, const int length) {
  T resultValue = 0;
  for (int i = 0; i < length; i++) {
    resultValue += A[i] * B[i];
  }
  result[0] = resultValue;
}

template<typename T>
__device__ void matrixMulVector(const T* matrix, const T* vector, T* result,
                                const int matrixRows, const int matrixColumns) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < matrixColumns) {
    T resultValue = 0;
    int rowStart = index * matrixColumns;
    for (int i = 0; i < matrixColumns; i++) {
      resultValue += matrix[rowStart + i] * vector[i];
    }

    result[index] = resultValue;
  }

}

template<typename T>
__device__ void vectorMulMatrix(const T* vector, const T* matrix, T* result,
                                const int matrixRows, const int matrixColumns) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < matrixRows) {
    T resultValue = 0;
    for (int i = 0; i < matrixRows; i++) {
      resultValue += vector[i] * matrix[index + i * matrixColumns];
    }
    result[index] = resultValue;
  }

}

template<typename T>
__device__ void vectorMatrixMulVector(const T* vectorA, const T* vectorB, T* resultMatrix,
                                      const int lengthA, const int lengthB) {
  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < lengthA && col < lengthB) {
    resultMatrix[row * lengthB + col] = vectorA[row] * vectorB[col];
  }
}