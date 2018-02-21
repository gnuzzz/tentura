template<typename T>
__device__ void vectorMulVector(T* A, T* B, T* result, int length) {
  T resultValue = 0;
  for (int i = 0; i < length; i++) {
    resultValue += A[i] * B[i];
  }
  result[0] = resultValue;
}

template<typename T>
__device__ void matrixMulVector(T* matrix, T* vector, T* result,
                                int matrixRows, int matrixColumns,
                                int vectorLength, int resultLength) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < resultLength) {
    T resultValue = 0;
    int rowStart = index * matrixColumns;
    for (int i = 0; i < vectorLength; i++) {
      resultValue += matrix[rowStart + i] * vector[i];
    }

    result[index] = resultValue;
  }

}

template<typename T>
__device__ void vectorMulMatrix(T* vector, T* matrix, T* result,
                                int vectorLength,
                                int matrixRows, int matrixColumns,
                                int resultLength) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < resultLength) {
    T resultValue = 0;
    for (int i = 0; i < vectorLength; i++) {
      resultValue += vector[i] * matrix[index + i * matrixColumns];
    }
    result[index] = resultValue;
  }

}

template<typename T>
__device__ void vectorMatrixMulVector(T* vectorA, T* vectorB, T* resultMatrix, int lengthA, int lengthB) {
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