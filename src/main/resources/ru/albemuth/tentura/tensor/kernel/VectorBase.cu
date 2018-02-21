extern "C"
__global__ void vectorMulVector(float* A, float* B, float* result, int length) {
  float resultValue = 0.0f;
  for (int i = 0; i < length; i++) {
    resultValue += A[i] * B[i];
  }
  result[0] = resultValue;
}

extern "C"
__global__ void matrixMulVector(float* matrix, float* vector, float* result,
                                int matrixRows, int matrixColumns,
                                int vectorLength, int resultLength) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < resultLength) {
    float resultValue = 0.0f;
    int rowStart = index * matrixColumns;
    for (int i = 0; i < vectorLength; i++) {
      resultValue += matrix[rowStart + i] * vector[i];
    }

    result[index] = resultValue;
  }

}

extern "C"
__global__ void vectorMulMatrix(float* vector, float* matrix, float* result,
                                int vectorLength,
                                int matrixRows, int matrixColumns,
                                int resultLength) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < resultLength) {
    float resultValue = 0.0f;
    for (int i = 0; i < vectorLength; i++) {
      resultValue += vector[i] * matrix[index + i * matrixColumns];
    }
    result[index] = resultValue;
  }

}

extern "C"
__global__ void vectorMatrixMulVector(float* vectorA, float* vectorB, float* resultMatrix, int lengthA, int lengthB) {
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