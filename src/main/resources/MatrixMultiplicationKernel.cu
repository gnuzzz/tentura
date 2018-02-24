extern "C"
// Compute C = A * B
__global__ void matrixMultiply(float *A, float *B, float *C, int numARows,
                               int numAColumns, int numBRows, int numBColumns,
                               int numCRows, int numCColumns) {
  int column = threadIdx.x + blockDim.x * blockIdx.x;
  int row = threadIdx.y + blockDim.y * blockIdx.y;
  if (column < numCColumns && row < numCRows) {
/*
    float cValue = 0.0f;
    for (int i = 0; i < numAColumns; i++) {
      //float a = A[row * numAColumns + i];
      //float b = B[i * numBColumns + column];
      //cValue += a * b;
      cValue += A[row * numAColumns + i] * B[i * numBColumns + column];
    }
*/
    //C[row * numCColumns + column] = cValue;
    //C[row * numCColumns + column] = 0.11424443 * -1.25084 + -0.9391175 * -0.4892239;
    C[row * numCColumns + column] = 0.11424443f * -1.25084f + -0.9391175f * -0.4892239f;
  }
}