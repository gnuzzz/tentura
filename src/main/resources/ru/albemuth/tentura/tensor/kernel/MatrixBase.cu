template<typename T>
__device__ void matrixMulMatrix(const T *Aconst , T *B, T *C,
                                const int numARows, const int numAColumns,
                                const int numBRows, const int numBColumns) {
  int column = threadIdx.x + blockDim.x * blockIdx.x;
  int row = threadIdx.y + blockDim.y * blockIdx.y;
  if (column < numCColumns && row < numCRows) {
    T cValue = 0;
    for (int i = 0; i < numAColumns; i++) {
      cValue += A[row * numAColumns + i] * B[i * numBColumns + column];
    }
    C[row * numCColumns + column] = cValue;
  }
}

template<typename T>
__device__ void matrixTranspose(T* A, T* C,
                                int numARows, int numAColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numAColumns && col < numARows) {
    int ij = row * numARows + col;
    int ji = col * numAColumns + row;
    C[ij] = A[ji];
  }
}