#define TILE_DIM 32

template<typename T>
__device__ void vectorAddVector(T* A, T* B, T* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] + B[index];
  }
}

template<typename T>
__device__ void vectorAddScalar(T* A, T scalar, T* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] + scalar;
  }
}

template<typename T>
__device__ void vectorSubVector(T* A, T* B, T* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] - B[index];
  }
}

template<typename T>
__device__ void vectorSubScalar(T* A, T scalar, T* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] - scalar;
  }
}

template<typename T>
__device__ void scalarSubVector(T scalar, T* A, T* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = scalar - A[index];
  }
}

template<typename T>
__device__ void vectorMulScalar(T* A, T scalar, T* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] * scalar;
  }
}

template<typename T>
__device__ void vectorMulVector(T* A, T* B, T* result, int length) {
  __shared__ T a_tile[TILE_DIM];
  __shared__ T b_tile[TILE_DIM];

  __shared__ T result_tile[TILE_DIM];
  for (int i = 0; i < TILE_DIM; i++) {
    result_tile[i] = 0;
  }

  int tx = threadIdx.x;

  for (int t = 0; t < (length - 1) / TILE_DIM + 1; t++) {
    int index = t * TILE_DIM + tx;
    if (index < length) {
      a_tile[tx] = A[index];
      b_tile[tx] = B[index];
    } else {
      a_tile[tx] = 0;
      b_tile[tx] = 0;
    }
    __syncthreads();

    result_tile[tx] += a_tile[tx] * b_tile[tx];
    __syncthreads();
  }

  T resultValue = 0;
  if (tx == 0) {
    for (int i = 0; i < TILE_DIM; i++) {
      resultValue += result_tile[i];
    }
    result[0] = resultValue;
  }
}

template<typename T>
__device__ void matrixMulVector(T* matrix, T* vector, T* result,
                                int matrixRows, int matrixColumns,
                                int vectorLength, int resultLength) {

  __shared__ T matrix_tile[TILE_DIM][TILE_DIM];
  __shared__ T vector_tile[TILE_DIM];

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int baseRow = bx * blockDim.x;
  int index = baseRow + tx;
  T resultValue = 0;

  for (int t = 0; t < (matrixColumns - 1) / TILE_DIM + 1; t++) {
    int column = t * TILE_DIM + tx;
    if (column < vectorLength) {
      vector_tile[tx] = vector[column];
      for (int i = 0; i < TILE_DIM; i++) {
        int row = baseRow + i;
        if (row < matrixRows) {
          matrix_tile[i][tx] = matrix[row * matrixColumns + column];
        } else {
          matrix_tile[i][tx] = 0;
        }
      }
    } else {
      vector_tile[tx] = 0;
      for (int i = 0; i < TILE_DIM; i++) {
        matrix_tile[i][tx] = 0;
      }
    }
    __syncthreads();

    for (int i = 0; i < TILE_DIM; i++) {
      resultValue += matrix_tile[tx][i] * vector_tile[i];
    }
    __syncthreads();
  }

  if (index < resultLength) {
    result[index] = resultValue;
  }
}

template<typename T>
__device__ void vectorMulMatrix(T* vector, T* matrix, T* result,
                                int vectorLength,
                                int matrixRows, int matrixColumns,
                                int resultLength) {

  __shared__ T vector_tile[TILE_DIM];
  __shared__ T matrix_tile[TILE_DIM][TILE_DIM];

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;
  T resultValue = 0;

  for (int t = 0; t < (matrixRows - 1) / TILE_DIM + 1; t++) {
    int idx = t * TILE_DIM + tx;
    if (idx < vectorLength) {
      vector_tile[tx] = vector[idx];
    } else {
      vector_tile[tx] = 0;
    }
    if (index < matrixColumns) {
      int firstTileRow = t * TILE_DIM;
      for (int i = 0; i < TILE_DIM; i++) {
        int row = firstTileRow + i;
        if (row < matrixRows) {
          matrix_tile[i][tx] = matrix[row * matrixColumns + index];
        } else {
          matrix_tile[i][tx] = 0;
        }
      }
    } else {
      for (int i = 0; i < TILE_DIM; i++) {
        matrix_tile[i][tx] = 0;
      }
    }
    __syncthreads();

    for (int i = 0; i < TILE_DIM; i++) {
      resultValue += vector_tile[i] * matrix_tile[i][tx];
    }
    __syncthreads();
  }

  if (index < resultLength) {
    result[index] = resultValue;
  }

}

template<typename T>
__device__ void vectorMatrixMulVector(T* vectorA, T* vectorB, T* resultMatrix, int lengthA, int lengthB) {
  __shared__ T vectorA_tile[TILE_DIM];
  __shared__ T vectorB_tile[TILE_DIM];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (ty == 0) {
    if (row + tx < lengthA) {
      vectorA_tile[tx] = vectorA[row + tx];
    }
    if (col < lengthB) {
      vectorB_tile[tx] = vectorB[col];
    }
  }
  __syncthreads();

  if (row < lengthA && col < lengthB) {
    resultMatrix[row * lengthB + col] = vectorA_tile[ty] * vectorB_tile[tx];
  }
}

template<typename T>
__device__ void vectorElementWiseMulVector(T* A, T* B, T* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] * B[index];
  }
}

template<typename T>
__device__ void vectorDivScalar(T* A, T scalar, T* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] / scalar;
  }
}

template<typename T>
__device__ void scalarDivVector(T scalar, T* A, T* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = scalar / A[index];
  }
}

template<typename T>
__device__ void vectorElementWiseDivVector(T* A, T* B, T* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] / B[index];
  }
}

template<typename T>
__device__ void vectorSum(T* vector, T* result, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index == 0) {
    float sum = 0;
    for (int i = 0; i < length; i++) {
      sum += vector[i];
    }
    result[0] = sum;
  }
}