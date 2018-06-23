#define TILE_DIM 32

template<typename T>
__device__ void vectorAddVector(const T* A, const T* B, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] + B[index];
  }
}

template<typename T>
__device__ void vectorAddScalar(const T* A, const T scalar, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] + scalar;
  }
}

template<typename T>
__device__ void vectorSubVector(const T* A, const T* B, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] - B[index];
  }
}

template<typename T>
__device__ void vectorSubScalar(const T* A, const T scalar, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] - scalar;
  }
}

template<typename T>
__device__ void scalarSubVector(const T* A, const T scalar, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = scalar - A[index];
  }
}

template<typename T>
__device__ void vectorMulScalar(const T* A, const T scalar, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] * scalar;
  }
}

template<typename T>
__device__ void vectorTimesVector(const T* A, const T* B, T* result, const int length) {
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
__device__ void matrixMulVector(const T* matrix, const T* vector, T* result,
                                const int matrixRows, const int matrixColumns) {

  __shared__ T matrix_tile[TILE_DIM][TILE_DIM];
  __shared__ T vector_tile[TILE_DIM];

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int baseRow = bx * blockDim.x;
  int index = baseRow + tx;
  T resultValue = 0;

  for (int t = 0; t < (matrixColumns - 1) / TILE_DIM + 1; t++) {
    int column = t * TILE_DIM + tx;
    if (column < matrixColumns) {
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

  if (index < matrixRows) {
    result[index] = resultValue;
  }
}

template<typename T>
__device__ void vectorMatrixMulVector(const T* vectorA, const T* vectorB, T* resultMatrix,
                                      const int lengthA, const int lengthB) {
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
__device__ void vectorElementWiseMulVector(const T* A, const T* B, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] * B[index];
  }
}

template<typename T>
__device__ void vectorDivScalar(const T* A, const T scalar, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] / scalar;
  }
}

template<typename T>
__device__ void scalarDivVector(const T* A, const T scalar, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = scalar / A[index];
  }
}

template<typename T>
__device__ void vectorElementWiseDivVector(const T* A, const T* B, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] / B[index];
  }
}

template<typename T>
__device__ void vectorSum(const T* vector, T* result, const int length) {

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

template<typename T>
__device__ void value(const T* vector, const int index, T* result, const int length) {
  result[0] = vector[index];
}

template<typename T>
__device__ void values(const T* vector, const int* indices, T* result, const int length, const int indicesLength) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < indicesLength) {
    result[index] = vector[indices[index]];
  }
}

template<typename T>
__device__ void slice(const T* vector, const int from, const int to, T* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;
  int vectorIndex = from + index;

  if (vectorIndex < to + index) {
    result[index] = vector[vectorIndex];
  }
}

template<typename T>
__device__ void vectorIndices(const T* vector, int* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = index;
  }
}