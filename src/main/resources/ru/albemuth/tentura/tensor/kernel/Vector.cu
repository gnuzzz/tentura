#define TILE_DIM 32

extern "C"
__global__ void vectorAddVector(float* A, float* B, float* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] + B[index];
  }
}

extern "C"
__global__ void vectorAddScalar(float* A, float scalar, float* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] + scalar;
  }
}

extern "C"
__global__ void vectorSubVector(float* A, float* B, float* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] - B[index];
  }
}

extern "C"
__global__ void vectorSubScalar(float* A, float scalar, float* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] - scalar;
  }
}

extern "C"
__global__ void scalarSubVector(float scalar, float* A, float* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = scalar - A[index];
  }
}

extern "C"
__global__ void vectorMulScalar(float* A, float scalar, float* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] * scalar;
  }
}

extern "C"
__global__ void vectorMulVector(float* A, float* B, float* result, int length) {
  __shared__ float a_tile[TILE_DIM];
  __shared__ float b_tile[TILE_DIM];

  __shared__ float result_tile[TILE_DIM];
  for (int i = 0; i < TILE_DIM; i++) {
    result_tile[i] = 0.0f;
  }

  int tx = threadIdx.x;

  for (int t = 0; t < (length - 1) / TILE_DIM + 1; t++) {
    int index = t * TILE_DIM + tx;
    if (index < length) {
      a_tile[tx] = A[index];
      b_tile[tx] = B[index];
    } else {
      a_tile[tx] = 0.0f;
      b_tile[tx] = 0.0f;
    }
    __syncthreads();

    result_tile[tx] += a_tile[tx] * b_tile[tx];
    __syncthreads();
  }

  float resultValue = 0.0f;
  if (tx == 0) {
    for (int i = 0; i < TILE_DIM; i++) {
      resultValue += result_tile[i];
    }
    result[0] = resultValue;
  }
}

extern "C"
__global__ void matrixMulVector(float* matrix, float* vector, float* result,
                                int matrixRows, int matrixColumns,
                                int vectorLength, int resultLength) {

  __shared__ float matrix_tile[TILE_DIM][TILE_DIM];
  __shared__ float vector_tile[TILE_DIM];

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int baseRow = bx * blockDim.x;
  int index = baseRow + tx;
  float resultValue = 0.0f;

  for (int t = 0; t < (matrixColumns - 1) / TILE_DIM + 1; t++) {
    int column = t * TILE_DIM + tx;
    if (column < vectorLength) {
      vector_tile[tx] = vector[column];
      for (int i = 0; i < TILE_DIM; i++) {
        int row = baseRow + i;
        if (row < matrixRows) {
          matrix_tile[i][tx] = matrix[row * matrixColumns + column];
        } else {
          matrix_tile[i][tx] = 0.0f;
        }
      }
    } else {
      vector_tile[tx] = 0.0f;
      for (int i = 0; i < TILE_DIM; i++) {
        matrix_tile[i][tx] = 0.0f;
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

extern "C"
__global__ void vectorMulMatrix(float* vector, float* matrix, float* result,
                                int vectorLength,
                                int matrixRows, int matrixColumns,
                                int resultLength) {

  __shared__ float vector_tile[TILE_DIM];
  __shared__ float matrix_tile[TILE_DIM][TILE_DIM];

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;
  float resultValue = 0.0f;

  for (int t = 0; t < (matrixRows - 1) / TILE_DIM + 1; t++) {
    int idx = t * TILE_DIM + tx;
    if (idx < vectorLength) {
      vector_tile[tx] = vector[idx];
    } else {
      vector_tile[tx] = 0.0f;
    }
    if (index < matrixColumns) {
      int firstTileRow = t * TILE_DIM;
      for (int i = 0; i < TILE_DIM; i++) {
        int row = firstTileRow + i;
        if (row < matrixRows) {
          matrix_tile[i][tx] = matrix[row * matrixColumns + index];
        } else {
          matrix_tile[i][tx] = 0.0f;
        }
      }
    } else {
      for (int i = 0; i < TILE_DIM; i++) {
        matrix_tile[i][tx] = 0.0f;
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

extern "C"
__global__ void vectorMatrixMulVector(float* vectorA, float* vectorB, float* resultMatrix, int lengthA, int lengthB) {
  __shared__ float vectorA_tile[TILE_DIM];
  __shared__ float vectorB_tile[TILE_DIM];

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

extern "C"
__global__ void vectorElementWiseMulVector(float* A, float* B, float* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] * B[index];
  }
}

extern "C"
__global__ void vectorDivScalar(float* A, float scalar, float* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] / scalar;
  }
}

extern "C"
__global__ void scalarDivVector(float scalar, float* A, float* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = scalar / A[index];
  }
}

extern "C"
__global__ void vectorElementWiseDivVector(float* A, float* B, float* C, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] / B[index];
  }
}

extern "C"
__global__ void vectorSigmoid(float* vector, float* result, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = 1.0f / (1.0f + exp(-vector[index]));
  }
}

extern "C"
__global__ void vectorPow2(float* vector, float* result, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    float vectorIndex = vector[index];
    result[index] = vectorIndex * vectorIndex;
  }
}

extern "C"
__global__ void vectorPow(float* vector, float power, float* result, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = pow(vector[index], power);
  }
}

extern "C"
__global__ void vectorExp(float* vector, float* result, int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = exp(vector[index]);
  }
}

extern "C"
__global__ void vectorSum(float* vector, float* result, int length) {

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