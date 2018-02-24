#define TILE_DIM 32
#define BLOCK_ROWS 4

extern "C"
__global__ void matrixAddMatrix(float* A, float* B, float* C, int rows, int columns) {

  int row = blockIdx.y * blockDim.y + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;

  if (col < columns) {
    #pragma unroll
    for (int i = 0; i < TILE_DIM && row + i < rows; i += BLOCK_ROWS) {
      int ij = (row + i) * columns + col;
      C[ij] = A[ij] + B[ij];
    }
  }
}

extern "C"
__global__ void matrixAddScalar(float* A, float scalar, float* C,
                                int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    int ij = row * numColumns + col;
    C[ij] = A[ij] + scalar;
  }
}

extern "C"
__global__ void matrixSubMatrix(float* A, float* B, float* C,
                                int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    int ij = row * numColumns + col;
    C[ij] = A[ij] - B[ij];
  }
}

extern "C"
__global__ void matrixSubScalar(float* A, float scalar, float* C,
                                int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    int ij = row * numColumns + col;
    C[ij] = A[ij] - scalar;
  }
}

extern "C"
__global__ void scalarSubMatrix(float scalar, float* A, float* C,
                                int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    int ij = row * numColumns + col;
    C[ij] = scalar - A[ij];
  }
}

extern "C"
__global__ void matrixMulMatrix(float* A, float* B, float* C,
                                int numARows, int numAColumns,
                                int numBRows, int numBColumns,
                                int numCRows, int numCColumns) {

  __shared__ float ds_A[TILE_DIM][TILE_DIM];
  __shared__ float ds_B[TILE_DIM][TILE_DIM];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;
  float cValue = 0;

  #pragma unroll
  for (int t = 0; t < (numAColumns - 1) / TILE_DIM + 1; t++) {
    if (row < numARows && t * TILE_DIM + tx < numAColumns) {
      ds_A[ty][tx] = A[row * numAColumns + t * TILE_DIM + tx];
    } else {
      ds_A[ty][tx] = 0.0;
    }
    if (t * TILE_DIM + ty < numBRows && col < numBColumns) {
      ds_B[ty][tx] = B[(t * TILE_DIM + ty) * numBColumns + col];
    } else {
      ds_B[ty][tx] = 0.0;
    }
    __syncthreads();

    #pragma unroll
    for (int i = 0; i < TILE_DIM; i++) {
      cValue += ds_A[ty][i] * ds_B[i][tx];
    }
    __syncthreads();
  }

  if (row < numCRows && col < numCColumns) {
    C[row * numCColumns + col] = cValue;
  }
}

extern "C"
__global__ void matrixMulScalar(float* A, float scalar, float* C,
                                int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    int ij = row * numColumns + col;
    C[ij] = A[ij] * scalar;
  }
}

extern "C"
__global__ void matrixElementWiseMulMatrix(float* A, float* B, float* C,
                                int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    int ij = row * numColumns + col;
    C[ij] = A[ij] * B[ij];
  }
}

extern "C"
__global__ void matrixDivScalar(float* A, float scalar, float* C,
                                int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    int ij = row * numColumns + col;
    C[ij] = A[ij] / scalar;
  }
}

extern "C"
__global__ void scalarDivMatrix(float scalar, float* A, float* C,
                                int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    int ij = row * numColumns + col;
    C[ij] = scalar / A[ij];
  }
}

extern "C"
__global__ void matrixElementWiseDivMatrix(float* A, float* B, float* C,
                                int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    int ij = row * numColumns + col;
    C[ij] = A[ij] / B[ij];
  }
}

extern "C"
__global__ void matrixTranspose(const float* matrix, float* result, const int rows, const int columns) {
  __shared__ float tile[TILE_DIM][TILE_DIM + 1];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * TILE_DIM + ty;
  int col = bx * TILE_DIM + tx;
  int srcRow = bx * TILE_DIM + ty;
  int srcCol = by * TILE_DIM + tx;

  if (srcCol < columns) {
    #pragma unroll
    for (int i = 0; i < TILE_DIM && srcRow + i < rows; i += BLOCK_ROWS) {
      tile[ty + i][tx] = matrix[(srcRow + i) * columns + srcCol];
    }
  }
  __syncthreads();

  if (col < rows) {
    #pragma unroll
    for (int i = 0; i < TILE_DIM && row + i < columns; i += BLOCK_ROWS) {
      result[(row + i) * rows + col] = tile[tx][ty + i];
    }
  }
}

extern "C"
__global__ void matrixRow(float* matrix, float* result, int row,
                          int matrixRows, int matrixColumns) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  result[index] = matrix[row * matrixColumns + index];
}

extern "C"
__global__ void matrixColumn(float* matrix, float* result, int column,
                          int matrixRows, int matrixColumns) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  result[index] = matrix[index * matrixColumns + column];
}

extern "C"
__global__ void matrixPow2(float* matrix, float* result,
                          int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    int ij = row * numColumns + col;
    float mij = matrix[ij];
    result[ij] = mij * mij;
  }
}

extern "C"
__global__ void matrixPow(float* matrix, float power, float* result,
                          int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    int ij = row * numColumns + col;
    result[ij] = pow(matrix[ij], power);
  }
}

extern "C"
__global__ void matrixExp(float* matrix, float* result,
                          int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    int ij = row * numColumns + col;
    result[ij] = exp(matrix[ij]);
  }
}

extern "C"
__global__ void matrixSigmoid(float* matrix, float* result,
                              int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    int ij = row * numColumns + col;
    result[ij] = 1.0f / (1.0f + exp(-matrix[ij]));
  }
}

extern "C"
__global__ void matrixSum(float* matrix, float* result,
                          int numRows, int numColumns) {

  __shared__ float tile[TILE_DIM][TILE_DIM];

  int tx = threadIdx.x;
  int ty = threadIdx.y;
  tile[ty][tx] = 0;

  #pragma unroll
  for (int tr = 0; tr < (numRows - 1) / TILE_DIM + 1; tr++) {
    for (int tc = 0; tc < (numColumns - 1) / TILE_DIM + 1; tc++) {
      int r = tr * TILE_DIM + ty;
      int c = tc * TILE_DIM + tx;
      if (r < numRows && c < numColumns) {
        tile[ty][tx] += matrix[r * numColumns + c];
      } else {
        tile[ty][tx] += 0;
      }
      __syncthreads();
    }
  }

  if (tx == 0 && ty == 0) {
    float sum = 0;
    #pragma unroll
    for (int i = 0; i < TILE_DIM; i++) {
      #pragma unroll
      for (int j = 0; j < TILE_DIM; j++) {
        sum += tile[i][j];
      }
    }
    result[0] = sum;
  }
}

extern "C"
__global__ void matrixSumRows(float* matrix, float* result,
                              int numRows, int numColumns) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int col = bx * blockDim.x + tx;
  if (col < numColumns) {
    float sum = 0;
    #pragma unroll
    for (int i = 0; i < numRows; i++) {
      int index = i * numColumns + col;
      sum += matrix[index];
    }
    result[col] = sum;
  }
}

extern "C"
__global__ void matrixSumColumns(float* matrix, float* result,
                                 int numRows, int numColumns) {

  __shared__ float tile[TILE_DIM][TILE_DIM];

  int by = blockIdx.y;
  int ty = threadIdx.y;
  int row = by * blockDim.y + ty;
  float sum = 0;

  #pragma unroll
  for (int t = 0; t < (numColumns - 1) / TILE_DIM + 1; t++) {
    #pragma unroll
    for (int i = 0; i < TILE_DIM; i++) {
      int r = by * TILE_DIM + i;
      int c = t * TILE_DIM + ty;
      if (r < numRows && c < numColumns) {
        tile[i][ty] = matrix[r * numColumns + c];
      } else {
        tile[i][ty] = 0;
      }
    }
    __syncthreads();

    #pragma unroll
    for (int j = 0; j < TILE_DIM; j++) {
      sum += tile[ty][j];
    }
    __syncthreads();
  }

  if (row < numRows) {
    result[row] = sum;
  }
}

extern "C"
__global__ void matrixAddRow(float* matrix, float* vector, float* result,
                             int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    float sum = matrix[row * numColumns + col] + vector[col];
    result[row * numColumns + col] = sum;
  }
}

extern "C"
__global__ void vectorRowAddMatrix(float* vector, float* matrix, float* result,
                                   int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    float sum = matrix[row * numColumns + col] + vector[col];
    result[row * numColumns + col] = sum;
  }
}

extern "C"
__global__ void matrixAddColumn(float* matrix, float* vector, float* result,
                                int numRows, int numColumns) {

  __shared__ float tile[TILE_DIM];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (ty == 0) {
    int r = row + tx;
    if (r < numRows) {
      tile[tx] = vector[r];
    } else {
      tile[tx] = 0;
    }
  }
  __syncthreads();

  if (row < numRows && col < numColumns) {
    float sum = matrix[row * numColumns + col] + tile[ty];
    result[row * numColumns + col] = sum;
  }
}

extern "C"
__global__ void vectorColumnAddMatrix(float* vector, float* matrix, float* result,
                                      int numRows, int numColumns) {

  __shared__ float tile[TILE_DIM];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (ty == 0) {
    int r = row + tx;
    if (r < numRows) {
      tile[tx] = vector[r];
    } else {
      tile[tx] = 0;
    }
  }
  __syncthreads();

  if (row < numRows && col < numColumns) {
    float sum = tile[ty] + matrix[row * numColumns + col];
    result[row * numColumns + col] = sum;
  }
}

extern "C"
__global__ void matrixSubRow(float* matrix, float* vector, float* result,
                             int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    float sum = matrix[row * numColumns + col] - vector[col];
    result[row * numColumns + col] = sum;
  }
}

extern "C"
__global__ void vectorRowSubMatrix(float* vector, float* matrix, float* result,
                                   int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    float sum = vector[col] - matrix[row * numColumns + col];
    result[row * numColumns + col] = sum;
  }
}

extern "C"
__global__ void matrixSubColumn(float* matrix, float* vector, float* result,
                                int numRows, int numColumns) {

  __shared__ float tile[TILE_DIM];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (ty == 0) {
    int r = row + tx;
    if (r < numRows) {
      tile[tx] = vector[r];
    } else {
      tile[tx] = 0;
    }
  }
  __syncthreads();

  if (row < numRows && col < numColumns) {
    float sum = matrix[row * numColumns + col] - tile[ty];
    result[row * numColumns + col] = sum;
  }
}

extern "C"
__global__ void vectorColumnSubMatrix(float* vector, float* matrix, float* result,
                                      int numRows, int numColumns) {

  __shared__ float tile[TILE_DIM];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (ty == 0) {
    int r = row + tx;
    if (r < numRows) {
      tile[tx] = vector[r];
    } else {
      tile[tx] = 0;
    }
  }
  __syncthreads();

  if (row < numRows && col < numColumns) {
    float sum = tile[ty] - matrix[row * numColumns + col];
    result[row * numColumns + col] = sum;
  }
}

extern "C"
__global__ void matrixColumnsValues(float* matrix, int* columnsIndices, float* result,
                                    int numRows, int numColumns) {

  int by = blockIdx.y;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;

  if (row < numRows) {
    int col = columnsIndices[row];
    result[row] = matrix[row * numColumns + col];
  }

}