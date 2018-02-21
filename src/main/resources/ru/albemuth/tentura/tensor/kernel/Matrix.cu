#define TILE_DIM 32
#define BLOCK_ROWS 4

template<typename T>
__device__ void matrixAddMatrix(T* A, T* B, T* C, int rows, int columns) {

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

template<typename T>
__device__ void matrixAddScalar(T* A, T scalar, T* C,
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

template<typename T>
__device__ void matrixSubMatrix(T* A, T* B, T* C,
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

template<typename T>
__device__ void matrixSubScalar(T* A, T scalar, T* C,
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

template<typename T>
__device__ void scalarSubMatrix(T scalar, T* A, T* C,
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

template<typename T>
__device__ void matrixMulMatrix(T* A, T* B, T* C,
                                int numARows, int numAColumns,
                                int numBRows, int numBColumns,
                                int numCRows, int numCColumns) {

  __shared__ T ds_A[TILE_DIM][TILE_DIM];
  __shared__ T ds_B[TILE_DIM][TILE_DIM];

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
      ds_A[ty][tx] = 0;
    }
    if (t * TILE_DIM + ty < numBRows && col < numBColumns) {
      ds_B[ty][tx] = B[(t * TILE_DIM + ty) * numBColumns + col];
    } else {
      ds_B[ty][tx] = 0;
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

template<typename T>
__device__ void matrixMulScalar(T* A, T scalar, T* C,
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

template<typename T>
__device__ void matrixElementWiseMulMatrix(T* A, T* B, T* C,
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

template<typename T>
__device__ void matrixDivScalar(T* A, T scalar, T* C,
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

template<typename T>
__device__ void scalarDivMatrix(T scalar, T* A, T* C,
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

template<typename T>
__device__ void matrixElementWiseDivMatrix(T* A, T* B, T* C,
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

template<typename T>
__device__ void matrixTranspose(const T* matrix, T* result, const int rows, const int columns) {
  __shared__ T tile[TILE_DIM][TILE_DIM + 1];

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

template<typename T>
__device__ void matrixRow(T* matrix, T* result, int row,
                          int matrixRows, int matrixColumns) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  result[index] = matrix[row * matrixColumns + index];
}

template<typename T>
__device__ void matrixColumn(T* matrix, T* result, int column,
                             int matrixRows, int matrixColumns) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  result[index] = matrix[index * matrixColumns + column];
}

template<typename T>
__device__ void matrixSum(T* matrix, T* result,
                          int numRows, int numColumns) {

  __shared__ T tile[TILE_DIM][TILE_DIM];

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
    T sum = 0;
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

template<typename T>
__device__ void matrixSumRows(T* matrix, T* result,
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

template<typename T>
__device__ void matrixSumColumns(T* matrix, T* result,
                                 int numRows, int numColumns) {

  __shared__ T tile[TILE_DIM][TILE_DIM];

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

template<typename T>
__device__ void matrixAddRow(T* matrix, T* vector, T* result,
                             int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    T sum = matrix[row * numColumns + col] + vector[col];
    result[row * numColumns + col] = sum;
  }
}

template<typename T>
__device__ void vectorRowAddMatrix(T* vector, T* matrix, T* result,
                                   int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    T sum = matrix[row * numColumns + col] + vector[col];
    result[row * numColumns + col] = sum;
  }
}

template<typename T>
__device__ void matrixAddColumn(T* matrix, T* vector, T* result,
                                int numRows, int numColumns) {

  __shared__ T tile[TILE_DIM];

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
    T sum = matrix[row * numColumns + col] + tile[ty];
    result[row * numColumns + col] = sum;
  }
}

template<typename T>
__device__ void vectorColumnAddMatrix(T* vector, T* matrix, T* result,
                                      int numRows, int numColumns) {

  __shared__ T tile[TILE_DIM];

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
    T sum = tile[ty] + matrix[row * numColumns + col];
    result[row * numColumns + col] = sum;
  }
}

template<typename T>
__device__ void matrixSubRow(T* matrix, T* vector, T* result,
                             int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    T sum = matrix[row * numColumns + col] - vector[col];
    result[row * numColumns + col] = sum;
  }
}

template<typename T>
__device__ void vectorRowSubMatrix(T* vector, T* matrix, T* result,
                                   int numRows, int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (row < numRows && col < numColumns) {
    T sum = vector[col] - matrix[row * numColumns + col];
    result[row * numColumns + col] = sum;
  }
}

template<typename T>
__device__ void matrixSubColumn(T* matrix, T* vector, T* result,
                                int numRows, int numColumns) {

  __shared__ T tile[TILE_DIM];

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
    T sum = matrix[row * numColumns + col] - tile[ty];
    result[row * numColumns + col] = sum;
  }
}

template<typename T>
__device__ void vectorColumnSubMatrix(T* vector, T* matrix, T* result,
                                      int numRows, int numColumns) {

  __shared__ T tile[TILE_DIM];

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
    T sum = tile[ty] - matrix[row * numColumns + col];
    result[row * numColumns + col] = sum;
  }
}
