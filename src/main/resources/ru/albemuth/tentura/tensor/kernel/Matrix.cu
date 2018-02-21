#define TILE_DIM 32
#define BLOCK_ROWS 4

template<typename T>
__device__ void matrixAddMatrix(const T* A, const T* B, T* C,
                                const int rows, const int columns) {

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
__device__ void matrixAddScalar(const T* A, const T scalar, T* C,
                                const int numRows, const int numColumns) {

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
__device__ void matrixSubMatrix(const T* A, const T* B, T* C,
                                const int numRows, const int numColumns) {

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
__device__ void matrixSubScalar(const T* A, const T scalar, T* C,
                                const int numRows, const int numColumns) {

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
__device__ void scalarSubMatrix(const T scalar, const T* A, T* C,
                                const int numRows, const int numColumns) {

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
__device__ void matrixMulMatrix(const T* A, const T* B, T* C,
                                const int numARows, const int numAColumns, const int numBRows, const int numBColumns) {

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

  if (row < numARows && col < numBColumns) {
    C[row * numBColumns + col] = cValue;
  }
}

template<typename T>
__device__ void matrixMulScalar(const T* A, const T scalar, T* C,
                                const int numRows, const int numColumns) {

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
__device__ void matrixElementWiseMulMatrix(const T* A, const T* B, T* C,
                                           const int numRows, const int numColumns) {

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
__device__ void matrixDivScalar(const T* A, const T scalar, T* C,
                                const int numRows, const int numColumns) {

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
__device__ void scalarDivMatrix(const T scalar, const T* A, T* C,
                                const int numRows, const int numColumns) {

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
__device__ void matrixElementWiseDivMatrix(const T* A, const T* B, T* C,
                                           const int numRows, const int numColumns) {

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
__device__ void matrixTranspose(const T* matrix, T* result,
                                const int rows, const int columns) {
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
__device__ void matrixRow(const T* matrix, const int row, T* result,
                          const int matrixRows, const int matrixColumns) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  result[index] = matrix[row * matrixColumns + index];
}

template<typename T>
__device__ void matrixColumn(const T* matrix, const int column, T* result,
                             const int matrixRows, const int matrixColumns) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  result[index] = matrix[index * matrixColumns + column];
}

template<typename T>
__device__ void matrixSum(const T* matrix, T* result,
                          const int numRows, const int numColumns) {

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
__device__ void matrixSumRows(const T* matrix, T* result,
                              const int numRows, const int numColumns) {

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
__device__ void matrixSumColumns(const T* matrix, T* result,
                                 const int numRows, const int numColumns) {

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
__device__ void matrixAddRow(const T* matrix, const T* vector, T* result,
                             const int numRows, const int numColumns) {

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
__device__ void vectorRowAddMatrix(const T* vector, const T* matrix, T* result,
                                   const int numRows, const int numColumns) {

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
__device__ void matrixAddColumn(const T* matrix, const T* vector, T* result,
                                const int numRows, const int numColumns) {

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
__device__ void vectorColumnAddMatrix(const T* vector, const T* matrix, T* result,
                                      const int numRows, const int numColumns) {

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
__device__ void matrixSubRow(const T* matrix, const T* vector, T* result,
                             const int numRows, const int numColumns) {

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
__device__ void vectorRowSubMatrix(const T* vector, const T* matrix, T* result,
                                   const int numRows, const int numColumns) {

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
__device__ void matrixSubColumn(const T* matrix, const T* vector, T* result,
                                const int numRows, const int numColumns) {

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
__device__ void vectorColumnSubMatrix(const T* vector, const T* matrix, T* result,
                                      const int numRows, const int numColumns) {

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
