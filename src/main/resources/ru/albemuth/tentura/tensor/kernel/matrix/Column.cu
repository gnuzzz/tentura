#define TILE_DIM 32

template<typename T>
__device__ void getColumn(const T* matrix, const int col, T* result,
                          const int rows, const int cols) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  result[index] = matrix[index * cols + col];
}

template<typename T>
__device__ void updateColumn(T* matrix, const int col, const T* column,
                             const int rows, const int cols) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  matrix[index * cols + col] = column[index];
}

template<typename T>
__device__ void matrixAddColumn(const T* matrix, const T* column, T* result,
                                const int rows, const int cols) {

  __shared__ T tile[TILE_DIM];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (ty == 0) {
    int r = row + tx;
    if (r < rows) {
      tile[tx] = column[r];
    } else {
      tile[tx] = 0;
    }
  }
  __syncthreads();

  if (row < rows && col < cols) {
    T value = matrix[row * cols + col] + tile[ty];
    result[row * cols + col] = value;
  }
}

template<typename T>
__device__ void columnAddMatrix(const T* column, const T* matrix, T* result,
                                const int rows, const int cols) {

  __shared__ T tile[TILE_DIM];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (ty == 0) {
    int r = row + tx;
    if (r < rows) {
      tile[tx] = column[r];
    } else {
      tile[tx] = 0;
    }
  }
  __syncthreads();

  if (row < rows && col < cols) {
    T value = tile[ty] + matrix[row * cols + col];
    result[row * cols + col] = value;
  }
}

template<typename T>
__device__ void matrixSubColumn(const T* matrix, const T* column, T* result,
                                const int rows, const int cols) {

  __shared__ T tile[TILE_DIM];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (ty == 0) {
    int r = row + tx;
    if (r < rows) {
      tile[tx] = column[r];
    } else {
      tile[tx] = 0;
    }
  }
  __syncthreads();

  if (row < rows && col < cols) {
    T value = matrix[row * cols + col] - tile[ty];
    result[row * cols + col] = value;
  }
}

template<typename T>
__device__ void columnSubMatrix(const T* column, const T* matrix, T* result,
                                const int rows, const int cols) {

  __shared__ T tile[TILE_DIM];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (ty == 0) {
    int r = row + tx;
    if (r < rows) {
      tile[tx] = column[r];
    } else {
      tile[tx] = 0;
    }
  }
  __syncthreads();

  if (row < rows && col < cols) {
    T value = tile[ty] - matrix[row * cols + col];
    result[row * cols + col] = value;
  }
}

template<typename T>
__device__ void matrixTimesColumn(const T* matrix, const T* column, T* result,
                                  const int rows, const int cols) {

  __shared__ T tile[TILE_DIM];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (ty == 0) {
    int r = row + tx;
    if (r < rows) {
      tile[tx] = column[r];
    } else {
      tile[tx] = 0;
    }
  }
  __syncthreads();

  if (row < rows && col < cols) {
    T value = matrix[row * cols + col] * tile[ty];
    result[row * cols + col] = value;
  }
}

template<typename T>
__device__ void columnTimesMatrix(const T* column, const T* matrix, T* result,
                                  const int rows, const int cols) {

  __shared__ T tile[TILE_DIM];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (ty == 0) {
    int r = row + tx;
    if (r < rows) {
      tile[tx] = column[r];
    } else {
      tile[tx] = 0;
    }
  }
  __syncthreads();

  if (row < rows && col < cols) {
    T value = tile[ty] * matrix[row * cols + col];
    result[row * cols + col] = value;
  }
}

template<typename T>
__device__ void matrixDivColumn(const T* matrix, const T* column, T* result,
                                const int rows, const int cols) {

  __shared__ T tile[TILE_DIM];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (ty == 0) {
    int r = row + tx;
    if (r < rows) {
      tile[tx] = column[r];
    } else {
      tile[tx] = 0;
    }
  }
  __syncthreads();

  if (row < rows && col < cols) {
    T value = matrix[row * cols + col] / tile[ty];
    result[row * cols + col] = value;
  }
}

template<typename T>
__device__ void columnDivMatrix(const T* column, const T* matrix, T* result,
                                const int rows, const int cols) {

  __shared__ T tile[TILE_DIM];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  if (ty == 0) {
    int r = row + tx;
    if (r < rows) {
      tile[tx] = column[r];
    } else {
      tile[tx] = 0;
    }
  }
  __syncthreads();

  if (row < rows && col < cols) {
    T value = tile[ty] / matrix[row * cols + col];
    result[row * cols + col] = value;
  }
}
