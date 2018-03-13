#define TILE_DIM 64
#define BLOCK_ROWS 4

extern "C"
__global__ void copy(float *odata, const float *idata) {
  int x = blockIdx.x * TILE_DIM + threadIdx.x;
  int y = blockIdx.y * TILE_DIM + threadIdx.y;
  int width = gridDim.x * TILE_DIM;

  for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
    odata[(y + j) * width + x] = idata[(y + j) * width + x];
}

extern "C"
__global__ void transposeNaive(float *odata, const float *idata) {
  int x = blockIdx.x * TILE_DIM + threadIdx.x;
  int y = blockIdx.y * TILE_DIM + threadIdx.y;
  int width = gridDim.x * TILE_DIM;

  for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
    odata[x * width + (y + j)] = idata[(y + j) * width + x];
}

extern "C"
__global__ void transposeCoalesced(float *odata, const float *idata)
{
  __shared__ float tile[TILE_DIM][TILE_DIM];

  int x = blockIdx.x * TILE_DIM + threadIdx.x;
  int y = blockIdx.y * TILE_DIM + threadIdx.y;
  int width = gridDim.x * TILE_DIM;

  for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
     tile[threadIdx.y + j][threadIdx.x] = idata[(y + j) * width + x];

  __syncthreads();

  x = blockIdx.y * TILE_DIM + threadIdx.x;  // transpose block offset
  y = blockIdx.x * TILE_DIM + threadIdx.y;

  for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
     odata[(y + j) * width + x] = tile[threadIdx.x][threadIdx.y + j];
}

extern "C"
__global__ void copySharedMem(float *odata, const float *idata)
{
  __shared__ float tile[TILE_DIM * TILE_DIM];

  int x = blockIdx.x * TILE_DIM + threadIdx.x;
  int y = blockIdx.y * TILE_DIM + threadIdx.y;
  int width = gridDim.x * TILE_DIM;

  for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
     tile[(threadIdx.y + j) * TILE_DIM + threadIdx.x] = idata[(y + j) * width + x];

  __syncthreads();

  for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
     odata[(y + j) * width + x] = tile[(threadIdx.y + j) * TILE_DIM + threadIdx.x];
}

extern "C"
__global__ void transposeNoBankConflicts(float *odata, const float *idata)
{
  __shared__ float tile[TILE_DIM][TILE_DIM+1];

  int x = blockIdx.x * TILE_DIM + threadIdx.x;
  int y = blockIdx.y * TILE_DIM + threadIdx.y;
  int width = gridDim.x * TILE_DIM;

  for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
     tile[threadIdx.y + j][threadIdx.x] = idata[(y + j) * width + x];

  __syncthreads();

  x = blockIdx.y * TILE_DIM + threadIdx.x;  // transpose block offset
  y = blockIdx.x * TILE_DIM + threadIdx.y;

  for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
     odata[(y + j) * width + x] = tile[threadIdx.x][threadIdx.y + j];
}

extern "C"
__global__ void transpose(const float* matrix, float* result, const int rows, const int columns) {
  __shared__ float tile[TILE_DIM][TILE_DIM + 1];

  int x = blockIdx.x * TILE_DIM + threadIdx.x;
  int y = blockIdx.y * TILE_DIM + threadIdx.y;
  int width = gridDim.x * TILE_DIM;

  for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
     tile[threadIdx.y + j][threadIdx.x] = matrix[(y + j) * width + x];

  __syncthreads();

  x = blockIdx.y * TILE_DIM + threadIdx.x;  // transpose block offset
  y = blockIdx.x * TILE_DIM + threadIdx.y;

  for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
     result[(y + j) * width + x] = tile[threadIdx.x][threadIdx.y + j];
}

extern "C"
__global__ void transposeDouble(const double* matrix, double* result, const int rows, const int columns) {
  __shared__ double tile[TILE_DIM][TILE_DIM + 1];

  int x = blockIdx.x * TILE_DIM + threadIdx.x;
  int y = blockIdx.y * TILE_DIM + threadIdx.y;
  int width = gridDim.x * TILE_DIM;

  for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
     tile[threadIdx.y + j][threadIdx.x] = matrix[(y + j) * width + x];

  __syncthreads();

  x = blockIdx.y * TILE_DIM + threadIdx.x;  // transpose block offset
  y = blockIdx.x * TILE_DIM + threadIdx.y;

  for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
     result[(y + j) * width + x] = tile[threadIdx.x][threadIdx.y + j];
}


template<typename T>
__device__ void transposeTemplate(const T* matrix, T* result, const int rows, const int columns) {
  __shared__ T tile[TILE_DIM][TILE_DIM + 1];

  int x = blockIdx.x * TILE_DIM + threadIdx.x;
  int y = blockIdx.y * TILE_DIM + threadIdx.y;
  int width = gridDim.x * TILE_DIM;

  for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
     tile[threadIdx.y + j][threadIdx.x] = matrix[(y + j) * width + x];

  __syncthreads();

  x = blockIdx.y * TILE_DIM + threadIdx.x;  // transpose block offset
  y = blockIdx.x * TILE_DIM + threadIdx.y;

  for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
     result[(y + j) * width + x] = tile[threadIdx.x][threadIdx.y + j];
}

extern "C"
__global__ void transposeTemplateFloat(const float* matrix, float* result, const int rows, const int columns) {
  transposeTemplate<float>(matrix, result, rows, columns);
}

extern "C"
__global__ void transposeTemplateDouble(const double* matrix, double* result, const int rows, const int columns) {
  transposeTemplate<double>(matrix, result, rows, columns);
}

//extern "C"
//template <> __global__ void transposeTemplate(const float* matrix, const float* result, const int rows, const int columns);

//extern "C"
//template <> __global__ void transposeTemplate<double>(double*, double*, int, int);

extern "C"
__global__ void transposeExperimental(const float* matrix, float* result, const int rows, const int columns) {
  __shared__ float tile[TILE_DIM][TILE_DIM + 1];

  int x = blockIdx.x * TILE_DIM + threadIdx.x;
  int y = blockIdx.y * TILE_DIM + threadIdx.y;

  if (x < columns) {
    #pragma unroll
    for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS) {
      if (y + j < rows) {
        tile[threadIdx.y + j][threadIdx.x] = matrix[(y + j) * columns + x];
//      } else {
//        tile[threadIdx.y + j][threadIdx.x] = 0;
      }
    }
//  } else {
//    #pragma unroll
//    for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS)
//      tile[threadIdx.y + j][threadIdx.x] = 0;
  }

  __syncthreads();

  x = blockIdx.y * TILE_DIM + threadIdx.x;  // transpose block offset
  y = blockIdx.x * TILE_DIM + threadIdx.y;

  if (x < rows) {
    #pragma unroll
    for (int j = 0; j < TILE_DIM; j += BLOCK_ROWS) {
      if (y + j < columns) {
        result[(y + j) * columns + x] = tile[threadIdx.x][threadIdx.y + j];
      }
    }
  }
}