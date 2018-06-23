#define TILE_DIM 128

//template<typename T>
//__device__ void sumColumns(const T* matrix, T* result,
//                           const int rows, const int cols) {
//
//  __shared__ T tile[TILE_DIM][TILE_DIM];
//
//  int by = blockIdx.y;
//  int ty = threadIdx.y;
//  int row = by * blockDim.y + ty;
//  T sum = 0;
//
//  #pragma unroll
//  for (int t = 0; t < (cols - 1) / TILE_DIM + 1; t++) {
//    #pragma unroll
//    for (int i = 0; i < TILE_DIM; i++) {
//      int r = by * TILE_DIM + i;
//      int c = t * TILE_DIM + ty;
//      if (r < rows && c < cols) {
//        tile[i][ty] = matrix[r * cols + c];
//      } else {
//        tile[i][ty] = 0;
//      }
//    }
//    __syncthreads();
//
//    #pragma unroll
//    for (int j = 0; j < TILE_DIM; j++) {
//      sum += tile[ty][j];
//    }
//    __syncthreads();
//  }
//
//  if (row < rows) {
//    result[row] = sum;
//  }
//}


template<typename T>
__device__ void sumColumns(const T* matrix, T* result,
                           const int rows, const int cols) {

  __shared__ T tile[TILE_DIM];

  int row = blockIdx.x;

  if (row < rows) {
    int index = threadIdx.x;
    int partLength = (cols + TILE_DIM - 1) / TILE_DIM;

    T sum = 0;
    #pragma unroll
    for (int i = 0; i < partLength; i++) {
      int col = i * TILE_DIM + index;
      if (col < cols) {
        T value = matrix[row * cols + col];
        sum += value;
      }
    }
    tile[index] = sum;

    #pragma unroll
    for (int d = 1; d < TILE_DIM && d < cols; d <<= 1) {
      __syncthreads();
      if (index % (d << 1) == 0) {
        int valueIndex = index + d;
        if (valueIndex < TILE_DIM) {
          T value = tile[valueIndex];
          sum += value;
          tile[index] = sum;
        }
      }
    }

    if (index == 0) {
      result[row] = sum;
    }
  }
}
