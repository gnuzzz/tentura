//template<typename T>
//__device__ void sliceRows(const T* matrix, const int from, const int to, T* result,
//                          const int numRows, const int numColumns) {
//
//  int bx = blockIdx.x;
//  int by = blockIdx.y;
//  int tx = threadIdx.x;
//  int ty = threadIdx.y;
//
//  int row = by * blockDim.y + ty;
//  int col = bx * blockDim.x + tx;
//
//  int resultNumRows = to - from;
//  if (row < resultNumRows && col < numColumns) {
//    int ij = row * numColumns + col;
//    result[ij] = matrix[row * numColumns + from + col];
//  }
//}

template<typename T>
__device__ void sliceColumns(const T* matrix, const int from, const int to, T* result,
                             const int numRows, const int numColumns) {

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;

  int resultNumColumns = to - from;
  if (row < numRows && col < resultNumColumns) {
    int ij = row * resultNumColumns + col;
    result[ij] = matrix[row * numColumns + from + col];
  }
}