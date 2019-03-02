template<typename T>
__device__ void setRowsValue(T* matrix, const int* indices, const T value,
                             const int rows, const int cols) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int col = bx * blockDim.x + tx;

  if (col < cols) {
    int row = indices[col];
    matrix[row * cols + col] = value;
  }

}