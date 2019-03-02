template<typename T>
__device__ void sumRows(const T* matrix, T* result,
                        const int rows, const int cols) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int col = bx * blockDim.x + tx;
  if (col < cols) {
    T sum = 0;
    #pragma unroll
    for (int i = 0; i < rows; i++) {
      int index = i * cols + col;
      sum += matrix[index];
    }
    result[col] = sum;
  }
}