template<typename T>
__device__ void vectorAddVector(const T* A, const T* B, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] + B[index];
  }
}

template<typename T>
__device__ void vectorSubVector(const T* A, const T* B, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] - B[index];
  }
}

template<typename T>
__device__ void vectorTimesVector(const T* A, const T* B, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] * B[index];
  }
}

template<typename T>
__device__ void vectorDivVector(const T* A, const T* B, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] / B[index];
  }
}