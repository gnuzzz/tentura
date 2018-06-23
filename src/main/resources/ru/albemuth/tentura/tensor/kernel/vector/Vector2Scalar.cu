template<typename T>
__device__ void vectorAddScalar(const T* A, const T scalar, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] + scalar;
  }
}

template<typename T>
__device__ void vectorSubScalar(const T* A, const T scalar, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] - scalar;
  }
}

template<typename T>
__device__ void scalarSubVector(const T* A, const T scalar, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = scalar - A[index];
  }
}

template<typename T>
__device__ void vectorTimesScalar(const T* A, const T scalar, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] * scalar;
  }
}

template<typename T>
__device__ void vectorDivScalar(const T* A, const T scalar, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = A[index] / scalar;
  }
}

template<typename T>
__device__ void scalarDivVector(const T* A, const T scalar, T* C, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    C[index] = scalar / A[index];
  }
}