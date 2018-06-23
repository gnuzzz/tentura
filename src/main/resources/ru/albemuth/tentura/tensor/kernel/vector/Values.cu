template<typename T>
__device__ void value(const T* vector, const int index, T* result, const int length) {
  result[0] = vector[index];
}

template<typename T>
__device__ void values(const T* vector, const int* indices, T* result, const int length, const int indicesLength) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < indicesLength) {
    result[index] = vector[indices[index]];
  }
}

template<typename T>
__device__ void slice(const T* vector, const int from, const int to, T* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;
  int vectorIndex = from + index;

  if (vectorIndex < to + index) {
    result[index] = vector[vectorIndex];
  }
}

template<typename T>
__device__ void vectorIndices(const T* vector, int* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = index;
  }
}

template<typename T>
__device__ void concat(const T* vector1, const T* vector2, T* result, const int length1, const int length2) {

}