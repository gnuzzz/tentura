//#define TILE_DIM 1024
//
//template<typename T>
//__device__ void reverse(const T* vector, T* result, const int length) {
////  __shared__ T tile[TILE_DIM];
////  __shared__ T anti_tile[TILE_DIM];
//  extern __shared__ char m[];
//  T* tile = (T*)m;
//  T* anti_tile = (T*)(m + blockDim.x * sizeof(T));
//
//  int bx = blockIdx.x;
//  int tx = threadIdx.x;
//
//  int index = bx * blockDim.x + tx;
//  int centerIndex = length / 2;
//  int nextBlockIndex = (bx + 1) * blockDim.x;
//  int blockShift = (nextBlockIndex < centerIndex ? 0 : nextBlockIndex - centerIndex);
//  int anti_index = index != centerIndex ? length - (bx + 1) * blockDim.x + tx + blockShift : centerIndex;
//  if(nextBlockIndex < centerIndex || index < centerIndex) {
//    int tileIndex = blockDim.x - 1 - tx - blockShift;
//    tile[tileIndex] = vector[index];
//    anti_tile[tileIndex] = vector[anti_index];
//  }
//  __syncthreads();
//
//  if (nextBlockIndex < centerIndex || index < centerIndex) {
//    result[index] = anti_tile[tx];
//    result[anti_index] = tile[tx];
//  } else if (index == centerIndex && length % 2 != 0) {
//    result[index] = vector[index];
//  }
//}

template<typename T>
__device__ void reverse(const T* vector, T* result, const int length) {
  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;
  int anti_index = length - 1 - index;

  if (anti_index > index) {
    T value = vector[index];
    T anti_value = vector[anti_index];
    result[index] = anti_value;
    result[anti_index] = value;
  } else if (index == anti_index) {
    result[index] = vector[index];
  }
}