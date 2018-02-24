extern "C"
#define TILE_WIDTH 16

// Compute C = A * B
__global__ void matrixMultiplyShared(float *A, float *B, float *C,
                                     int numARows, int numAColumns,
                                     int numBRows, int numBColumns,
                                     int numCRows, int numCColumns) {
  __shared__ float ds_A[TILE_WIDTH][TILE_WIDTH];
  __shared__ float ds_B[TILE_WIDTH][TILE_WIDTH];

  int bx = blockIdx.x;
  int by = blockIdx.y;
  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int row = by * blockDim.y + ty;
  int col = bx * blockDim.x + tx;
  float cValue = 0;

  for (int t = 0; t < (numAColumns - 1) / TILE_WIDTH + 1; t++) {
    if (row < numARows && t * TILE_WIDTH + tx < numAColumns) {
      ds_A[ty][tx] = A[row * numAColumns + t * TILE_WIDTH + tx];
    } else {
      ds_A[ty][tx] = 0.0;
    }
    if (t * TILE_WIDTH + ty < numBRows && col < numBColumns) {
      ds_B[ty][tx] = B[(t * TILE_WIDTH + ty) * numBColumns + col];
    } else {
      ds_B[ty][tx] = 0.0;
    }
    __syncthreads();

    for (int i = 0; i < TILE_WIDTH; i++) {
      cValue += ds_A[ty][i] * ds_B[i][tx];
    }
    __syncthreads();
  }

  if (row < numCRows && col < numCColumns) {
    C[row * numCColumns + col] = cValue;
  }
}