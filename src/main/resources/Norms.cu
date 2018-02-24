extern "C"

#define TILE_LENGTH 128

__global__ void l2(float *v1, float *v2, int n, float *result) {

  __shared__ float ds_R[TILE_LENGTH];

  //int tx = blockIdx.y * blockDim.y + threadIdx.y;
  int tx = blockIdx.x * blockDim.x + threadIdx.x;

  float ret = 0.0f;
  for (int t = 0; t < (n - 1) / TILE_LENGTH + 1; t++) {
    if (t * TILE_LENGTH + tx < n) {
      float f1 = v1[t * TILE_LENGTH + tx];
      float f2 = v2[t * TILE_LENGTH + tx];
      ds_R[tx] = (f1 - f2) * (f1 - f2);
    } else {
      ds_R[tx] = 0;
    }
    __syncthreads();

    if (tx == 0) {
      for (int i = 0; i < TILE_LENGTH; i++) {
        ret += ds_R[i];
      }
    }
    __syncthreads();
  }

  if (tx == 0) {
    result[0] = sqrt(ret);
  }
}