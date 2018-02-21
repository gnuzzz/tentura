extern "C"
__global__ void scalarSigmoid(float* scalar, float* result) {
  result[0] = 1.0f / (1.0f + exp(-scalar[0]));
}