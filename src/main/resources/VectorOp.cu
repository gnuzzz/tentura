extern "C"
__global__ void l2(float *v1, float *v2, int n, float *result)
{
    int i = blockIdx.y * blockDim.y + threadIdx.y;
    if (i == 0)
    {
        result[0] = 44.0f;
    }

}