template<typename T>
__device__ void abs(const T* data, T* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = (T)abs((float)data[index]);
  }
}

extern "C"
__global__ void abs_Boolean(const unsigned char* data, unsigned char* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = data[index];
  }
}

extern "C"
__global__ void abs_Char(const unsigned short* data, unsigned short* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = data[index];
  }
}

extern "C"
__global__ void abs_Double(const double* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = abs(data[index]);
  }
}

template<typename T>
__device__ void acosf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = acos((float)data[index]);
  }
}

template<typename T>
__device__ void acosd(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = acos((double)data[index]);
  }
}

template<typename T>
__device__ void acoshf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = acosh((float)data[index]);
  }
}

template<typename T>
__device__ void acoshd(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = acosh((double)data[index]);
  }
}

template<typename T>
__device__ void asinf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = asin((float)data[index]);
  }
}

template<typename T>
__device__ void asind(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = asin((double)data[index]);
  }
}

template<typename T>
__device__ void asinhf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = asinh((float)data[index]);
  }
}

template<typename T>
__device__ void asinhd(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = asinh((double)data[index]);
  }
}

template<typename T>
__device__ void atanf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = atan((float)data[index]);
  }
}

template<typename T>
__device__ void atand(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = atan((double)data[index]);
  }
}

template<typename T>
__device__ void atanhf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = atanh((float)data[index]);
  }
}

template<typename T>
__device__ void atanhd(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = atanh((double)data[index]);
  }
}

template<typename T>
__device__ void cbrtf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = cbrt((float)data[index]);
  }
}

template<typename T>
__device__ void cbrtd(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = cbrt((double)data[index]);
  }
}

template<typename T>
__device__ void ceilf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = ceil((float)data[index]);
  }
}

template<typename T>
__device__ void ceild(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = ceil((double)data[index]);
  }
}

template<typename T>
__device__ void cosf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = cos((float)data[index]);
  }
}

template<typename T>
__device__ void cosd(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = cos((double)data[index]);
  }
}

template<typename T>
__device__ void coshf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = cosh((float)data[index]);
  }
}

template<typename T>
__device__ void coshd(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = cosh((double)data[index]);
  }
}

template<typename T>
__device__ void expf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = exp((float)data[index]);
  }
}

template<typename T>
__device__ void expd(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = exp((double)data[index]);
  }
}

template<typename T>
__device__ void exp10f(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = exp10((float)data[index]);
  }
}

template<typename T>
__device__ void exp10d(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = exp10((double)data[index]);
  }
}

template<typename T>
__device__ void exp2f(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = exp2((float)data[index]);
  }
}

template<typename T>
__device__ void exp2d(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = exp2((double)data[index]);
  }
}

template<typename T>
__device__ void floorf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = floor((float)data[index]);
  }
}

template<typename T>
__device__ void floord(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = floor((double)data[index]);
  }
}

template<typename T>
__device__ void lnf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = log((float)data[index]);
  }
}

template<typename T>
__device__ void lnd(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = log((double)data[index]);
  }
}

template<typename T>
__device__ void logf(const T* data, const float base, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = log((float)data[index]) / log(base);
  }
}

template<typename T>
__device__ void logd(const T* data, const double base, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = log((float)data[index]) / log(base);
  }
}

template<typename T>
__device__ void log10f(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = log10((float)data[index]);
  }
}

template<typename T>
__device__ void log10d(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = log10((double)data[index]);
  }
}

template<typename T>
__device__ void log2f(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = log2((float)data[index]);
  }
}

template<typename T>
__device__ void log2d(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = log2((double)data[index]);
  }
}

template<typename T>
__device__ void max(const T* data1, const T* data2, T* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    T value1 = data1[index];
    T value2 = data2[index];
    T diff = value1 - value2;
    result[index] = diff > 0 ? value1 : value2;
  }
}

extern "C"
__global__ void max_Float(const float* data1, const float* data2, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = max(data1[index], data2[index]);
  }
}

extern "C"
__global__ void max_Double(const double* data1, const double* data2, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = max(data1[index], data2[index]);
  }
}

template<typename T>
__device__ void min(const T* data1, const T* data2, T* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    T value1 = data1[index];
    T value2 = data2[index];
    T diff = value1 - value2;
    result[index] = diff < 0 ? value1 : value2;
  }
}

extern "C"
__global__ void min_Float(const float* data1, const float* data2, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = min(data1[index], data2[index]);
  }
}

extern "C"
__global__ void min_Double(const double* data1, const double* data2, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = min(data1[index], data2[index]);
  }
}

template<typename T>
__device__ void powf(const T* data, const float power, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = pow((float)data[index], power);
  }
}

template<typename T>
__device__ void powd(T* data, const double power, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = pow((double)data[index], power);
  }
}

template<typename T>
__device__ void pow2f(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    float value = data[index];
    result[index] = value * value;
  }
}

template<typename T>
__device__ void pow2d(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    double value = data[index];
    result[index] = value * value;
  }
}

template<typename T>
__device__ void reluf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    float value = data[index];
    result[index] = value >= 0 ? value : 0;
  }
}

template<typename T>
__device__ void relud(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    double value = data[index];
    result[index] = value >= 0 ? value : 0;
  }
}

extern "C"
__global__ void round_f(const float* data, int* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = round(data[index]);
  }
}

extern "C"
__global__ void round_d(const double* data, long long int* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = round(data[index]);
  }
}

template<typename T>
__device__ void sigmoidf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = 1.0f / (1.0f + exp(-(float)data[index]));
  }
}

template<typename T>
__device__ void sigmoidd(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = 1.0f / (1.0f + exp(-(double)data[index]));
  }
}

template<typename T>
__device__ void sign(const T* data, int* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    T value = data[index];
    result[index] = value == 0 ? 0 : (value > 0 ? 1 : -1);
  }
}

template<typename T>
__device__ void sinf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = sin((float)data[index]);
  }
}

template<typename T>
__device__ void sind(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = sin((double)data[index]);
  }
}

template<typename T>
__device__ void sinhf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = sinh((float)data[index]);
  }
}

template<typename T>
__device__ void sinhd(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = sinh((double)data[index]);
  }
}

template<typename T>
__device__ void sqrtf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = sqrt((float)data[index]);
  }
}

template<typename T>
__device__ void sqrtd(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = sqrt((double)data[index]);
  }
}

template<typename T>
__device__ void tanf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = tan((float)data[index]);
  }
}

template<typename T>
__device__ void tand(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = tan((double)data[index]);
  }
}

template<typename T>
__device__ void tanhf(const T* data, float* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = tanh((float)data[index]);
  }
}

template<typename T>
__device__ void tanhd(const T* data, double* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;
  int index = bx * blockDim.x + tx;

  if (index < length) {
    result[index] = tanh((double)data[index]);
  }
}
