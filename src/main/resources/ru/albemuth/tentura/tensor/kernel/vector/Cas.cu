#include "../comparator.h"
#include "../operator.h"

template<typename T>
__device__ void cas_scalar_threshold_scalar_operand(const T* vector, const int comparator, const T threshold, const int op, const T operand, T* result, const int length) {
  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    T value = vector[index];

    if (compare(value, comparator, threshold)) {
      result[index] = operation(value, op, operand);
    } else {
      result[index] = value;
    }
  }
}

template<typename T>
__device__ void cas_scalar_threshold_vector_operand(const T* vector, const int comparator, const T threshold, const int op, const T* operand, T* result, const int length) {
  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    T value = vector[index];

    if (compare(value, comparator, threshold)) {
      T operandValue = operand[index];
      result[index] = operation(value, op, operandValue);
    } else {
      result[index] = value;
    }
  }
}

template<typename T>
__device__ void cas_vector_threshold_scalar_operand(const T* vector, const int comparator, const T* threshold, const int op, const T operand, T* result, const int length) {
  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    T value = vector[index];
    T thresholdValue = threshold[index];

    if (compare(value, comparator, thresholdValue)) {
      result[index] = operation(value, op, operand);
    } else {
      result[index] = value;
    }
  }
}

template<typename T>
__device__ void cas_vector_threshold_vector_operand(const T* vector, const int comparator, const T* threshold, const int op, const T* operand, T* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    T value = vector[index];
    T thresholdValue = threshold[index];

    if (compare(value, comparator, thresholdValue)) {
      T operandValue = operand[index];
      result[index] = operation(value, op, operandValue);
    } else {
      result[index] = value;
    }
  }

}
