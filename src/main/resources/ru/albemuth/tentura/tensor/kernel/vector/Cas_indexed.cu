#include "../comparator.h"
#include "../operator.h"

template<typename T>
__device__ void cas_scalar_threshold_indexed_operand(const T* vector, const int comparator, const T threshold, const int op, const T* operand, const int* indices, T* result, const int length, const int indices_length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < indices_length) {
    int valueIndex = indices[index];
    T value = vector[valueIndex];
    T operandValue = operand[index];

    T resultValue;
    if (compare(value, comparator, threshold)) {
      resultValue = operation(value, op, operandValue);
    } else {
      resultValue = value;
    }
    result[valueIndex] = resultValue;
  }

}

template<typename T>
__device__ void cas_indexed_threshold_scalar_operand(const T* vector, const int comparator, const T* threshold, const int op, const T operand, const int* indices, T* result, const int length, const int indices_length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < indices_length) {
    int valueIndex = indices[index];
    T value = vector[valueIndex];
    T thresholdValue = threshold[index];

    T resultValue;
    if (compare(value, comparator, thresholdValue)) {
      resultValue = operation(value, op, operand);
    } else {
      resultValue = value;
    }
    result[valueIndex] = resultValue;
  }

}

template<typename T>
__device__ void cas_indexed_threshold_indexed_operand(const T* vector, const int comparator, const T* threshold, const int op, const T* operand, const int* indices, T* result, const int length, const int indices_length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < indices_length) {
    int valueIndex = indices[index];
    T value = vector[valueIndex];
    T thresholdValue = threshold[index];
    T operandValue = operand[index];

    T resultValue;
    if (compare(value, comparator, thresholdValue)) {
      resultValue = operation(value, op, operandValue);
    } else {
      resultValue = value;
    }
    result[valueIndex] = resultValue;
  }

}