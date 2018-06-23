#include "../comparator.h"
#include "../operator.h"

template<typename T>
__device__ void cas_scalar_threshold_indexed_operand1_indexed_operand2(const T* vector, const int comparator, const T threshold, const int op1, const T* operand1, const int op2, const T* operand2, const int* indices, T* result, const int length, const int indices_length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < indices_length) {
    int valueIndex = indices[index];
    T value = vector[valueIndex];

    T resultValue;
    if (compare(value, comparator, threshold)) {
      T operandValue = operand1[index];
      resultValue = operation(value, op1, operandValue);
    } else {
      T operandValue = operand2[index];
      resultValue = operation(value, op2, operandValue);
    }
    result[valueIndex] = resultValue;
  }

}

template<typename T>
__device__ void cas_indexed_threshold_scalar_operand1_scalar_operand2(const T* vector, const int comparator, const T* threshold, const int op1, const T operand1, const int op2, const T operand2, const int* indices, T* result, const int length, const int indices_length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < indices_length) {
    int valueIndex = indices[index];
    T value = vector[valueIndex];
    T thresholdValue = threshold[index];

    T resultValue;
    if (compare(value, comparator, thresholdValue)) {
      resultValue = operation(value, op1, operand1);
    } else {
      resultValue = operation(value, op2, operand2);
    }
    result[valueIndex] = resultValue;
  }

}

template<typename T>
__device__ void cas_indexed_threshold_indexed_operand1_indexed_operand2(const T* vector, const int comparator, const T* threshold, const int op1, const T* operand1, const int op2, const T* operand2, const int* indices, T* result, const int length, const int indices_length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < indices_length) {
    int valueIndex = indices[index];
    T value = vector[valueIndex];
    T thresholdValue = threshold[index];

    T resultValue;
    if (compare(value, comparator, thresholdValue)) {
      T operandValue = operand1[index];
      resultValue = operation(value, op1, operandValue);
    } else {
      T operandValue = operand2[index];
      resultValue = operation(value, op2, operandValue);
    }
    result[valueIndex] = resultValue;
  }

}
