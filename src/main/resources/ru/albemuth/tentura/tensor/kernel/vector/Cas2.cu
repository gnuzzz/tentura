#include "../comparator.h"
#include "../operator.h"

template<typename T>
__device__ void cas_scalar_threshold_scalar_operand1_scalar_operand2(const T* vector, const int comparator, const T threshold, const int op1, const T operand1, const int op2, const T operand2, T* result, const int length) {
  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    T value = vector[index];

    if (compare(value, comparator, threshold)) {
      result[index] = operation(value, op1, operand1);
    } else {
      result[index] = operation(value, op2, operand2);
    }
  }
}

template<typename T>
__device__ void cas_scalar_threshold_vector_operand1_vector_operand_2(const T* vector, const int comparator, const T threshold, const int op1, const T* operand1, const int op2, const T* operand2, T* result, const int length) {
  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    T value = vector[index];

    if (compare(value, comparator, threshold)) {
      T operandValue = operand1[index];
      result[index] = operation(value, op1, operandValue);
    } else {
      T operandValue = operand2[index];
      result[index] = operation(value, op2, operandValue);
    }
  }
}

template<typename T>
__device__ void cas_vector_threshold_scalar_operand1_scalar_operand2(const T* vector, const int comparator, const T* threshold, const int op1, const T operand1, const int op2, const T operand2, T* result, const int length) {
  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    T value = vector[index];
    T thresholdValue = threshold[index];

    if (compare(value, comparator, thresholdValue)) {
      result[index] = operation(value, op1, operand1);
    } else {
      result[index] = operation(value, op2, operand2);
    }
  }
}

template<typename T>
__device__ void cas_vector_threshold_vector_operand1_vector_operand2(const T* vector, const int comparator, const T* threshold, const int op1, const T* operand1, const int op2, const T* operand2, T* result, const int length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < length) {
    T value = vector[index];
    T thresholdValue = threshold[index];

    if (compare(value, comparator, thresholdValue)) {
      T operandValue = operand1[index];
      result[index] = operation(value, op1, operandValue);
    } else {
      T operandValue = operand2[index];
      result[index] = operation(value, op2, operandValue);
    }
  }

}
