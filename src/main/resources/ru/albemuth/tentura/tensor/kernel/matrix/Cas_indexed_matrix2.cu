#include "../comparator.h"
#include "../operator.h"

template<typename T>
__device__ void cas_matrix_threshold_indexed_operand1_indexed_operand2(const T* matrix, const int comparator, const T* threshold, const int op1, const T* operand1, const int op2, const T* operand2, const int* rows, const int* cols, T* result, const int rowsCount, const int colsCount, const int indices_length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < indices_length) {
    int row = rows[index];
    int col = cols[index];
    int ij = row * colsCount + col;
    T value = matrix[ij];
    T thresholdValue = threshold[ij];

    T resultValue;
    if (compare(value, comparator, thresholdValue)) {
      T operandValue = operand1[index];
      resultValue = operation(value, op1, operandValue);
    } else {
      T operandValue = operand2[index];
      resultValue = operation(value, op2, operandValue);
    }
    result[ij] = resultValue;
  }

}

template<typename T>
__device__ void cas_indexed_threshold_matrix_operand1_matrix_operand2(const T* matrix, const int comparator, const T* threshold, const int op1, const T* operand1, const int op2, const T* operand2, const int* rows, const int* cols, T* result, const int rowsCount, const int colsCount, const int indices_length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < indices_length) {
    int row = rows[index];
    int col = cols[index];
    int ij = row * colsCount + col;
    T value = matrix[ij];
    T thresholdValue = threshold[index];

    T resultValue;
    if (compare(value, comparator, thresholdValue)) {
      T operandValue = operand1[ij];
      resultValue = operation(value, op1, operandValue);
    } else {
      T operandValue = operand2[ij];
      resultValue = operation(value, op2, operandValue);
    }
    result[ij] = resultValue;
  }

}
