#include "../comparator.h"
#include "../operator.h"

template<typename T>
__device__ void cas_scalar_threshold_scalar_operand1_scalar_operand2(const T* matrix, const int comparator, const T threshold, const int op1, const T operand1, const int op2, const T operand2, T* result, const int rows, const int cols) {

  int gridHeight = gridDim.y;
  int tileDim = rows / gridHeight + (rows % gridHeight == 0 ? 0 : 1);
  int blockRows = blockDim.y;

  int row = blockIdx.y * tileDim + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;

  if (col < cols) {
    #pragma unroll
    for (int i = 0; i < tileDim && row + i < rows; i += blockRows) {
      int ij = (row + i) * cols + col;
      T value = matrix[ij];

      T resultValue;
      if (compare(value, comparator, threshold)) {
        resultValue = operation(value, op1, operand1);
      } else {
        resultValue = operation(value, op2, operand2);
      }
      result[ij] = resultValue;
    }
  }

}

template<typename T>
__device__ void cas_scalar_threshold_row_operand1_row_operand2(const T* matrix, const int comparator, const T threshold, const int op1, const T* operand1, const int op2, const T* operand2, T* result, const int rows, const int cols) {

  int gridHeight = gridDim.y;
  int tileDim = rows / gridHeight + (rows % gridHeight == 0 ? 0 : 1);
  int blockRows = blockDim.y;

  int row = blockIdx.y * tileDim + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;

  if (col < cols) {
    T operandValue1 = operand1[col];
    T operandValue2 = operand2[col];
    #pragma unroll
    for (int i = 0; i < tileDim && row + i < rows; i += blockRows) {
      int ij = (row + i) * cols + col;
      T value = matrix[ij];

      T resultValue;
      if (compare(value, comparator, threshold)) {
        resultValue = operation(value, op1, operandValue1);
      } else {
        resultValue = operation(value, op2, operandValue2);
      }
      result[ij] = resultValue;
    }
  }
}

template<typename T>
__device__ void cas_scalar_threshold_column_operand1_column_operand2(const T* matrix, const int comparator, const T threshold, const int op1, const T* operand1, const int op2, const T* operand2, T* result, const int rows, const int cols) {

  int gridHeight = gridDim.y;
  int tileDim = rows / gridHeight + (rows % gridHeight == 0 ? 0 : 1);
  int blockRows = blockDim.y;

  int row = blockIdx.y * tileDim + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;

  if (col < cols) {
    #pragma unroll
    for (int i = 0; i < tileDim && row + i < rows; i += blockRows) {
      int ij = (row + i) * cols + col;
      T value = matrix[ij];

      T resultValue;
      if (compare(value, comparator, threshold)) {
        T operandValue = operand1[row + i];
        resultValue = operation(value, op1, operandValue);
      } else {
        T operandValue = operand2[row + i];
        resultValue = operation(value, op2, operandValue);
      }
      result[ij] = resultValue;
    }
  }

}

template<typename T>
__device__ void cas_scalar_threshold_matrix_operand1_matrix_operand2(const T* matrix, const int comparator, const T threshold, const int op1, const T* operand1, const int op2, const T* operand2, T* result, const int rows, const int cols) {

  int gridHeight = gridDim.y;
  int tileDim = rows / gridHeight + (rows % gridHeight == 0 ? 0 : 1);
  int blockRows = blockDim.y;

  int row = blockIdx.y * tileDim + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;

  if (col < cols) {
    #pragma unroll
    for (int i = 0; i < tileDim && row + i < rows; i += blockRows) {
      int ij = (row + i) * cols + col;
      T value = matrix[ij];

      T resultValue;
      if (compare(value, comparator, threshold)) {
        T operandValue = operand1[ij];
        resultValue = operation(value, op1, operandValue);
      } else {
        T operandValue = operand2[ij];
        resultValue = operation(value, op2, operandValue);
      }
      result[ij] = resultValue;
    }
  }

}
