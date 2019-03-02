#include "../comparator.h"
#include "../operator.h"

template<typename T>
__device__ void cas_matrix_threshold_scalar_operand(const T* matrix, const int comparator, const T* threshold, const int op, const T operand, T* result, const int rows, const int cols) {

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
      T thresholdValue = threshold[ij];

      T resultValue;
      if (compare(value, comparator, thresholdValue)) {
        resultValue = operation(value, op, operand);
      } else {
        resultValue = value;
      }
      result[ij] = resultValue;
    }
  }

}

template<typename T>
__device__ void cas_matrix_threshold_row_operand(const T* matrix, const int comparator, const T* threshold, const int op, const T* operand, T* result, const int rows, const int cols) {

  int gridHeight = gridDim.y;
  int tileDim = rows / gridHeight + (rows % gridHeight == 0 ? 0 : 1);
  int blockRows = blockDim.y;

  int row = blockIdx.y * tileDim + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;

  if (col < cols) {
    T operandValue = operand[col];
    #pragma unroll
    for (int i = 0; i < tileDim && row + i < rows; i += blockRows) {
      int ij = (row + i) * cols + col;
      T value = matrix[ij];
      T thresholdValue = threshold[ij];

      T resultValue;
      if (compare(value, comparator, thresholdValue)) {
        resultValue = operation(value, op, operandValue);
      } else {
        resultValue = value;
      }
      result[ij] = resultValue;
    }
  }

}

template<typename T>
__device__ void cas_matrix_threshold_column_operand(const T* matrix, const int comparator, const T* threshold, const int op, const T* operand, T* result, const int rows, const int cols) {

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
      T thresholdValue = threshold[ij];
      T operandValue = operand[row + i];

      T resultValue;
      if (compare(value, comparator, thresholdValue)) {
        resultValue = operation(value, op, operandValue);
      } else {
        resultValue = value;
      }
      result[ij] = resultValue;
    }
  }

}

template<typename T>
__device__ void cas_matrix_threshold_matrix_operand(const T* matrix, const int comparator, const T* threshold, const int op, const T* operand, T* result, const int rows, const int cols) {

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
      T thresholdValue = threshold[ij];
      T operandValue = operand[ij];

      T resultValue;
      if (compare(value, comparator, thresholdValue)) {
        resultValue = operation(value, op, operandValue);
      } else {
        resultValue = value;
      }
      result[ij] = resultValue;
    }
  }

}
