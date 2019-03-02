#include "../comparator.h"
#include "../operator.h"

template<typename T>
__device__ void cas_indexed_threshold_indexed_operand(const T* matrix, const int comparator, const T* threshold, const int op, const T* operand, const int* rows, const int* cols, T* result, const int rowsCount, const int colsCount, const int indices_length) {

  int bx = blockIdx.x;
  int tx = threadIdx.x;

  int index = bx * blockDim.x + tx;

  if (index < indices_length) {
    int row = rows[index];
    int col = cols[index];
    int ij = row * colsCount + col;
    T value = matrix[ij];
    T thresholdValue = threshold[index];
    T operandValue = operand[index];

    T resultValue;
    if (compare(value, comparator, thresholdValue)) {
      resultValue = operation(value, op, operandValue);
    } else {
      resultValue = value;
    }
    result[ij] = resultValue;
  }

}
