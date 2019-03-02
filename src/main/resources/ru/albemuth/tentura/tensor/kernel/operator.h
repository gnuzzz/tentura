#ifndef OPERATOR_H
#define OPERATOR_H

#define PLUS              0
#define MINUS             1
#define TIMES             2
#define DIV               3

template<typename T>
__device__ __forceinline__ T operation(const T value1, const int _operator, const T value2) {
  T result;
  switch (_operator) {
    case PLUS:
      result = value1 + value2;
      break;
    case MINUS:
      result = value1 - value2;
      break;
    case TIMES:
      result = value1 * value2;
      break;
    case DIV:
      result = value1 / value2;
      break;
    default:
      result = value2;
      break;
  }
  return result;
}

#endif //OPERATOR_H
