#ifndef COMPARATOR_H
#define COMPARATOR_H

#define EQUAL             0
#define NOT_EQUAL         1
#define LESS              2
#define LESS_OR_EQUAL     3
#define GREATER           4
#define GREATER_OR_EQUAL  5

template<typename T>
__device__ __forceinline__ bool compare(const T value1, const int comparator, const T value2) {
  T diff = value1 - value2;
  bool result;
  switch (comparator) {
    case EQUAL:
      result = diff == 0;
      break;
    case NOT_EQUAL:
      result = diff != 0;
      break;
    case LESS:
      result = diff < 0;
      break;
    case LESS_OR_EQUAL:
      result = diff <= 0;
      break;
    case GREATER:
      result = diff > 0;
      break;
    case GREATER_OR_EQUAL:
      result = diff >= 0;
      break;
    default:
      result = false;
      break;
  }
  return result;
}

#endif //COMPARATOR_H
