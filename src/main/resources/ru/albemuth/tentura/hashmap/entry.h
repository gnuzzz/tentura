#ifndef HASHMAP_ENTRY_H
#define HASHMAP_ENTRY_H

template<typename K, typename V>
class Entry {
public:
  __device__ Entry(const K &key, const V &value) : _key(key), _value(value), _next(NULL) {
  }

  __device__ K key() const {
    return _key;
  }

  __device__ V value() const {
    return _value;
  }

  __device__ void setValue(V value) {
    _value = value;
  }

  __device__ Entry *next() const {
    return _next;
  }

  __device__ void setNext(Entry *next) {
    _next = next;
  }


private:
  K _key;
  V _value;
  Entry *_next;

  __device__ Entry(const Entry &);

  __device__ Entry &operator=(const Entry &);
};

#endif //HASHMAP_ENTRY_H
