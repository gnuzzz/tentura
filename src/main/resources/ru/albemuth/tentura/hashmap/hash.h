#ifndef HASHMAP_HASH_H
#define HASHMAP_HASH_H

template<typename K>
struct Hash {
  __device__ unsigned long operator()(const K &key) const {
    return reinterpret_cast<unsigned long>(key);
  }
};

struct IntHash {
  __device__ unsigned long operator()(const int &key) const {
    return key;
  }
};

#endif //HASHMAP_HASH_H
