#ifndef HASHMAP_HASHMAP_H
#define HASHMAP_HASHMAP_H

#include "hash.h"
#include "entry.h"

template<typename K, typename V, typename F = Hash<K>>
class HashMap {
public:
  __device__ HashMap(size_t size) : hashFunc() {
    bucketsSize = size;
    buckets = new Entry<K, V> *[bucketsSize];
    for (size_t i = 0; i < bucketsSize; i++) {
      buckets[i] = NULL;
    }
  }

  __device__ ~HashMap() {
    for (size_t i = 0; i < bucketsSize; i++) {
      Entry<K, V> *entry = buckets[i];

      while (entry != NULL) {
        Entry<K, V> *prev = entry;
        entry = entry->next();
        delete prev;
      }

      buckets[i] = NULL;
    }

    delete buckets;
  }

  __device__ bool get(const K &key, V &value) {
    unsigned long bucket = bucketIndex(key);
    Entry<K, V> *entry = buckets[bucket];
    while (entry != NULL && entry->key() != key) {
      entry = entry->next();
    }
    if (entry != NULL) {
      value = entry->value();
      return true;
    } else {
      return false;
    }
  }

  __device__ Entry<K, V> *entry(const K &key) {
    unsigned long bucket = bucketIndex(key);
    Entry<K, V> *entry = buckets[bucket];
    while (entry != NULL && entry->key() != key) {
      entry = entry->next();
    }
    return entry;
  };

  __device__ void put(const K &key, const V &value) {
    unsigned long bucket = bucketIndex(key);
    Entry<K, V> *prev = NULL;
    Entry<K, V> *entry = buckets[bucket];
    while (entry != NULL && entry->key() != key) {
      prev = entry;
      entry = entry->next();
    }
    if (entry == NULL) {
      entry = new Entry<K, V>(key, value);
      if (prev == NULL) {
        buckets[bucket] = entry;
      } else {
        prev->setNext(entry);
      }
    } else {
      entry->setValue(value);
    }
  }

  __device__ void remove(const K &key) {
    unsigned long bucket = bucketIndex(key);
    Entry<K, V> *prev = NULL;
    Entry<K, V> *entry = buckets[bucket];
    while (entry != NULL && entry->key() != key) {
      prev = entry;
      entry = entry->getNext();
    }
    if (entry != NULL) {
      if (prev == NULL) {
        buckets[bucket] = entry->next();
      } else {
        prev->setNext(entry->next());
      }
      delete entry;
    }
  }

  class EntriesIterator {
  public:
    __device__ EntriesIterator(HashMap<K, V, F> *_map) {
      map = _map;
      bucket = 0;
      entry = nextEntry(map->buckets[0]);
    }

    __device__ ~EntriesIterator() {
      entry = NULL;
      bucket = 0;
      map = NULL;
    }

    __device__ bool hasNext() {
      return entry != NULL;
    }

    __device__ Entry<K, V> *next() {
      if (entry == NULL) return NULL;
      Entry<K, V> *ret = entry;
      entry = nextEntry(entry->next());
      return ret;
    }

  private:
    HashMap<K, V, F> *map;
    unsigned long bucket;
    Entry<K, V> *entry;

    __device__ Entry<K, V> *nextEntry(Entry<K, V> *currentEntry) {
      for (; currentEntry == NULL && ++bucket < map->bucketsSize; ) {
        currentEntry = map->buckets[bucket];
      }
      return currentEntry;
    };
  };

  __device__ EntriesIterator *entriesIterator() {
    return new EntriesIterator(this);
  }

protected:
  __device__ HashMap(const HashMap &other);

  __device__ const HashMap &operator=(const HashMap &other);

  __device__ unsigned long bucketIndex(const K &key) const {
    return hashFunc(key) % bucketsSize;
  }

  size_t bucketsSize;
  Entry<K, V> **buckets;
  F hashFunc;
};

#endif //HASHMAP_HASHMAP_H
