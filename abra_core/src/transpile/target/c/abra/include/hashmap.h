#ifndef __ABRA_HASHMAP_H
#define __ABRA_HASHMAP_H

#include "gc.h"
#include "abra_value.h"

typedef struct hash_entry_t {
  AbraValue key;
  AbraValue value;
  struct hash_entry_t* next;
} hash_entry_t;

hash_entry_t* new_hash_entry(AbraValue key, AbraValue value) {
  hash_entry_t* new_entry = GC_MALLOC(sizeof(hash_entry_t));
  new_entry->key = key;
  new_entry->value = value;
  new_entry->next = NULL;
  return new_entry;
}

typedef struct hashmap_t {
  size_t size;
  size_t capacity;
  hash_entry_t** buckets;
  size_t (*hash_fn)(AbraValue);
  bool (*eq_fn)(AbraValue, AbraValue);
} hashmap_t;

hashmap_t new_hashmap(
  size_t (*hash_fn)(AbraValue),
  bool (*eq_fn)(AbraValue, AbraValue)
) {
  hashmap_t h;
  h.size = 0;
  h.capacity = 24;
  h.buckets = GC_MALLOC(sizeof(hash_entry_t) * h.capacity);
  h.hash_fn = hash_fn;
  h.eq_fn = eq_fn;
  return h;
}

void _hashmap_resize(hashmap_t* h);
void hashmap_insert(hashmap_t* h, AbraValue key, AbraValue value) {
  if (h->size > h->capacity * 0.75) {
    _hashmap_resize(h);
  }

  size_t hash = h->hash_fn(key);
  size_t idx = hash % h->capacity;

  if (h->buckets[idx] == NULL) {
    h->buckets[idx] = new_hash_entry(key, value);
  } else {
    hash_entry_t* node;
    for (node = h->buckets[idx]; node; ) {
      if (h->eq_fn(node->key, key)) {
        node->value = value;
        return;
      }
      if (node->next) {
        node = node->next;
      } else {
        break;
      }
    }
    node->next = new_hash_entry(key, value);
  }

  h->size++;
}

AbraValue hashmap_get(hashmap_t* h, AbraValue key) {
  size_t hash = h->hash_fn(key);
  size_t idx = hash % h->capacity;

  if (h->buckets[idx] == NULL) return ABRA_NONE;

  hash_entry_t* node;
  for (node = h->buckets[idx]; node; node = node->next) {
    if (h->eq_fn(node->key, key)) {
      return node->value;
    }
  }
  return ABRA_NONE;
}

AbraValue hashmap_remove(hashmap_t* h, AbraValue key) {
    size_t hash = h->hash_fn(key);
    size_t idx = hash % h->capacity;

    if (h->buckets[idx] == NULL) return ABRA_NONE;

    hash_entry_t* node;
    hash_entry_t* prev;
    size_t depth = 0;
    for (node = h->buckets[idx]; node; node = node->next) {
        if (h->eq_fn(node->key, key)) {
            if (depth == 0) {
                h->buckets[idx] = node->next;
            } else {
                prev->next = node->next;
            }
            h->size--;
            return node->value;
        }
        prev = node;
        depth++;
    }
    return ABRA_NONE;
}

AbraValue* hashmap_keys(hashmap_t* h) {
  AbraValue* keys = GC_MALLOC(sizeof(AbraValue) * h->size);
  size_t idx = 0;
  for (int i = 0; i < h->capacity; ++i) {
    if (h->buckets[i] == NULL) continue;

    hash_entry_t* node;
    for (node = h->buckets[i]; node; node = node->next) {
      keys[idx++] = node->key;
    }
  }
  return keys;
}

void _hashmap_resize(hashmap_t* h) {
  size_t new_capacity = h->capacity * 2;
  size_t old_capacity = h->capacity;
  h->capacity = new_capacity;

  hash_entry_t** old_buckets = h->buckets;
  h->buckets = GC_MALLOC(sizeof(hash_entry_t) * h->capacity);
  h->size = 0;

  for (int i = 0; i < old_capacity; ++i) {
    hash_entry_t* node;
    for (node = old_buckets[i]; node; node = node->next) {
      hashmap_insert(h, node->key, node->value);
    }
  }
}

#endif
