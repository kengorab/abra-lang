#ifndef __HASHMAP_H
#define __HASHMAP_H

#include "stdbool.h"
#include "stdint.h"
#include "gc.h"
#include "nan.h"

typedef uint32_t (*hash_fn_t)(value_t);
typedef bool (*eq_fn_t)(value_t, value_t);

typedef struct hash_entry_t {
  value_t key;
  value_t value;
  struct hash_entry_t* next;
} hash_entry_t;

hash_entry_t* new_hash_entry(value_t key, value_t value) {
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
  hash_fn_t hash_fn;
  eq_fn_t eq_fn;
} hashmap_t;

hashmap_t new_hashmap(uint32_t init_size, hash_fn_t hash_fn, eq_fn_t eq_fn) {
  hashmap_t h;
  h.size = 0;
  h.capacity = init_size == 0 ? 24 : init_size;
  h.buckets = GC_MALLOC(sizeof(hash_entry_t) * h.capacity);
  h.hash_fn = hash_fn;
  h.eq_fn = eq_fn;
  return h;
}

void _hashmap_resize(hashmap_t* h);
void hashmap_insert(hashmap_t* h, value_t key, value_t value) {
  if (h->size > h->capacity * 0.75) {
    _hashmap_resize(h);
  }

  uint32_t hash = h->hash_fn(key);
  uint32_t idx = hash % h->capacity;

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

value_t hashmap_get(hashmap_t* h, value_t key) {
  uint32_t hash = h->hash_fn(key);
  uint32_t idx = hash % h->capacity;

  if (h->buckets[idx] == NULL) return VAL_NONE;

  hash_entry_t* node;
  for (node = h->buckets[idx]; node; node = node->next) {
    if (h->eq_fn(node->key, key)) {
      return node->value;
    }
  }

  return VAL_NONE;
}

value_t hashmap_remove(hashmap_t* h, value_t key) {
  uint32_t hash = h->hash_fn(key);
  uint32_t idx = hash % h->capacity;

  if (h->buckets[idx] == NULL) return VAL_NONE;

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

  return VAL_NONE;
}

value_t* hashmap_keys(hashmap_t* h) {
  value_t* keys = GC_MALLOC(sizeof(value_t) * h->size);
  uint32_t idx = 0;
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
  uint32_t new_capacity = h->capacity * 2;
  uint32_t old_capacity = h->capacity;
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
