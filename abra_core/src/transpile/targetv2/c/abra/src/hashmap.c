#include "hashmap.h"

hash_entry_t* new_hash_entry(AbraAny key, AbraAny value) {
  hash_entry_t* new_entry = malloc(sizeof(hash_entry_t));
  new_entry->key = key;
  new_entry->value = value;
  new_entry->next = NULL;
  return new_entry;
}

hashmap_t new_hashmap() {
  hashmap_t h;
  h.size = 0;
  h.capacity = 24;
  h.buckets = malloc(sizeof(hash_entry_t) * h.capacity);
  return h;
}

void hashmap_insert(hashmap_t* h, AbraAny key, AbraAny value) {
  if (h->size > h->capacity * 0.75) {
    _hashmap_resize(h);
  }

  int64_t hash = prelude__hash(key).value;
  size_t idx = hash % h->capacity;

  if (h->buckets[idx] == NULL) {
    h->buckets[idx] = new_hash_entry(key, value);
  } else {
    hash_entry_t* node;
    for (node = h->buckets[idx]; node; ) {
      if (prelude__eq(node->key, key, false).value) {
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

AbraAny hashmap_get(hashmap_t* h, AbraAny key) {
  int64_t hash = prelude__hash(key).value;
  size_t idx = hash % h->capacity;

  if (h->buckets[idx] == NULL) return AbraNone;

  hash_entry_t* node;
  for (node = h->buckets[idx]; node; node = node->next) {
    if (prelude__eq(node->key, key, false).value) {
      return node->value;
    }
  }
  return AbraNone;
}

AbraAny hashmap_remove(hashmap_t* h, AbraAny key) {
  int64_t hash = prelude__hash(key).value;
  size_t idx = hash % h->capacity;

  if (h->buckets[idx] == NULL) return AbraNone;

  hash_entry_t* node;
  hash_entry_t* prev;
  size_t depth = 0;
  for (node = h->buckets[idx]; node; node = node->next) {
    if (prelude__eq(node->key, key, false).value) {
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
  return AbraNone;
}

AbraAny* hashmap_keys(hashmap_t* h) {
  AbraAny* keys = malloc(sizeof(AbraAny) * h->size);
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
  h->buckets = malloc(sizeof(hash_entry_t) * h->capacity);
  h->size = 0;

  for (int i = 0; i < old_capacity; ++i) {
    hash_entry_t* node;
    for (node = old_buckets[i]; node; node = node->next) {
      hashmap_insert(h, node->key, node->value);
    }
  }
}
