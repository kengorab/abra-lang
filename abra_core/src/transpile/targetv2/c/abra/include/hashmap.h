#ifndef ABRA_HASHMAP_H
#define ABRA_HASHMAP_H

#include "prelude.h"

typedef struct hash_entry_t {
    AbraAny key;
    AbraAny value;
    struct hash_entry_t* next;
} hash_entry_t;

hash_entry_t* new_hash_entry(AbraAny key, AbraAny value);

typedef struct hashmap_t {
    size_t size;
    size_t capacity;
    hash_entry_t** buckets;
} hashmap_t;

hashmap_t new_hashmap();
void hashmap_insert(hashmap_t* h, AbraAny key, AbraAny value);
AbraAny hashmap_get(hashmap_t* h, AbraAny key);
AbraAny hashmap_remove(hashmap_t* h, AbraAny key);
AbraAny* hashmap_keys(hashmap_t* h);
void _hashmap_resize(hashmap_t* h);

#endif // ABRA_HASHMAP_H
