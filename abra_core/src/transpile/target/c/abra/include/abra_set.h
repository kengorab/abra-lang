#ifndef __ABRA_SET_H
#define __ABRA_SET_H

#include "abra_value.h"
#include "hashmap.h"

typedef struct AbraSet {
  Obj _header;
  hashmap_t hash;
} AbraSet;

AbraValue alloc_set() {
  AbraSet* set = GC_MALLOC(sizeof(AbraSet));

  set->_header.type = OBJ_SET;
  set->hash = new_hashmap(&std__hash, &std__eq);

  return ((AbraValue){.type = ABRA_TYPE_OBJ, .as = {.obj = ((Obj*)set)}});
}

void std_set__insert(Obj* o, AbraValue val) {
  AbraSet* self = (AbraSet*)o;
  hashmap_insert(&self->hash, val, ABRA_TRUE);
}

bool std_set__eq(Obj* o1, Obj* o2) {
  AbraSet* self = (AbraSet*)o1;
  AbraSet* other = (AbraSet*)o2;
  if (self->hash.size != other->hash.size) return false;

  AbraValue* keys = hashmap_keys(&self->hash);
  for (int i = 0; i < self->hash.size; ++i) {
    AbraValue key = keys[i];
    AbraValue self_val = hashmap_get(&self->hash, key);
    AbraValue other_val = hashmap_get(&other->hash, key);
    if (!std__eq(self_val, other_val)) return false;
  }
  return true;
}

char const* std_set__to_string(Obj* obj) {
  AbraSet* self = (AbraSet*)obj;

  if (self->hash.size == 0) return "#{}";

  AbraValue* keys = hashmap_keys(&self->hash);
  size_t str_len;
  char* arr_str = (char*) array_to_string(keys, self->hash.size, &str_len);
  arr_str[0] = '{';
  arr_str[str_len - 1] = '}';

  char* set_str = GC_MALLOC(str_len + 1);
  set_str[0] = '#';
  memcpy(set_str + 1, arr_str, str_len);

  return set_str;
}

size_t std_set__hash(Obj* obj) {
  AbraSet* self = (AbraSet*)obj;

  // Adapted from djb2 hashing algorithm
  size_t hash = 4253;
  AbraValue* keys = hashmap_keys(&self->hash);
  for (int i = 0; i < self->hash.size; ++i) {
    AbraValue key = keys[i];
    hash = ((hash << 5) + hash) ^ std__hash(key);
  }
  return hash;
}

AbraValue std_set__field_size(AbraValue _self) {
    AbraSet* self = (AbraSet*)AS_OBJ(_self);
    return NEW_INT(self->hash.size);
}

#endif
