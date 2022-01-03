#ifndef __ABRA_SET_H
#define __ABRA_SET_H

#include "abra_value.h"
#include "abra_tuple.h"
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

  char* set_str = GC_MALLOC(str_len + 2); // Account for null term
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

// ************************************
// * Set fields & methods
// ************************************

// size: Int
AbraValue std_set__field_size(AbraValue _self) {
  AbraSet* self = (AbraSet*)AS_OBJ(_self);
  return NEW_INT(self->hash.size);
}

// toString(): String
AbraValue std_set__method_toString(void* _env, AbraValue _self) {
  char* str = (char*) std_set__to_string(AS_OBJ(_self));
  // No need to free str, since it's GC_MALLOC'd
  return alloc_string(str, strlen(str));
}

// isEmpty(): Bool
AbraValue std_set__method_isEmpty(void* _env, AbraValue _self) {
  AbraSet* self = (AbraSet*)AS_OBJ(_self);
  return self->hash.size == 0 ? ABRA_TRUE : ABRA_FALSE;
}

// enumerate(): (T, Int)[]
AbraValue std_set__method_enumerate(void* _env, AbraValue _self) {
  AbraSet* self = (AbraSet*)AS_OBJ(_self);

  size_t size = self->hash.size;
  AbraValue* items = GC_MALLOC(sizeof(AbraValue) * size);
  AbraValue* map_keys = hashmap_keys(&self->hash);
  for (int i = 0; i < size; ++i) {
    AbraValue* tuple_items = GC_MALLOC(sizeof(AbraValue) * 2);
    tuple_items[0] = map_keys[i];
    tuple_items[1] = NEW_INT(i);
    items[i] = alloc_tuple(tuple_items, 2);
  }
  return alloc_array(items, size);
}

// contains(item: T): Bool
AbraValue std_set__method_contains(void* _env, AbraValue _self, AbraValue _item) {
  AbraSet* self = (AbraSet*)AS_OBJ(_self);
  return IS_NONE(hashmap_get(&self->hash, _item)) ? ABRA_FALSE : ABRA_TRUE;
}

// insert(item: T)
AbraValue std_set__method_insert(void* _env, AbraValue _self, AbraValue item) {
  std_set__insert(AS_OBJ(_self), item);
  return ABRA_NONE;
}

// remove(item: T)
AbraValue std_set__method_remove(void* _env, AbraValue _self, AbraValue item) {
  AbraSet* self = (AbraSet*)AS_OBJ(_self);
  hashmap_remove(&self->hash, item);
  return ABRA_NONE;
}

// map<U>(fn: (T) => U): U[]
AbraValue std_set__method_map(void* _env, AbraValue _self, AbraValue _fn) {
  AbraSet* self = (AbraSet*)AS_OBJ(_self);
  size_t size = self->hash.size;
  if (size == 0) return alloc_array(GC_MALLOC(0), 0);

  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);
  AbraValue* items = GC_MALLOC(sizeof(AbraValue) * size);
  AbraValue* map_keys = hashmap_keys(&self->hash);
  for (int i = 0; i < size; ++i) {
    AbraValue val = call_fn_1((callable_ctx__1_t*)fn->ctx, map_keys[i]);
    items[i] = val;
  }

  return alloc_array(items, size);
}

// filter(fn: (T) => Bool): Set<T>
AbraValue std_set__method_filter(void* _env, AbraValue _self, AbraValue _fn) {
  AbraSet* self = (AbraSet*)AS_OBJ(_self);
  size_t size = self->hash.size;
  if (size == 0) return alloc_set();

  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);
  AbraValue set = alloc_set();
  AbraValue* map_keys = hashmap_keys(&self->hash);
  size_t count = 0;
  for (int i = 0; i < size; ++i) {
    AbraValue val = call_fn_1((callable_ctx__1_t*)fn->ctx, map_keys[i]);
    if (AS_BOOL(val)) {
      std_set__insert(AS_OBJ(set), map_keys[i]);
    }
  }

  return set;
}

// reduce<U>(initialValue: U, fn: (U, T) => U): U
AbraValue std_set__method_reduce(void* _env, AbraValue _self, AbraValue initial_value, AbraValue _fn) {
  AbraSet* self = (AbraSet*)AS_OBJ(_self);
  size_t size = self->hash.size;
  if (size == 0) return initial_value;

  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);
  AbraValue* map_keys = hashmap_keys(&self->hash);
  for (int i = 0; i < size; ++i) {
    initial_value = call_fn_2((callable_ctx__2_t*)fn->ctx, initial_value, map_keys[i]);
  }

  return initial_value;
}

// asArray(): T[]
AbraValue std_set__method_asArray(void* _env, AbraValue _self) {
  AbraSet* self = (AbraSet*)AS_OBJ(_self);
  size_t size = self->hash.size;
  if (size == 0) return alloc_array(GC_MALLOC(0), 0);

  AbraValue* map_keys = hashmap_keys(&self->hash);
  return alloc_array(map_keys, size);
}

// union(other: Set<T>): Set<T>
AbraValue std_set__method_union(void* _env, AbraValue _self, AbraValue _other) {
  AbraSet* self = (AbraSet*)AS_OBJ(_self);
  AbraSet* other = (AbraSet*)AS_OBJ(_other);

  AbraValue set = alloc_set();
  AbraValue* self_items = hashmap_keys(&self->hash);
  for (int i = 0; i < self->hash.size; ++i) {
    std_set__insert(AS_OBJ(set), self_items[i]);
  }
  AbraValue* other_items = hashmap_keys(&other->hash);
  for (int i = 0; i < other->hash.size; ++i) {
    std_set__insert(AS_OBJ(set), other_items[i]);
  }
  return set;
}

// difference(other: Set<T>): Set<T>
AbraValue std_set__method_difference(void* _env, AbraValue _self, AbraValue _other) {
  AbraSet* self = (AbraSet*)AS_OBJ(_self);
  AbraSet* other = (AbraSet*)AS_OBJ(_other);
  
  AbraValue set = alloc_set();
  size_t size = self->hash.size;
  AbraValue* self_items = hashmap_keys(&self->hash);
  for (int i = 0; i < size; ++i) {
    AbraValue item = self_items[i];
    if (IS_NONE(hashmap_get(&other->hash, item))) {
      std_set__insert(AS_OBJ(set), item);
    }
  }
  return set;
}

// intersection(other: Set<T>): Set<T>
AbraValue std_set__method_intersection(void* _env, AbraValue _self, AbraValue _other) {
  AbraSet* self = (AbraSet*)AS_OBJ(_self);
  AbraSet* other = (AbraSet*)AS_OBJ(_other);

  AbraValue set = alloc_set();
  size_t size = self->hash.size;
  AbraValue* self_items = hashmap_keys(&self->hash);
  for (int i = 0; i < size; ++i) {
    AbraValue item = self_items[i];
    if (!IS_NONE(hashmap_get(&other->hash, item))) {
      std_set__insert(AS_OBJ(set), item);
    }
  }
  return set;
}

#endif
