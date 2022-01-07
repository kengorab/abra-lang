#ifndef __ABRA_ARRAY_H
#define __ABRA_ARRAY_H

#include "../../abra_value.h"
#include "abra_tuple.h"
#include "abra_function.h"
#include "abra_string.h"
#include "abra_map.h"
#include "abra_set.h"
#include "../../callable.h"
#include "../../utils.h"
#include "../../hashmap.h"

void std_array_grow_if_necessary(AbraArray* self) {
  if (self->size == self->capacity) {
    if (self->capacity == 0)
      self->capacity = 2;
    else
      self->capacity *= 2;
    self->items = GC_REALLOC(self->items, sizeof(AbraValue) * self->capacity);
  }
}

AbraValue std_array_shallow_clone(AbraValue* items, size_t count) {
  AbraValue* copy = GC_MALLOC(sizeof(AbraValue) * count);
  for (int i = 0; i < count; ++i) {
    memcpy(copy + i, items + i, sizeof(AbraValue));
  }
  return alloc_array(copy, count);
}

bool std_array__eq(Obj* o1, Obj* o2) {
  AbraArray* self = (AbraArray*)o1;
  AbraArray* other = (AbraArray*)o2;
  if (self->size != other->size) return false;

  for (int i = 0; i < self->size; ++i) {
    if (!std__eq(self->items[i], other->items[i])) return false;
  }
  return true;
}

char const* std_array__to_string(Obj* obj) {
  AbraArray* self = (AbraArray*)obj;
  return array_to_string(self->items, self->size, NULL);
}

size_t std_array__hash(Obj* obj) {
  AbraArray* arr = (AbraArray*)obj;

  // Adapted from djb2 hashing algorithm
  size_t hash = 5381;
  for (int i = 0; i < arr->size; ++i) {
    hash = ((hash << 5) + hash) ^ std__hash(arr->items[i]);
  }
  return hash;
}

AbraValue std_array__index(Obj* obj, int64_t index) {
  AbraArray* self = (AbraArray*)obj;
  int64_t len = (int64_t) self->size;
  if (index < -len || index >= len) return ABRA_NONE;

  if (index < 0) index += len;
  return self->items[index];
}

AbraValue std_array__index_assign(Obj* obj, AbraValue _index, AbraValue item) {
    AbraArray* self = (AbraArray*)obj;
    int64_t index = AS_INT(_index);
    int64_t len = (int64_t) self->size;

    if (index < -len) return item;
    if (index >= len) {
        size_t new_cap = index + 1;
        self->items = GC_REALLOC(self->items, sizeof(AbraValue) * new_cap);
        self->capacity = new_cap;
        self->size = new_cap;
    }
    if (index < 0) index += len;

    self->items[index] = item;
    return item;
}

AbraValue std_array__range(Obj* obj, int64_t start, int64_t end) {
  AbraArray* self = (AbraArray*)obj;
  int64_t len = (int64_t) self->size;
  range_endpoints(len, &start, &end);

  if (start >= end) {
      return alloc_array(GC_MALLOC(0), 0);
  }

  int64_t slice_size = end - start;
  AbraValue* items = GC_MALLOC(sizeof(AbraValue) * slice_size);
  for (int i = start; i < end; ++i) {
    items[i - start] = self->items[i];
  }
  return alloc_array(items, slice_size);
}

AbraValue std_array__range_from_start(Obj* obj, int64_t end) {
  return std_array__range(obj, 0, end);
}

AbraValue std_array__range_to_end(Obj* obj, int64_t start) {
  AbraArray* self = (AbraArray*)obj;
  return std_array__range(obj, start, self->size);
}

// ************************************
// * Array fields & methods
// ************************************

// length: Int
AbraValue std_array__field_length(AbraValue _self) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  return NEW_INT(self->size);
}

// static fill<T1>(amount: Int, value: T1): T1[]
AbraValue std_array__static_method_fill(void* _env, AbraValue _amount, AbraValue value) {
  int64_t amount = AS_INT(_amount);

  AbraValue* items = GC_MALLOC(sizeof(AbraValue) * amount);
  for (int i = 0; i < amount; ++i) {
    items[i] = value;
  }
  return alloc_array(items, amount);
}

// static fillBy<T1>(amount: Int, fn: (Int) => T1): T1[]
AbraValue std_array__static_method_fillBy(void* _env, AbraValue _amount, AbraValue _fn) {
  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);
  int64_t amount = AS_INT(_amount);

  AbraValue* items = GC_MALLOC(sizeof(AbraValue) * amount);
  for (int i = 0; i < amount; ++i) {
    AbraValue value = call_fn_1((callable_ctx__1_t*)fn->ctx, NEW_INT(i));
    items[i] = value;
  }
  return alloc_array(items, amount);
}

// toString(): String
AbraValue std_array__method_toString(void* _env, AbraValue _self) {
  char* str = (char*) std_array__to_string(AS_OBJ(_self));
  // No need to free str, since it's GC_MALLOC'd
  return alloc_string(str, strlen(str));
}

// isEmpty(): Bool
AbraValue std_array__method_isEmpty(void* _env, AbraValue _self) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  return self->size == 0 ? ABRA_TRUE : ABRA_FALSE;
}

// enumerate(): (T, Int)[]
AbraValue std_array__method_enumerate(void* _env, AbraValue _self) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);

  AbraValue* items = GC_MALLOC(sizeof(AbraValue) * self->size);
  for (int i = 0; i < self->size; ++i) {
    AbraValue* tuple_items = GC_MALLOC(sizeof(AbraValue) * 2);
    tuple_items[0] = self->items[i];
    tuple_items[1] = NEW_INT(i);
    items[i] = alloc_tuple(tuple_items, 2);
  }

  return alloc_array(items, self->size);
}

// push(item: T, *others: T[])
AbraValue std_array__method_push(void* _env, AbraValue _self, AbraValue item, AbraValue _others) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);

  std_array_grow_if_necessary(self);
  self->items[self->size++] = item;

  if (!IS_NONE(_others)) {
    AbraArray* others = (AbraArray*)AS_OBJ(_others);
    for (int i = 0; i < others->size; ++i) {
      std_array_grow_if_necessary(self);
      self->items[self->size++] = others->items[i];
    }
  }

  return ABRA_NONE;
}

// pop(): T?
AbraValue std_array__method_pop(void* _env, AbraValue _self) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  if (self->size == 0) return ABRA_NONE;

  AbraValue item = self->items[self->size - 1];
  self->size--;
  return item;
}

// popFront(): T?
AbraValue std_array__method_popFront(void* _env, AbraValue _self) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  if (self->size == 0) return ABRA_NONE;

  AbraValue item = self->items[0];
  self->size--;
  self->items = self->items + 1;
  return item;
}

// splitAt(index: Int): (T[], T[])
AbraValue std_array__method_splitAt(void* _env, AbraValue _self, AbraValue _index) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  int64_t index = AS_INT(_index);

  AbraValue* tuple_items = GC_MALLOC(sizeof(AbraValue) * 2);
  if (index >= self->size) {
    tuple_items[0] = std_array_shallow_clone(self->items, self->size);
    tuple_items[1] = alloc_array(GC_MALLOC(sizeof(AbraValue) * 0), 0);
  } else if (index < (-1 * (int64_t)self->size)) {
    AbraValue* copy = GC_MALLOC(sizeof(AbraValue) * self->size);
    memcpy(copy, self->items, self->size);
    tuple_items[0] = alloc_array(GC_MALLOC(sizeof(AbraValue) * 0), 0);
    tuple_items[1] = std_array_shallow_clone(self->items, self->size);
  } else {
    size_t split_idx = (self->size + index) % self->size;
    tuple_items[0] = std_array_shallow_clone(self->items, split_idx);
    tuple_items[1] = std_array_shallow_clone(self->items + split_idx, self->size - split_idx);
  }
  return alloc_tuple(tuple_items, 2);
}

// concat(other: T[]): T[]
AbraValue std_array__method_concat(void* _env, AbraValue _self, AbraValue _other) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  AbraArray* other = (AbraArray*)AS_OBJ(_other);

  size_t concat_size = self->size + other->size;
  AbraValue* concat_items = GC_MALLOC(sizeof(AbraValue) * concat_size);
  for (int i = 0; i < self->size; ++i) {
    concat_items[i] = self->items[i];
  }
  for (int i = 0; i < other->size; ++i) {
    concat_items[self->size + i] = other->items[i];
  }
  return alloc_array(concat_items, concat_size);
}

// map<U>(fn: (T) => U): U[]
AbraValue std_array__method_map(void* _env, AbraValue _self, AbraValue _fn) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  if (self->size == 0) return _self;

  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);
  AbraValue* items = GC_MALLOC(sizeof(AbraValue) * self->size);
  for (int i = 0; i < self->size; ++i) {
    AbraValue val = call_fn_1((callable_ctx__1_t*)fn->ctx, self->items[i]);
    items[i] = val;
  }
  return alloc_array(items, self->size);
}

// filter(fn: (T) => Bool): T[]
AbraValue std_array__method_filter(void* _env, AbraValue _self, AbraValue _fn) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  if (self->size == 0) return _self;

  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);
  AbraValue* items = GC_MALLOC(sizeof(AbraValue) * self->size);
  size_t count = 0;
  for (int i = 0; i < self->size; ++i) {
    AbraValue val = call_fn_1((callable_ctx__1_t*)fn->ctx, self->items[i]);
    if (AS_BOOL(val)) {
      items[count++] = self->items[i];
    }
  }
  return alloc_array(items, count);
}

// reduce<U>(initialValue: U, fn: (U, T) => U): U
AbraValue std_array__method_reduce(void* _env, AbraValue _self, AbraValue initial_value, AbraValue _fn) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  if (self->size == 0) return _self;

  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);
  for (int i = 0; i < self->size; ++i) {
    initial_value = call_fn_2((callable_ctx__2_t*)fn->ctx, initial_value, self->items[i]);
  }
  return initial_value;
}

// forEach(fn: (T) => Unit)
AbraValue std_array__method_forEach(void* _env, AbraValue _self, AbraValue _fn) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  if (self->size == 0) return _self;

  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);
  for (int i = 0; i < self->size; ++i) {
    call_fn_1((callable_ctx__1_t*)fn->ctx, self->items[i]);
  }
  return ABRA_NONE;
}

// join(joiner?: String): String
AbraValue std_array__method_join(void* _env, AbraValue _self, AbraValue _joiner) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  if (self->size == 0) return alloc_string("", 0);

  char* joiner_str = "";
  size_t joiner_size = 0;
  if (!IS_NONE(_joiner)) {
    AbraString* joiner = (AbraString*)AS_OBJ(_joiner);
    joiner_str = joiner->data;
    joiner_size = joiner->size;
  }

  char** items = malloc(sizeof(char*) * self->size);
  size_t* item_sizes = malloc(sizeof(size_t*) * self->size);
  size_t total_size = joiner_size * (self->size - 1);
  for (int i = 0; i < self->size; ++i) {
    items[i] = (char*) std__to_string(self->items[i]);
    item_sizes[i] = strlen(items[i]);
    total_size += item_sizes[i];
  }

  char* str = malloc(total_size);
  size_t offset = 0;
  for (int i = 0; i < self->size; ++i) {
    memcpy(str + offset, items[i], item_sizes[i]);
    offset += item_sizes[i];
    memcpy(str + offset, joiner_str, joiner_size);
    offset += joiner_size;
  }

  AbraValue ret = alloc_string(str, total_size);
  free(str);
  free(items);
  free(item_sizes);
  return ret;
}

// contains(item: T): Bool
AbraValue std_array__method_contains(void* _env, AbraValue _self, AbraValue item) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  for (int i = 0; i < self->size; ++i) {
    if (std__eq(self->items[i], item)) return ABRA_TRUE;
  }
  return ABRA_FALSE;
}

void array_find_item(AbraArray* self, AbraFunction* fn, AbraValue* item, size_t* index) {
  for (int i = 0; i < self->size; ++i) {
    AbraValue val = call_fn_1((callable_ctx__1_t*)fn->ctx, self->items[i]);
    if (IS_FALSY(val)) continue;

    *item = self->items[i];
    *index = i;
    return;
  }

  *index = -1;
}

// find<U>(fn: (T) => (Bool | U?)): T?
AbraValue std_array__method_find(void* _env, AbraValue _self, AbraValue _fn) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);

  AbraValue item;
  size_t idx;
  array_find_item(self, fn, &item, &idx);
  return idx == -1 ? ABRA_NONE : item;
}

// findIndex<U>(fn: (T) => (Bool | U?)): (T, Int)?
AbraValue std_array__method_findIndex(void* _env, AbraValue _self, AbraValue _fn) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);

  AbraValue item;
  size_t idx;
  array_find_item(self, fn, &item, &idx);
  if (idx == -1) return ABRA_NONE;

  AbraValue* tuple_items = GC_MALLOC(sizeof(AbraValue) * 2);
  tuple_items[0] = item;
  tuple_items[1] = NEW_INT(idx);
  return alloc_tuple(tuple_items, 2);
}

// any<U>(fn: (T) => (Bool | U?)): Bool
AbraValue std_array__method_any(void* _env, AbraValue _self, AbraValue _fn) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);

  AbraValue item;
  size_t idx;
  array_find_item(self, fn, &item, &idx);
  return idx == -1 ? ABRA_FALSE : ABRA_TRUE;
}

// all<U>(fn: (T) => (Bool | U?)): Bool
AbraValue std_array__method_all(void* _env, AbraValue _self, AbraValue _fn) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);

  for (int i = 0; i < self->size; ++i) {
    AbraValue val = call_fn_1((callable_ctx__1_t*)fn->ctx, self->items[i]);
    if (IS_FALSY(val)) return ABRA_FALSE;
  }
  return ABRA_TRUE;
}

// none<U>(fn: (T) => (Bool | U?)): Bool
AbraValue std_array__method_none(void* _env, AbraValue _self, AbraValue _fn) {
  AbraValue all = std_array__method_all(_env, _self, _fn);
  return AS_BOOL(all) ? ABRA_FALSE : ABRA_TRUE;
}

// sortBy(fn: (T) => Int, reverse?: Bool): T[]
AbraValue std_array__method_sortBy(void* _env, AbraValue _self, AbraValue _fn, AbraValue _reverse) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);

  bool reverse = false;
  if (!IS_NONE(_reverse)) {
    reverse = AS_BOOL(_reverse);
  }

  sort_item_t* sort_items = malloc(sizeof(sort_item_t) * self->size);
  for (int i = 0; i < self->size; ++i) {
    AbraValue val = call_fn_1((callable_ctx__1_t*)fn->ctx, self->items[i]);
    sort_item_t item = {
      .sort_key = AS_INT(val),
      .sort_value = self->items[i],
    };
    sort_items[i] = item;
  }
  quicksort(sort_items, 0, self->size - 1);

  AbraValue* sorted_items = GC_MALLOC(sizeof(AbraValue) * self->size);
  for (int i = 0; i < self->size; ++i) {
    size_t idx = reverse ? self->size - i - 1 : i;
    sorted_items[idx] = sort_items[i].sort_value;
  }
  free(sort_items);
  return alloc_array(sorted_items, self->size);
}

// dedupe(): T[]
AbraValue std_array__method_dedupe(void* _env, AbraValue _self) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);

  hashmap_t map = new_hashmap(&std__hash, &std__eq);
  AbraValue* items = GC_MALLOC(sizeof(AbraValue) * self->size);
  size_t count = 0;
  for (int i = 0; i < self->size; ++i) {
    AbraValue item = self->items[i];
    if (IS_NONE(hashmap_get(&map, item))) {
      hashmap_insert(&map, item, ABRA_TRUE);
      items[count++] = item;
    }
  }
  return alloc_array(items, count);
}

// dedupeBy<U>(fn: (T) => U): T[]
AbraValue std_array__method_dedupeBy(void* _env, AbraValue _self, AbraValue _fn) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);

  hashmap_t map = new_hashmap(&std__hash, &std__eq);
  AbraValue* items = GC_MALLOC(sizeof(AbraValue) * self->size);
  size_t count = 0;
  for (int i = 0; i < self->size; ++i) {
    AbraValue item = self->items[i];
    AbraValue dedupe_key = call_fn_1((callable_ctx__1_t*)fn->ctx, item);
    if (IS_NONE(hashmap_get(&map, dedupe_key))) {
      hashmap_insert(&map, dedupe_key, ABRA_TRUE);
      items[count++] = item;
    }
  }
  return alloc_array(items, count);
}

// partition<U>(fn: (T) => U): Map<U, T[]>
AbraValue std_array__method_partition(void* _env, AbraValue _self, AbraValue _fn) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);

  AbraValue map = alloc_map();
  for (int i = 0; i < self->size; ++i) {
    AbraValue item = self->items[i];
    AbraValue partition_key = call_fn_1((callable_ctx__1_t*)fn->ctx, item);

    AbraValue partition = std_map__index(AS_OBJ(map), partition_key);
    if (IS_NONE(partition)) {
      AbraValue* items = GC_MALLOC(sizeof(AbraValue) * 1);
      items[0] = item;
      AbraValue arr = alloc_array(items, 1);
      std_map__insert(AS_OBJ(map), partition_key, arr);
    } else {
      std_array__method_push(_env, partition, item, ABRA_NONE);
    }
  }
  return map;
}

// tally(): Map<T, Int>
AbraValue std_array__method_tally(void* _env, AbraValue _self) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);

  AbraValue map = alloc_map();
  for (int i = 0; i < self->size; ++i) {
    AbraValue item = self->items[i];

    AbraValue cur_tally = std_map__index(AS_OBJ(map), item);
    if (IS_NONE(cur_tally)) {
      std_map__insert(AS_OBJ(map), item, NEW_INT(1));
    } else {
      std_map__insert(AS_OBJ(map), item, NEW_INT(AS_INT(cur_tally) + 1));
    }
  }
  return map;
}

// tallyBy<U>(fn: (T) => U): Map<U, Int>
AbraValue std_array__method_tallyBy(void* _env, AbraValue _self, AbraValue _fn) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);

  AbraValue map = alloc_map();
  for (int i = 0; i < self->size; ++i) {
    AbraValue item = self->items[i];
    AbraValue tally_key = call_fn_1((callable_ctx__1_t*)fn->ctx, item);

    AbraValue cur_tally = std_map__index(AS_OBJ(map), tally_key);
    if (IS_NONE(cur_tally)) {
      std_map__insert(AS_OBJ(map), tally_key, NEW_INT(1));
    } else {
      std_map__insert(AS_OBJ(map), tally_key, NEW_INT(AS_INT(cur_tally) + 1));
    }
  }
  return map;
}

// asSet(): Set<T>
AbraValue std_array__method_asSet(void* _env, AbraValue _self) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);

  AbraValue set = alloc_set();
  for (int i = 0; i < self->size; ++i) {
    std_set__insert(AS_OBJ(set), self->items[i]);
  }
  return set;
}

// getOrDefault(key: Int, default: T): T
AbraValue std_array__method_getOrDefault(void* _env, AbraValue _self, AbraValue _key, AbraValue _default) {
  AbraValue item = std_array__index(AS_OBJ(_self), AS_INT(_key));
  return IS_NONE(item) ? _default : item;
}

// getOrElse(key: Int, fn: () => T): T
AbraValue std_array__method_getOrElse(void* _env, AbraValue _self, AbraValue _key, AbraValue _fn) {
  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);
  AbraValue item = std_array__index(AS_OBJ(_self), AS_INT(_key));
  if (IS_NONE(item)) {
    return call_fn_0((callable_ctx__0_t*)fn->ctx);
  }
  return item;
}

// update(key: Int, fn: (T) => T)
AbraValue std_array__method_update(void* _env, AbraValue _self, AbraValue _key, AbraValue _fn) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);
  AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);
  int64_t index = AS_INT(_key);
  AbraValue item = std_array__index(AS_OBJ(_self), index);

  if (IS_NONE(item)) return ABRA_NONE;

  AbraValue replacement = call_fn_1((callable_ctx__1_t*)fn->ctx, item);
  if (index < 0) index += self->size;
  self->items[index] = replacement;

  return ABRA_NONE;
}

// reverse(): T[]
AbraValue std_array__method_reverse(void* _env, AbraValue _self) {
  AbraArray* self = (AbraArray*)AS_OBJ(_self);

  AbraValue* items = GC_MALLOC(sizeof(AbraValue) * self->size);
  for (int i = 0; i < self->size; ++i) {
    items[i] = self->items[self->size - 1 - i];
  }
  return alloc_array(items, self->size);
}

#endif
