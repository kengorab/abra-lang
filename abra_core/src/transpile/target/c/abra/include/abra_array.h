#ifndef __ABRA_ARRAY_H
#define __ABRA_ARRAY_H

#include "abra_value.h"
#include "utils.h"

typedef struct AbraArray {
  Obj _header;
  uint32_t size;
  uint32_t capacity;
  AbraValue* items;
} AbraArray;

AbraValue alloc_array(AbraValue* values, size_t size) {
  AbraArray* arr = GC_MALLOC(sizeof(AbraArray));

  arr->_header.type = OBJ_ARRAY;
  arr->size = size;
  arr->capacity = size;
  arr->items = values;

  return ((AbraValue){.type = ABRA_TYPE_OBJ, .as = {.obj = ((Obj*)arr)}});
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

AbraValue std_array__range(Obj* obj, int64_t start, int64_t end) {
  AbraArray* self = (AbraArray*)obj;
  int64_t len = (int64_t) self->size;
  range_endpoints(len, &start, &end);

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

#endif
