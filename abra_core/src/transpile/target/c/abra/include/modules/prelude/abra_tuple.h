#ifndef __ABRA_TUPLE_H
#define __ABRA_TUPLE_H

#include "../../abra_value.h"
#include "../../utils.h"

typedef struct AbraTuple {
  Obj _header;
  uint32_t size;
  AbraValue* items;
} AbraTuple;

AbraValue alloc_tuple(AbraValue* values, size_t size) {
  AbraTuple* tuple = GC_MALLOC(sizeof(AbraTuple));

  tuple->_header.type = OBJ_TUPLE;
  tuple->size = size;
  tuple->items = values;

  return ((AbraValue){.type = ABRA_TYPE_OBJ, .as = {.obj = ((Obj*)tuple)}});
}

bool std_tuple__eq(Obj* o1, Obj* o2) {
  AbraTuple* self = (AbraTuple*)o1;
  AbraTuple* other = (AbraTuple*)o2;
  if (self->size != other->size) return false;

  for (int i = 0; i < self->size; ++i) {
    if (!std__eq(self->items[i], other->items[i])) return false;
  }
  return true;
}

char const* std_tuple__to_string(Obj* obj) {
  AbraTuple* self = (AbraTuple*)obj;
  size_t s;
  char* str = (char*) array_to_string(self->items, self->size, &s);
  str[0] = '(';
  str[s - 1] = ')';
  return str;
}

size_t std_tuple__hash(Obj* obj) {
  AbraTuple* tuple = (AbraTuple*)obj;

  // Adapted from djb2 hashing algorithm
  size_t hash = 4253;
  for (int i = 0; i < tuple->size; ++i) {
      hash = ((hash << 5) + hash) ^ std__hash(tuple->items[i]);
  }
  return hash;
}

AbraValue std_tuple__index(Obj* obj, int64_t index) {
  AbraTuple* self = (AbraTuple*)obj;
  return self->items[index];
}

AbraValue std_tuple__index_assign(Obj* obj, AbraValue _index, AbraValue item) {
    AbraTuple* self = (AbraTuple*)obj;
    int64_t index = AS_INT(_index);
    self->items[index] = item;
    return item;
}

#endif
