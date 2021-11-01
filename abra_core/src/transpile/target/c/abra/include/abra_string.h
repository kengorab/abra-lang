#ifndef __ABRA_STRING_H
#define __ABRA_STRING_H

#include "abra_value.h"

typedef struct AbraString {
  Obj _header;
  uint32_t size;
  char* data;
} AbraString;

AbraValue alloc_string(char* data, size_t size) {
  AbraString* str = GC_MALLOC(sizeof(AbraString));

  str->_header.type = OBJ_STR;
  str->size = size;
  if (data != NULL) {
    str->data = strdup(data);
  }
  str->data[size] = 0;

  return ((AbraValue){.type = ABRA_TYPE_OBJ, .as = {.obj = ((Obj*)str)}});
}

AbraValue std_string__concat(AbraValue str1, AbraValue str2) {
  AbraString* s1 = (AbraString*)str1.as.obj;
  AbraString* s2 = (AbraString*)str2.as.obj;

  size_t new_str_len = s1->size + s2->size + 1;
  char* tmp = malloc(new_str_len);
  memcpy(tmp, s1->data, s1->size);
  memcpy(tmp + s1->size, s2->data, s2->size);
  tmp[new_str_len] = 0;

  size_t size = s1->size + s2->size;
  AbraValue new_string = alloc_string(tmp, size);
  free(tmp);
  return new_string;
}

bool std_string__eq(Obj* o1, Obj* o2) {
  AbraString* s1 = (AbraString*)o1;
  AbraString* s2 = (AbraString*)o2;
  if (s1->size != s2->size) return false;

  for (int i = 0; i < s1->size; ++i) {
    if (s1->data[i] != s2->data[i]) return false;
  }
  return true;
}

char const* std_string__to_string(Obj* obj) {
  return ((AbraString*)obj)->data;
}

size_t std_string__hash(Obj* obj) {
  AbraString* self = (AbraString*)obj;

  // Adapted from djb2 hashing algorithm
  size_t hash = 5381;
  for (size_t i = 0; i < self->size; ++i) {
    hash = ((hash << 5) + hash) ^ self->data[i];
  }
  return hash;
}

AbraValue std_string__index(Obj* obj, int64_t index) {
  AbraString* self = (AbraString*)obj;
  int64_t len = (int64_t)self->size;
  if (index < -len || index >= len) return ABRA_NONE;

  if (index < 0) index += len;
  return alloc_string(self->data + index, 1);
}

AbraValue std_string__range(Obj* obj, int64_t start, int64_t end) {
  AbraString* self = (AbraString*)obj;
  int64_t len = self->size;
  range_endpoints(len, &start, &end);

  int64_t slice_size = end - start;
  char* tmp = malloc(slice_size);
  memcpy(tmp, self->data+start, slice_size);
  tmp[slice_size] = 0;

  AbraValue new_string = alloc_string(tmp, slice_size);
  free(tmp);
  return new_string;
}

AbraValue std_string__range_from_start(Obj* obj, int64_t end) {
  return std_string__range(obj, 0, end);
}

AbraValue std_string__range_to_end(Obj* obj, int64_t start) {
  AbraString* self = (AbraString*)obj;
  return std_string__range(obj, start, self->size);
}

AbraValue std_string__field_length(AbraValue _self) {
    AbraString* self = (AbraString*)AS_OBJ(_self);
    return NEW_INT(self->size);
}

#endif
