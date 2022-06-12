#ifndef __ABRA_RT_H
#define __ABRA_RT_H

#include "stdio.h"
#include "stdlib.h"
#include "stdint.h"
#include "string.h"
#include "stdbool.h"
#include "math.h"

// NaN-tagging
//
// Treat double/uint64_t as 64-bit array
//
// 63          51 48
// v           v  v
// SEEEEEEEEEEEMMMMMMMMMMMMMMMMMM...
//

typedef uint64_t value_t;

typedef union {
  value_t raw;
  double d;
} value_t_transmute;

value_t double_to_value_t(double value) {
  value_t_transmute t = {.d = value};
  return t.raw;
}

double value_t_to_double(value_t value) {
  value_t_transmute t = {.raw = value};
  return t.d;
}

const uint64_t MASK_NAN =            (uint64_t)0x7ffc000000000000;
const uint64_t MASK_INT = MASK_NAN | (uint64_t)0x0002000000000000;
const uint64_t MASK_OBJ = MASK_NAN | (uint64_t)0x8000000000000000;

//const uint64_t VAL_NONE  = MASK_NAN | (uint64_t)0x0001000000000000;
const uint64_t VAL_FALSE = MASK_NAN | (uint64_t)0x0001000000000001;
const uint64_t VAL_TRUE  = MASK_NAN | (uint64_t)0x0001000000000002;

const uint64_t PAYLOAD_MASK_INT = (uint64_t)0x00000000ffffffff;
const uint64_t PAYLOAD_MASK_OBJ = (uint64_t)0x0000ffffffffffff;

#define AS_INT(val)    ((int32_t) (val & PAYLOAD_MASK_INT))
#define AS_DOUBLE(val) ((double) (value_t_to_double(val)))

#define AS_OBJ(val, typ) ((typ*)(val & PAYLOAD_MASK_OBJ))
#define MK_OBJ(val)      (MASK_OBJ | (uint64_t)val)

// ------------------------------------------------------------------------

typedef struct String {
  int32_t size;
  char* chars;
} String;

value_t prelude__String__toString(value_t self) {
  return self;
}

value_t prelude__String__toUpper(value_t _self) {
  String* self = AS_OBJ(_self, String);

  char* chars = strdup(self->chars);
  for (int i = 0; i < self->size; ++i) {
    if (chars[i] >= 'a' && chars[i] <= 'z') {
      chars[i] = chars[i] - 32;
    }
  }

  String* s = GC_MALLOC(sizeof(String));
  s->size = self->size;
  s->chars = chars;
  return MK_OBJ(s);
}

value_t prelude__Int__toString(value_t _self) {
  int32_t self = AS_INT(_self);

  int len = snprintf(NULL, 0, "%d", self);
  char *result = GC_MALLOC(len + 1);
  snprintf(result, len + 1, "%d", self);

  String* s = GC_MALLOC(sizeof(String));
  s->size = len;
  s->chars = result;
  return MK_OBJ(s);
}

value_t prelude__Float__toString(value_t _self) {
  double self = AS_DOUBLE(_self);

  int len = snprintf(NULL, 0, "%f", self);
  char *result = GC_MALLOC(len + 1);
  snprintf(result, len + 1, "%f", self);

  String* s = GC_MALLOC(sizeof(String));
  s->size = len;
  s->chars = result;
  return MK_OBJ(s);
}

typedef struct Array {
  int32_t length;
  int32_t capacity;
  value_t* items;
} Array;

value_t alloc_array(int32_t length) {
  Array* array = GC_MALLOC(sizeof(Array));
  value_t* array_items = GC_MALLOC(sizeof(value_t) * length);

  array->length = length;
  array->capacity = length;
  array->items = array_items;
  return MK_OBJ(array);
}

void array_insert(value_t _self, value_t _idx, value_t item) {
  Array* self = AS_OBJ(_self, Array);
  int32_t idx = AS_INT(_idx);

  self->items[idx] = item;
}

value_t array_get(value_t _self, int32_t idx) {
  Array* self = AS_OBJ(_self, Array);

  return self->items[idx];
}

#endif
