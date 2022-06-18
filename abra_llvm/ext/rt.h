#ifndef __ABRA_RT_H
#define __ABRA_RT_H

#include "stdarg.h"
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

const uint64_t VAL_NONE  = MASK_NAN | (uint64_t)0x0001000000000000;
const uint64_t VAL_FALSE = MASK_NAN | (uint64_t)0x0001000000000001;
const uint64_t VAL_TRUE  = MASK_NAN | (uint64_t)0x0001000000000002;

const uint64_t PAYLOAD_MASK_INT = (uint64_t)0x00000000ffffffff;
const uint64_t PAYLOAD_MASK_OBJ = (uint64_t)0x0000ffffffffffff;

#define AS_INT(val)    ((int32_t) (val & PAYLOAD_MASK_INT))
#define AS_DOUBLE(val) ((double) (value_t_to_double(val)))

#define AS_OBJ(val, typ)  ((typ*)(val & PAYLOAD_MASK_OBJ))
#define TAG_OBJ(val)      (MASK_OBJ | (uint64_t)val)

// ------------------------------------------------------------------------

value_t* vtable[256];
uint32_t next_type_id = 0;

value_t* vtable_alloc_entry(uint32_t type_id, uint32_t size) {
  value_t* entry = GC_MALLOC(sizeof(value_t) * size);
  vtable[type_id] = entry;
  return entry;
}

value_t vtable_lookup(uint32_t type_id, uint32_t idx) {
  return vtable[type_id][idx];
}

typedef struct obj_header_t {
  uint32_t type_id;
} obj_header_t;

uint32_t type_id_Int;
uint32_t type_id_Float;
uint32_t type_id_Bool;

uint32_t type_id_for_val(value_t value) {
  if ((value & MASK_INT) == MASK_INT) {
    return type_id_Int;
  }
  if ((value & MASK_NAN) != MASK_NAN) {
    return type_id_Float;
  }
  if (value == VAL_TRUE || value == VAL_FALSE) {
    return type_id_Bool;
  }

  obj_header_t* header = AS_OBJ(value, obj_header_t);
  return header->type_id;
}

uint32_t type_id_String;
typedef struct String {
  obj_header_t h;
  int32_t size;
  char* chars;
} String;

value_t string_alloc(int32_t length, char* chars) {
  String* string = GC_MALLOC(sizeof(String));

  string->h.type_id = type_id_String;
  string->size = length;
  string->chars = chars;
  return TAG_OBJ(string);
}

value_t value_to_string(value_t value) {
  if (value == VAL_NONE) {
    return string_alloc(4, "None");
  }

  uint32_t type_id = type_id_for_val(value);
  value_t(*tostring_method)(value_t) = (value_t(*)(value_t))vtable_lookup(type_id, 0);
  return tostring_method(value);
}

value_t string_concat(value_t _s1, value_t _s2) {
  String* s1 = AS_OBJ(value_to_string(_s1), String);
  String* s2 = AS_OBJ(value_to_string(_s2), String);

  char* chars = GC_MALLOC(sizeof(char) * (s1->size + s2->size));
  memcpy(chars, s1->chars, s1->size);
  memcpy(chars + s1->size, s2->chars, s2->size);
  return string_alloc(s1->size + s2->size, chars);
}

void range_endpoints(int32_t len, int32_t* start, int32_t* end) {
  if (*start < 0) {
    *start += len;
  } else if (len == 0) {
    *start = 0;
  }

  if (*end < 0) {
    *end += len;
  } else if (*end < *start) {
    *end = *start;
  } else if (*end >= len) {
    *end = len;
  }
}

value_t string_get(value_t _self, int32_t idx) {
  String* self = AS_OBJ(_self, String);

  if (idx < -self->size || idx >= self->size) return VAL_NONE;

  char* s = GC_MALLOC(sizeof(char) * 1);
  s[0] = idx < 0 ? self->chars[idx + self->size] : self->chars[idx];
  return string_alloc(1, s);
}

value_t string_range(value_t _self, value_t _start, value_t _end) {
  String* self = AS_OBJ(_self, String);
  int32_t start = _start == VAL_NONE ? 0 : AS_INT(_start);
  int32_t end = _end == VAL_NONE ? self->size : AS_INT(_end);
  range_endpoints(self->size, &start, &end);

  if (start >= end) return string_alloc(0, "");

  int32_t slice_size = end - start;
  char* tmp = GC_MALLOC(sizeof(char) * slice_size);
  memcpy(tmp, self->chars + (start * sizeof(char)), slice_size);

  return string_alloc(slice_size, tmp);
}

value_t prelude__String__toString(value_t self) {
  return self;
}

value_t prelude__String__toLower(value_t _self) {
  String* self = AS_OBJ(_self, String);

  char* chars = GC_MALLOC(sizeof(char) * self->size);
  for (int i = 0; i < self->size; ++i) {
    char ch = self->chars[i];
    chars[i] = (ch >= 'A' && ch <= 'Z') ? ch + 32 : ch;
  }

  return string_alloc(self->size, chars);
}

value_t prelude__String__toUpper(value_t _self) {
  String* self = AS_OBJ(_self, String);

  char* chars = GC_MALLOC(sizeof(char) * self->size);
  for (int i = 0; i < self->size; ++i) {
    char ch = self->chars[i];
    chars[i] = (ch >= 'a' && ch <= 'z') ? ch - 32 : ch;
  }

  return string_alloc(self->size, chars);
}

value_t prelude__Int__toString(value_t _self) {
  int32_t self = AS_INT(_self);

  int len = snprintf(NULL, 0, "%d", self);
  char *result = GC_MALLOC(len + 1);
  snprintf(result, len + 1, "%d", self);

  return string_alloc(len, result);
}

value_t prelude__Float__toString(value_t _self) {
  double self = AS_DOUBLE(_self);

  int len = snprintf(NULL, 0, "%f", self);
  char *result = GC_MALLOC(len + 1);
  snprintf(result, len + 1, "%f", self);

  return string_alloc(len, result);
}

value_t prelude__Bool__toString(value_t self) {
  if (self == VAL_TRUE) {
    return string_alloc(4, "true");
  }
  return string_alloc(5, "false");
}

uint32_t type_id_Array;
typedef struct Array {
  obj_header_t h;
  int32_t length;
  int32_t capacity;
  value_t* items;
} Array;

value_t array_alloc(int32_t length) {
  Array* array = GC_MALLOC(sizeof(Array));
  value_t* array_items = GC_MALLOC(sizeof(value_t) * length);

  array->h.type_id = type_id_Array;
  array->length = length;
  array->capacity = length;
  array->items = array_items;
  return TAG_OBJ(array);
}

void array_insert(value_t _self, value_t _idx, value_t item) {
  Array* self = AS_OBJ(_self, Array);
  int32_t idx = AS_INT(_idx);

  self->items[idx] = item;
}

value_t array_get(value_t _self, int32_t idx) {
  Array* self = AS_OBJ(_self, Array);

  if (idx < -self->length || idx >= self->length) return VAL_NONE;
  if (idx < 0) return self->items[idx + self->length];
  return self->items[idx];
}

value_t array_range(value_t _self, value_t _start, value_t _end) {
  Array* self = AS_OBJ(_self, Array);
  int32_t start = _start == VAL_NONE ? 0 : AS_INT(_start);
  int32_t end = _end == VAL_NONE ? self->length : AS_INT(_end);
  range_endpoints(self->length, &start, &end);

  if (start >= end) return array_alloc(0);

  int32_t slice_size = end - start;
  value_t sub_array_v = array_alloc(slice_size);
  Array* sub_array = AS_OBJ(sub_array_v, Array);
  memcpy(sub_array->items, self->items + start, slice_size * sizeof(value_t));

  return sub_array_v;
}

value_t values_to_string(
  int32_t length,
  value_t* values,
  int32_t prefix_len, char* prefix,
  int32_t suffix_len, char* suffix,
  int32_t sep_len, char* sep
) {
  if (length == 0) {
    char* empty_str = GC_MALLOC(sizeof(char) * (prefix_len + suffix_len));
    memcpy(empty_str, prefix, prefix_len);
    memcpy(empty_str + prefix_len, suffix, suffix_len);
    return string_alloc(prefix_len + suffix_len, empty_str);
  }

  String** strings = GC_MALLOC(sizeof(String*) * length);
  int32_t total_length = prefix_len + suffix_len;
  for (int i = 0; i < length; i++) {
    value_t str_val = value_to_string(values[i]);
    String* s = AS_OBJ(str_val, String);
    strings[i] = s;
    total_length += s->size;
    if (i != length - 1) {
      total_length += sep_len;
    }
  }

  char* chars = GC_MALLOC(sizeof(char) * total_length);
  memcpy(chars, prefix, prefix_len);
  int32_t offset = prefix_len;
  for (int i = 0; i < length; i++) {
    memcpy(chars + offset, strings[i]->chars, strings[i]->size);
    offset += strings[i]->size;

    if (i != length - 1) {
      memcpy(chars + offset, sep, sep_len);
      offset += sep_len;
    }
  }
  memcpy(chars + offset, suffix, suffix_len);

  return string_alloc(total_length, chars);
}

value_t prelude__Array__toString(value_t _self) {
  Array* self = AS_OBJ(_self, Array);
  return values_to_string(self->length, self->items, 1, "[", 1, "]", 2, ", ");
}

uint32_t type_id_Tuple;
typedef struct Tuple {
  obj_header_t h;
  int32_t length;
  value_t* items;
} Tuple;

value_t tuple_alloc(int32_t length, ...) {
  Tuple* tuple = GC_MALLOC(sizeof(Tuple));
  value_t* tuple_items = GC_MALLOC(sizeof(value_t) * length);

  va_list ptr;
  va_start(ptr, length);
  for (int i = 0; i < length; i++) {
    tuple_items[i] = va_arg(ptr, value_t);
  }
  va_end(ptr);

  tuple->h.type_id = type_id_Tuple;
  tuple->length = length;
  tuple->items = tuple_items;
  return TAG_OBJ(tuple);
}

value_t tuple_get(value_t _self, int32_t idx) {
  Tuple* self = AS_OBJ(_self, Tuple);

  if (idx < -self->length || idx >= self->length) return VAL_NONE;
  if (idx < 0) return self->items[idx + self->length];
  return self->items[idx];
}

value_t prelude__Tuple__toString(value_t _self) {
  Tuple* self = AS_OBJ(_self, Tuple);
  return values_to_string(self->length, self->items, 1, "(", 1, ")", 2, ", ");
}

char* print_impl(value_t varargs) {
  if (varargs == VAL_NONE) return "";

  Array* args = AS_OBJ(varargs, Array);
  value_t str = values_to_string(args->length, args->items, 0, "", 0, "", 1, " ");
  String* s = AS_OBJ(str, String);
  return s->chars;
}

void prelude__print(value_t varargs) {
  printf("%s", print_impl(varargs));
}

void prelude__println(value_t varargs) {
  printf("%s\n", print_impl(varargs));
}

#endif
