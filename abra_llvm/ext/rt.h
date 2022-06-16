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

value_t* v_table[256];
uint32_t next_type_id = 0;

value_t* vtable_alloc_entry(uint32_t type_id, uint32_t size) {
  value_t* entry = GC_MALLOC(sizeof(value_t) * size);
  v_table[type_id] = entry;
  return entry;
}

value_t vtable_lookup(uint32_t type_id, uint32_t idx) {
  return v_table[type_id][idx];
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

  return self->items[idx];
}

value_t prelude__Array__toString(value_t _self) {
    Array* self = AS_OBJ(_self, Array);
    if (self->length == 0) {
        return string_alloc(2, "[]");
    }

    String** strings = GC_MALLOC(sizeof(String*) * self->length);
    int32_t total_length = 2;
    for (int i = 0; i < self->length; i++) {
        value_t str_val = value_to_string(self->items[i]);
        String* s = AS_OBJ(str_val, String);
        strings[i] = s;
        total_length += s->size;
        if (i != self->length - 1) {
            total_length += 2; // ', '
        }
    }

    char* chars = GC_MALLOC(sizeof(char) * total_length);
    chars[0] = '[';
    int32_t offset = 1;
    for (int i = 0; i < self->length; i++) {
        memcpy(chars + offset, strings[i]->chars, strings[i]->size);
        offset += strings[i]->size;

        if (i != self->length - 1) {
            memcpy(chars + offset, ", ", 2);
            offset += 2;
        }
    }
    chars[offset] = ']';

    return string_alloc(total_length, chars);
}

void prelude__println(value_t varargs) {
  if (varargs == VAL_NONE) {
      printf("\n");
      return;
  }

  Array* args = AS_OBJ(varargs, Array);

  String** strings = GC_MALLOC(sizeof(String*) * args->length);
  int32_t total_length = 0;
  for (int i = 0; i < args->length; i++) {
    value_t str_val = value_to_string(args->items[i]);
    String* s = AS_OBJ(str_val, String);
    strings[i] = s;
    if (i != args->length - 1) {
      total_length += 1; // ' '
    }
  }

  char* chars = GC_MALLOC(sizeof(char) * total_length);
  int32_t offset = 0;
  for (int i = 0; i < args->length; i++) {
    memcpy(chars + offset, strings[i]->chars, strings[i]->size);
    offset += strings[i]->size;

    if (i != args->length - 1) {
      memcpy(chars + offset, " ", 1);
      offset += 1;
    }
  }

  printf("%s\n", chars);
}

#endif
