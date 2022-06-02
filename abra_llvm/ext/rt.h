#ifndef __ABRA_RT_H
#define __ABRA_RT_H

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "stdbool.h"
#include "math.h"

typedef union {
    long long i64;
    double d;
} double_int64_transmute;

long long transmute_double_int64(double value) {
    double_int64_transmute t = {.d = value};
    return t.i64;
}

double transmute_int64_double(long long value) {
    double_int64_transmute t = {.i64 = value};
    return t.d;
}

//// NaN-tagging
//
//typedef uint64_t value_t;
//
//const uint64_t mask_nan = ((uint64_t)0x7ffc000000000000);
//const uint64_t val_none = mask_nan | (uint64_t)0x0001000000000000;
//const uint64_t val_false = mask_nan | (uint64_t)0x0001000000000001;
//const uint64_t val_true = mask_nan | (uint64_t)0x0001000000000002;
//
//
//inline value_t val_num(double v) { return (value_t) transmute_double_int64(v); }
//inline bool val_is_num(value_t v) { return (v & mask_nan) != mask_nan; }




// ---------------------------------------------

typedef struct String {
  int32_t size;
  char* chars;
} String;

String* prelude__String__toString(String* self) {
  return self;
}

String* prelude__String__toUpper(String* self) {
  char* chars = strdup(self->chars);
  for (int i = 0; i < self->size; ++i) {
    if (chars[i] >= 'a' && chars[i] <= 'z') {
      chars[i] = chars[i] - 32;
    }
  }

  String* s = GC_MALLOC(sizeof(String));
  s->size = self->size;
  s->chars = chars;
  return s;
}

String* prelude__Int__toString(int64_t self) {
  int len = snprintf(NULL, 0, "%lld", self);
  char *result = GC_MALLOC(len + 1);
  snprintf(result, len + 1, "%lld", self);

  String* s = GC_MALLOC(sizeof(String));
  s->size = len;
  s->chars = result;
  return s;
}

String* prelude__Float__toString(double self) {
  int len = snprintf(NULL, 0, "%f", self);
  char *result = GC_MALLOC(len + 1);
  snprintf(result, len + 1, "%f", self);

  String* s = GC_MALLOC(sizeof(String));
  s->size = len;
  s->chars = result;
  return s;
}

String* prelude__Bool__toString(bool self) {
  String* s = GC_MALLOC(sizeof(String));
  s->size = self ? 4 : 5;
  s->chars = self ? "true" : "false";
  return s;
}

#endif
