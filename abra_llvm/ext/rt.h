#ifndef __ABRA_RT_H
#define __ABRA_RT_H

#include "stdio.h"
#include "stdlib.h"
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

long long double_to_value_t(double value) {
    value_t_transmute t = {.d = value};
    return t.raw;
}

double value_t_to_double(long long value) {
    value_t_transmute t = {.raw = value};
    return t.d;
}


//const uint64_t MASK_NAN  =            (uint64_t)0x7ffc000000000000;
//const uint64_t VAL_NONE  = MASK_NAN | (uint64_t)0x0001000000000000;
//const uint64_t VAL_FALSE = MASK_NAN | (uint64_t)0x0001000000000001;
//const uint64_t VAL_TRUE  = MASK_NAN | (uint64_t)0x0001000000000002;
//
//value_t val_num(double d) { return (value_t) double_to_value_t(d); }
//bool val_is_num(value_t v) { return (v & MASK_NAN) != MASK_NAN; }




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
