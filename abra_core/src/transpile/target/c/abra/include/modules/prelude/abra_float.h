#ifndef __ABRA_FLOAT_H
#define __ABRA_FLOAT_H

#include "../../abra_value.h"
#include "math.h"

// toString(): String
AbraValue ABRA_METHOD_NAME(std, Float, toString)(void* _env, AbraValue _self) {
  char* str = (char*) std__to_string(_self);
  // No need to free str, since it's GC_MALLOC'd
  return alloc_string(str, strlen(str));
}

// floor(): Int
AbraValue ABRA_METHOD_NAME(std, Float, floor)(void* _env, AbraValue _self) {
  int64_t val = (int64_t) floor(AS_FLOAT(_self));
  return NEW_INT(val);
}

// ceil(): Int
AbraValue ABRA_METHOD_NAME(std, Float, ceil)(void* _env, AbraValue _self) {
  int64_t val = (int64_t) ceil(AS_FLOAT(_self));
  return NEW_INT(val);
}

// round(): Int
AbraValue ABRA_METHOD_NAME(std, Float, round)(void* _env, AbraValue _self) {
  int64_t val = (int64_t) round(AS_FLOAT(_self));
  return NEW_INT(val);
}

// withPrecision(precision: Int): Float
AbraValue ABRA_METHOD_NAME(std, Float, withPrecision)(void* _env, AbraValue _self, AbraValue _precision) {
  int64_t precision = AS_INT(_precision);
  if (precision < 0) return _self;
  if (precision == 0) {
    return NEW_FLOAT(round(AS_FLOAT(_self)));
  }

  int64_t factor = pow(10, precision);
  double val = round(AS_FLOAT(_self) * factor) / factor;
  return NEW_FLOAT(val);
}

// abs(): Float
AbraValue ABRA_METHOD_NAME(std, Float, abs)(void* _env, AbraValue _self) {
  return NEW_FLOAT(fabs(AS_FLOAT(_self)));
}

#endif
