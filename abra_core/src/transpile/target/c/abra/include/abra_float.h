#ifndef __ABRA_FLOAT_H
#define __ABRA_FLOAT_H

#include "abra_value.h"
#include "math.h"

// floor(): Int
AbraValue std_float__method_floor(void* _env, AbraValue _self) {
  int64_t val = (int64_t) floor(AS_FLOAT(_self));
  return NEW_INT(val);
}

// ceil(): Int
AbraValue std_float__method_ceil(void* _env, AbraValue _self) {
  int64_t val = (int64_t) ceil(AS_FLOAT(_self));
  return NEW_INT(val);
}

// round(): Int
AbraValue std_float__method_round(void* _env, AbraValue _self) {
  int64_t val = (int64_t) round(AS_FLOAT(_self));
  return NEW_INT(val);
}

// withPrecision(precision: Int): Float
AbraValue std_float__method_withPrecision(void* _env, AbraValue _self, AbraValue _precision) {
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
AbraValue std_float__method_abs(void* _env, AbraValue _self) {
  return NEW_FLOAT(fabs(AS_FLOAT(_self)));
}

#endif
