#ifndef __ABRA_INT_H
#define __ABRA_INT_H

#include "abra_value.h"
#include "math.h"
#include "string.h"

// abs(): Int
AbraValue std_int__method_abs(void* _env, AbraValue _self) {
  int64_t self = AS_INT(_self);
  return NEW_INT(llabs(self));
}

char char_from_digit(int num, int base) {
  if (num >= base) return '\0';
  if (num < 10) {
    return '0' + num;
  } else {
    return 'a' + num - 10;
  }
}

// asBase(base: Int): String
AbraValue std_int__method_asBase(void* _env, AbraValue _self, AbraValue _base) {
  int64_t self = AS_INT(_self);
  int64_t base = AS_INT(_base);

  if (base <= 1 || base >= 37 || self <= 0) {
    char* str = (char*) std__to_string(_self);
    size_t len = strlen(str);
    return alloc_string(str, len);
  }

  char* str = malloc(sizeof(char) * 65); // length of int64_t max in binary + 1 for null terminator
  size_t count = 0;
  while (self > 0) {
    char c = char_from_digit(self % base, base);
    str[count++] = c;
    self = self / base;
  }
  for (int i = 0; i < count / 2; ++i) {
    char tmp = str[i];
    str[i] = str[count - 1 - i];
    str[count - 1 - i] = tmp;
  }
  return alloc_string(str, count);
}

// isEven(): Bool
AbraValue std_int__method_isEven(void* _env, AbraValue _self) {
  return AS_INT(_self) % 2 == 0 ? ABRA_TRUE : ABRA_FALSE;
}

// isOdd(): Bool
AbraValue std_int__method_isOdd(void* _env, AbraValue _self) {
  return AS_INT(_self) % 2 == 0 ? ABRA_FALSE : ABRA_TRUE;
}

// isBetween(lower: Int, upper: Int, inclusive?: Bool): Bool
AbraValue std_int__method_isBetween(void* _env, AbraValue _self, AbraValue _lower, AbraValue _upper, AbraValue _inclusive) {
  int64_t self = AS_INT(_self);
  int64_t upper = AS_INT(_upper);
  int64_t lower = AS_INT(_lower);
  bool inclusive = IS_NONE(_inclusive) ? false : AS_BOOL(_inclusive);

  bool is_between;
  if (inclusive) {
    is_between = lower <= self && self <= upper;
  } else {
    is_between = lower < self && self < upper;
  }
  return NEW_BOOL(is_between);
}

#endif
