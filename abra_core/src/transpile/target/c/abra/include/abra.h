#ifndef __ABRA_H
#define __ABRA_H

// To print debug information, run binary with GC_PRINT_STATS=1 env var
#define GC_DEBUG

#include "gc.h"
#include "inttypes.h"
#include "math.h"
#include "stdbool.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#include "abra_value.h"

// Forward-declare, they're referenced in subsequent includes
bool std__eq(AbraValue v1, AbraValue v2);
char const* std__to_string(AbraValue val);
size_t std__hash(AbraValue val);

#include "utils.h"
#include "hashmap.h"

#include "abra_string.h"
#include "abra_array.h"
#include "abra_tuple.h"
#include "abra_map.h"
#include "abra_function.h"

// Arbitrary limit for number of unique object types. This will need tuning
#define OBJ_LIMIT 100

// equality functions for builtin types
typedef bool (*EqFn)(Obj*, Obj*);
static EqFn eq_fns[OBJ_LIMIT];
bool std__eq(AbraValue v1, AbraValue v2) {
  if (v1.type == ABRA_TYPE_INT && v2.type == ABRA_TYPE_FLOAT)
    return ((double)v1.as.abra_int) == v2.as.abra_float;
  if (v1.type == ABRA_TYPE_FLOAT && v2.type == ABRA_TYPE_INT)
    return v1.as.abra_float == ((double)v2.as.abra_int);

  if (v1.type != v2.type) return false;
  switch (v1.type) {
    case ABRA_TYPE_NONE:
      return true;
    case ABRA_TYPE_INT:
      return v1.as.abra_int == v2.as.abra_int;
    case ABRA_TYPE_FLOAT:
      return v1.as.abra_float == v2.as.abra_float;
    case ABRA_TYPE_BOOL:
      return v1.as.abra_bool == v2.as.abra_bool;
    case ABRA_TYPE_OBJ: {
      Obj* o1 = v1.as.obj;
      Obj* o2 = v2.as.obj;
      if (o1->type != o2->type) return false;
      return eq_fns[o1->type](o1, o2);
    }
  }

  return true;
}

// toString functions for builtin types
typedef char const* (*ToStringFn)(Obj*);
static ToStringFn to_string_fns[OBJ_LIMIT];
char const* std__to_string(AbraValue val) {
  switch (val.type) {
    case ABRA_TYPE_NONE:
      return "None";
    case ABRA_TYPE_INT: {
      int64_t i = val.as.abra_int;
      int len = snprintf(NULL, 0, "%" PRId64, i);
      char* str = GC_MALLOC(len + 1);
      snprintf(str, len + 1, "%" PRId64, i);
      return str;
    }
    case ABRA_TYPE_FLOAT: {
      double d = val.as.abra_float;
      int len = snprintf(NULL, 0, "%f", d);
      char* str = GC_MALLOC(len + 1);
      snprintf(str, len + 1, "%f", d);
      // Trim trailing zeroes
      for (int i = len - 1; i >= 1; --i) {
        if (str[i] == '0' && str[i - 1] != '.') {
          str[i] = 0;
        } else {
          break;
        }
      }
      return str;
    }
    case ABRA_TYPE_BOOL:
      return val.as.abra_bool ? "true" : "false";
    case ABRA_TYPE_OBJ: {
      Obj* o = val.as.obj;
      return to_string_fns[o->type](o);
    } break;
    default:
      UNREACHABLE // All the primitive types have been handled
  }
}

// hash functions for builtin types
typedef size_t (*HashFn)(Obj*);
static HashFn hash_fns[OBJ_LIMIT];
size_t std__hash(AbraValue val) {
  switch (val.type) {
    case ABRA_TYPE_NONE: return 0;
    case ABRA_TYPE_INT: return val.as.abra_int * 719;
    case ABRA_TYPE_FLOAT: return (size_t) (val.as.abra_float * 1000000000000 * 839);
    case ABRA_TYPE_BOOL: return val.as.abra_bool ? 42643801 : 43112609;
    case ABRA_TYPE_OBJ: {
      Obj* o = val.as.obj;
      return hash_fns[o->type](o);
    }
  }
}

void std__print(AbraValue val) {
  if (IS_NONE(val)) {
    return;
  }

  AbraArray* varargs = (AbraArray*)val.as.obj;
  for (int i = 0; i < varargs->size; ++i) {
    printf("%s", std__to_string(varargs->items[i]));
    if (i < varargs->size - 1) {
      printf(" ");
    }
  }
}

void std__println(AbraValue val) {
  std__print(val);
  printf("\n");
}

void abra_init() {
  GC_INIT();

  eq_fns[OBJ_STR] = &std_string__eq;
  eq_fns[OBJ_ARRAY] = &std_array__eq;
  eq_fns[OBJ_TUPLE] = &std_tuple__eq;
  eq_fns[OBJ_MAP] = &std_map__eq;
  eq_fns[OBJ_FUNCTION] = &std_function__eq;

  to_string_fns[OBJ_STR] = &std_string__to_string;
  to_string_fns[OBJ_ARRAY] = &std_array__to_string;
  to_string_fns[OBJ_TUPLE] = &std_tuple__to_string;
  to_string_fns[OBJ_MAP] = &std_map__to_string;
  to_string_fns[OBJ_FUNCTION] = &std_function__to_string;

  hash_fns[OBJ_STR] = &std_string__hash;
  hash_fns[OBJ_ARRAY] = &std_array__hash;
  hash_fns[OBJ_TUPLE] = &std_tuple__hash;
  hash_fns[OBJ_MAP] = &std_map__hash;
  hash_fns[OBJ_FUNCTION] = &std_function__hash;
}

#endif
