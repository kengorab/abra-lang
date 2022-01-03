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
#include "callable.h"

#include "abra_int.h"
#include "abra_float.h"
#include "abra_string.h"
#include "abra_array.h"
#include "abra_tuple.h"
#include "abra_map.h"
#include "abra_set.h"
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
      if (o1->type == OBJ_INSTANCE) {
          if (o1->type_id != o2->type_id) return false;
          // o1->type_id will be >= OBJ_INSTANCE
          return eq_fns[o1->type_id](o1, o2);
      }
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
      Obj* o = AS_OBJ(val);
      if (o->type == OBJ_INSTANCE) {
          // o->type_id will be >= OBJ_INSTANCE
          return to_string_fns[o->type_id](o);
      }
      return to_string_fns[o->type](o);
    }
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
      Obj* o = AS_OBJ(val);
      if (o->type == OBJ_INSTANCE) {
          // o->type_id will be >= OBJ_INSTANCE
          return hash_fns[o->type_id](o);
      }
      return hash_fns[o->type](o);
    }
  }
}

// print(*items: Any[])
AbraValue std__print(void* _env, AbraValue val) {
  if (IS_NONE(val)) return ABRA_NONE;

  AbraArray* varargs = (AbraArray*)AS_OBJ(val);
  for (int i = 0; i < varargs->size; ++i) {
    printf("%s", std__to_string(varargs->items[i]));
    if (i < varargs->size - 1) {
      printf(" ");
    }
  }
  return ABRA_NONE;
}
AbraValue std__print_val;

// println(*items: Any[])
AbraValue std__println(void* _env, AbraValue val) {
  std__print(NULL, val);
  printf("\n");
  return ABRA_NONE;
}
AbraValue std__println_val;

// range(from: Int, to: Int, increment?: Int): Int[]
AbraValue std__range(void* _env, AbraValue _from, AbraValue _to, AbraValue _increment) {
    int64_t from = AS_INT(_from);
    int64_t to = AS_INT(_to);
    int64_t increment = IS_NONE(_increment) ? 1 : AS_INT(_increment);

    if (to <= from) {
        AbraValue* array_items = GC_MALLOC(sizeof(AbraValue) * 0);
        return alloc_array(array_items, 0);
    }

    size_t size = (to - from) / increment;
    AbraValue* array_items = GC_MALLOC(sizeof(AbraValue) * size);
    size_t idx = 0;
    while (from < to) {
        array_items[idx++] = NEW_INT(from);
        from += increment;
    }

    return alloc_array(array_items, size);
}
AbraValue std__range_val;

// Coalesce operator (`?:`) implementation
AbraValue std_option__coalesce(AbraValue lhs, AbraValue rhs) {
    return IS_NONE(lhs) ? rhs : lhs;
}

bool std_type_is(AbraValue val, size_t type_id) {
    if (val.type == ABRA_TYPE_INT) return type_id == std__Int__type_id;
    if (val.type == ABRA_TYPE_FLOAT) return type_id == std__Float__type_id;
    if (val.type == ABRA_TYPE_BOOL) return type_id == std__Bool__type_id;
    if (val.type == ABRA_TYPE_OBJ) {
        Obj* o = AS_OBJ(val);
        if (o->type == OBJ_STR) return type_id == std__String__type_id;
        if (o->type == OBJ_INSTANCE) return o->type_id == type_id;

        printf("Unknown type_id %zu\n", type_id);
        return false;
    }
    return false;
}

bool std_type_is_tuple(AbraValue val) {
    return val.type == ABRA_TYPE_OBJ && AS_OBJ(val)->type == OBJ_TUPLE;
}

void abra_init() {
  GC_INIT();

  // Bind `print` builtin fn
  callable_ctx__1_t* std__print_val_ctx = GC_MALLOC(sizeof(callable_ctx__1_t));
  std__print_val_ctx->fn = &std__print;
  std__print_val = alloc_function("print", "std__print", (void*) std__print_val_ctx);

  // Bind `println` builtin fn
  callable_ctx__1_t* std__println_val_ctx = GC_MALLOC(sizeof(callable_ctx__1_t));
  std__println_val_ctx->fn = &std__println;
  std__println_val = alloc_function("println", "std__println", (void*) std__println_val_ctx);

  // Bind `range` builtin fn
  callable_ctx__3_t* std__range_val_ctx = GC_MALLOC(sizeof(callable_ctx__3_t));
  std__range_val_ctx->fn = &std__range;
  std__range_val = alloc_function("range", "std__range", (void*) std__range_val_ctx);

  // Bind eq functions for primitive types
  eq_fns[OBJ_STR] = &std_string__eq;
  eq_fns[OBJ_ARRAY] = &std_array__eq;
  eq_fns[OBJ_TUPLE] = &std_tuple__eq;
  eq_fns[OBJ_MAP] = &std_map__eq;
  eq_fns[OBJ_SET] = &std_set__eq;
  eq_fns[OBJ_FUNCTION] = &std_function__eq;

  // Bind toString functions for primitive types
  to_string_fns[OBJ_STR] = &std_string__to_string;
  to_string_fns[OBJ_ARRAY] = &std_array__to_string;
  to_string_fns[OBJ_TUPLE] = &std_tuple__to_string;
  to_string_fns[OBJ_MAP] = &std_map__to_string;
  to_string_fns[OBJ_SET] = &std_set__to_string;
  to_string_fns[OBJ_FUNCTION] = &std_function__to_string;

  // Bind hash functions for primitive types
  hash_fns[OBJ_STR] = &std_string__hash;
  hash_fns[OBJ_ARRAY] = &std_array__hash;
  hash_fns[OBJ_TUPLE] = &std_tuple__hash;
  hash_fns[OBJ_MAP] = &std_map__hash;
  hash_fns[OBJ_SET] = &std_set__hash;
  hash_fns[OBJ_FUNCTION] = &std_function__hash;
}

#endif
