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

#define TODO(m)                                         \
  do {                                                  \
    printf("%s:%d: TODO: " m "\n", __FILE__, __LINE__); \
    exit(1);                                            \
  } while (0);
#define UNREACHABLE                                                  \
  do {                                                               \
    printf("%s:%d: Entered unreachable code\n", __FILE__, __LINE__); \
    exit(1);                                                         \
  } while (0);

typedef enum {
  OBJ_STR = 0,
  OBJ_ARRAY,
} ObjectType;
typedef struct Obj {
  ObjectType type;
} Obj;

typedef enum {
  ABRA_TYPE_NONE = 0,
  ABRA_TYPE_INT,
  ABRA_TYPE_FLOAT,
  ABRA_TYPE_BOOL,
  ABRA_TYPE_OBJ,
} AbraType;
typedef struct AbraValue {
  AbraType type;
  union {
    int64_t abra_int;
    double abra_float;
    bool abra_bool;
    Obj* obj;
  } as;
} AbraValue;

static AbraValue ABRA_NONE = {.type = ABRA_TYPE_NONE };
#define IS_NONE(v) (v.type == ABRA_TYPE_NONE)

static AbraValue ABRA_TRUE = {.type = ABRA_TYPE_BOOL, .as = {.abra_bool = true } };
static AbraValue ABRA_FALSE = {.type = ABRA_TYPE_BOOL, .as = {.abra_bool = false } };

#define NEW_INT(i) ((AbraValue){.type = ABRA_TYPE_INT, .as = {.abra_int = i}})
#define AS_INT(v) v.as.abra_int
#define NEW_FLOAT(f) ((AbraValue){.type = ABRA_TYPE_FLOAT, .as = {.abra_float = f}})
#define AS_FLOAT(v) v.as.abra_float
#define NEW_BOOL(b) (b ? ABRA_TRUE : ABRA_FALSE)
#define AS_BOOL(v) v.as.abra_bool

typedef struct AbraString {
  Obj _header;
  uint32_t size;
  char* data;
} AbraString;

AbraValue alloc_string(char* data, size_t size) {
  AbraString* str = GC_MALLOC(sizeof(AbraString));

  str->_header.type = OBJ_STR;
  str->size = size;
  if (size != 0 && data != NULL) {
    str->data = strdup(data);
  }
  str->data[size] = 0;

  return ((AbraValue){.type = ABRA_TYPE_OBJ, .as = {.obj = ((Obj*)str)}});
}

AbraValue std_string__concat(AbraValue str1, AbraValue str2) {
  AbraString* s1 = (AbraString*)str1.as.obj;
  AbraString* s2 = (AbraString*)str2.as.obj;

  size_t new_str_len = s1->size + s2->size + 1;
  char* tmp = malloc(new_str_len);
  memcpy(tmp, s1->data, s1->size);
  memcpy(tmp + s1->size, s2->data, s2->size);
  tmp[new_str_len] = 0;

  size_t size = s1->size + s2->size;
  AbraValue new_string = alloc_string(tmp, size);
  free(tmp);
  return new_string;
}

typedef struct AbraArray {
  Obj _header;
  uint32_t size;
  uint32_t capacity;
  AbraValue* items;
} AbraArray;

AbraValue alloc_array(AbraValue* values, size_t size) {
  AbraArray* arr = GC_MALLOC(sizeof(AbraArray));

  arr->_header.type = OBJ_ARRAY;
  arr->size = size;
  arr->capacity = size;
  arr->items = values;

  return ((AbraValue){.type = ABRA_TYPE_OBJ, .as = {.obj = ((Obj*)arr)}});
}

char const* std__to_string(AbraValue val) {
  switch (val.type) {
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
      switch (o->type) {
        case OBJ_STR:
          return ((AbraString*)o)->data;
        case OBJ_ARRAY: {
          AbraArray* arr = (AbraArray*)o;

          if (arr->size == 0) {
              return "[]";
          }

          // Convert each element to string and compute total size
          char const** items = malloc(sizeof(char*) * arr->size);
          size_t size = 2 + (2 * (arr->size - 1));  // Account for "[" and "]", plus ", " for all but last item
          for (int i = 0; i < arr->size; ++i) {
            AbraValue item = arr->items[i];
            char const* item_str = std__to_string(item);
            items[i] = item_str;
            size += strlen(item_str);
          }

          // Allocate necessary string and copy items' strings into place
          char* array_str = GC_MALLOC(sizeof(char) * size);
          array_str[0] = '[';
          char* ptr = array_str + 1;
          for (int i = 0; i < arr->size; ++i) {
            char const* item_str = items[i];
            size_t len = strlen(item_str);
            memcpy(ptr, item_str, len);
            ptr += len;
            if (i < arr->size - 1) {
              memcpy(ptr, ", ", 2);
              ptr += 2;
            }
          }
          array_str[size - 1] = ']';
          array_str[size] = 0;

          free(items);

          return array_str;
        }
        default:
          TODO("Implement other Obj println cases")
      }
    } break;
    default:
      UNREACHABLE // All the primitive types have been handled
  }
}

void std__println(AbraValue val) {
  if (IS_NONE(val)) {
    printf("\n");
    return;
  }

  AbraArray* varargs = (AbraArray*)val.as.obj;
  for (int i = 0; i < varargs->size; ++i) {
    printf("%s", std__to_string(varargs->items[i]));
    if (i < varargs->size - 1) {
      printf(" ");
    }
  }
  printf("\n");
}

bool std__eq(AbraValue v1, AbraValue v2) {
    if (v1.type != v2.type) return false;
    switch (v1.type) {
        case ABRA_TYPE_NONE: return true;
        case ABRA_TYPE_INT: return v1.as.abra_int == v2.as.abra_int;
        case ABRA_TYPE_FLOAT: return v1.as.abra_float == v2.as.abra_float;
        case ABRA_TYPE_BOOL: return v1.as.abra_bool == v2.as.abra_bool;
        case ABRA_TYPE_OBJ: {
            Obj* o1 = v1.as.obj;
            Obj* o2 = v2.as.obj;
            if (o1->type != o2->type) return false;

            switch (o1->type) {
                case OBJ_STR: {
                    AbraString* s1 = (AbraString*) o1;
                    AbraString* s2 = (AbraString*) o2;
                    if (s1->size != s2->size) return false;

                    for (int i = 0; i < s1->size; ++i) {
                        if (s1->data[i] != s2->data[i]) return false;
                    }
                    return true;
                }
                case OBJ_ARRAY: {
                    AbraArray* a1 = (AbraArray*) o1;
                    AbraArray* a2 = (AbraArray*) o2;
                    if (a1->size != a2->size) return false;

                    for (int i = 0; i < a1->size; ++i) {
                        if (!std__eq(a1->items[i], a2->items[i])) return false;
                    }
                    return true;
                }
            }
        }
    }

    return true;
}

#endif
