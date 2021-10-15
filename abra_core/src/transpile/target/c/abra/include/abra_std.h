#ifndef __ABRA_H
#define __ABRA_H

#define GC_DEBUG

#include "math.h"
#include "stdbool.h"
#include "stdint.h"
#include "inttypes.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "gc.h"

#define TODO(m)                                         \
  do {                                                  \
    printf("%s:%d: TODO: " m "\n", __FILE__, __LINE__); \
    exit(1);                                            \
  } while (0);

typedef enum {
  OBJ_STR,
} ObjectType;

typedef struct Obj {
  ObjectType type;
} Obj;

#define ABRA_TYPE_INT 0
#define ABRA_TYPE_FLOAT 1
#define ABRA_TYPE_BOOL 2
#define ABRA_TYPE_OBJ 3
typedef struct AbraValue {
  int8_t type_id;
  union {
    int64_t abra_int;
    double abra_float;
    bool abra_bool;
    Obj* obj;
  } as;
} AbraValue;

#define NEW_INT(i) ((AbraValue){.type_id = ABRA_TYPE_INT, .as = {.abra_int = i}})
#define AS_INT(v) v.as.abra_int
#define NEW_FLOAT(f) ((AbraValue){.type_id = ABRA_TYPE_FLOAT, .as = {.abra_float = f}})
#define AS_FLOAT(v) v.as.abra_float
#define NEW_BOOL(b) ((AbraValue){.type_id = ABRA_TYPE_BOOL, .as = {.abra_bool = b}})
#define AS_BOOL(v) v.as.abra_bool

typedef struct AbraString {
  Obj _header;
  uint32_t size;
  char data[];
} AbraString;

AbraString* alloc_string(char* data, size_t size) {
    AbraString* str = GC_MALLOC(sizeof(AbraString));

    str->_header.type = OBJ_STR;
    str->size = size;
    if (size != 0 && data != NULL) memcpy(str->data, data, size);

    return str;
}

void std__println(AbraValue val) {
  switch (val.type_id) {
    case ABRA_TYPE_INT:
      printf("%" PRId64 "\n", val.as.abra_int);
      break;
    case ABRA_TYPE_FLOAT:
      printf("%f\n", val.as.abra_float);
      break;
    case ABRA_TYPE_BOOL:
      if (val.as.abra_bool) {
        printf("true\n");
      } else {
        printf("false\n");
      }
      break;
    default:
      TODO("Other println cases")
  }
}

#endif
