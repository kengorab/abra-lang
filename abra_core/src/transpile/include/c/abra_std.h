#ifndef __ABRA_H
#define __ABRA_H

#include "math.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"

#define TODO(m)              \
  do {                       \
    printf("TODO: " m "\n"); \
    exit(1);                 \
  } while (0);

// Maybe eventually replace with nan-tagging?
#define ABRA_TYPE_INT 0
#define ABRA_TYPE_FLOAT 1
typedef struct AbraValue {
  int8_t type_id;
  union {
    int64_t abra_int;
    double abra_float;
  } as;
} AbraValue;

#define NEW_INT(i) ((AbraValue){.type_id = ABRA_TYPE_INT, .as = {.abra_int = i}})
#define AS_INT(v) v.as.abra_int
#define NEW_FLOAT(f) ((AbraValue){.type_id = ABRA_TYPE_FLOAT, .as = {.abra_float = f}})
#define AS_FLOAT(v) v.as.abra_float

void std__println(AbraValue val) {
  switch (val.type_id) {
    case ABRA_TYPE_INT:
      printf("%lld\n", val.as.abra_int);
      break;
    case ABRA_TYPE_FLOAT:
      printf("%f\n", val.as.abra_float);
      break;
    default:
      TODO("Other println cases")
  }
}

#endif
