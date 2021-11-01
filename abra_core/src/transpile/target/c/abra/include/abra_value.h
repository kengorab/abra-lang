#ifndef __ABRA_VALUE_H
#define __ABRA_VALUE_H

typedef enum {
  OBJ_STR = 0,
  OBJ_ARRAY,
  OBJ_TUPLE,
  OBJ_MAP,
  OBJ_SET,
  OBJ_FUNCTION,
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

static AbraValue ABRA_NONE = {.type = ABRA_TYPE_NONE};
#define IS_NONE(v) (v.type == ABRA_TYPE_NONE)

static AbraValue ABRA_TRUE = {.type = ABRA_TYPE_BOOL, .as = {.abra_bool = true}};
static AbraValue ABRA_FALSE = {.type = ABRA_TYPE_BOOL, .as = {.abra_bool = false}};

#define NEW_INT(i) ((AbraValue){.type = ABRA_TYPE_INT, .as = {.abra_int = i}})
#define AS_INT(v) v.as.abra_int
#define NEW_FLOAT(f) ((AbraValue){.type = ABRA_TYPE_FLOAT, .as = {.abra_float = f}})
#define AS_FLOAT(v) v.as.abra_float
#define NEW_BOOL(b) (b ? ABRA_TRUE : ABRA_FALSE)
#define AS_BOOL(v) v.as.abra_bool
#define AS_OBJ(v) (v.as.obj)

#endif
