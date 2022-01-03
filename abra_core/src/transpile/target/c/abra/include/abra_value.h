#ifndef __ABRA_VALUE_H
#define __ABRA_VALUE_H

typedef enum {
  OBJ_STR = 0,
  OBJ_ARRAY,
  OBJ_TUPLE,
  OBJ_MAP,
  OBJ_SET,
  OBJ_FUNCTION,
  OBJ_INSTANCE,
  _OBJECT_TYPE_END
} ObjectType;
typedef struct Obj {
  ObjectType type;
  size_t type_id;
} Obj;

// Predefined type_ids for builtins. Note that additional types will be assigned
// a type_id at runtime, incrementing from __next_type_id. Note also that there is
// a gap between the predefined type_ids and the start of the auto-incrementing
// type_ids. This is okay, as the two are actually unrelated; the important thing
// about the auto-incrementing type_ids is that their entries in
// eq_fns/to_string_fns/hash_fns begins at OBJ_INSTANCE.
size_t std__Int__type_id = 0;
size_t std__Float__type_id = 1;
size_t std__Bool__type_id = 2;
size_t std__String__type_id = 3;
static size_t __next_type_id = _OBJECT_TYPE_END - 1;

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
#define IS_FALSY(v) (IS_NONE(v) || (v.type == ABRA_TYPE_BOOL && !AS_BOOL(v)))

#define NEW_INT(i) ((AbraValue){.type = ABRA_TYPE_INT, .as = {.abra_int = i}})
#define AS_INT(v) v.as.abra_int
#define NEW_FLOAT(f) ((AbraValue){.type = ABRA_TYPE_FLOAT, .as = {.abra_float = f}})
#define AS_FLOAT(v) v.as.abra_float
#define NEW_BOOL(b) (b ? ABRA_TRUE : ABRA_FALSE)
#define AS_BOOL(v) v.as.abra_bool
#define AS_OBJ(v) (v.as.obj)

// TODO: Proper .h/.c file split. Precompile .c's and statically link
typedef struct AbraString {
  Obj _header;
  uint32_t size;
  char* data;
} AbraString;

AbraValue alloc_string(char* data, size_t size) {
  AbraString* str = GC_MALLOC(sizeof(AbraString));

  str->_header.type = OBJ_STR;
  str->size = size;
  if (data != NULL) {
    str->data = strdup(data);
  }
  str->data[size] = 0;

  return ((AbraValue){.type = ABRA_TYPE_OBJ, .as = {.obj = ((Obj*)str)}});
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

#endif
