#ifndef __ABRA_RT_H
#define __ABRA_RT_H

#include "stdarg.h"
#include "stdio.h"
#include "stdlib.h"
#include "stdint.h"
#include "string.h"
#include "stdbool.h"
#include "math.h"

// ------------------------ NAN TAGGING ------------------------
typedef uint64_t value_t;

const uint64_t MASK_NAN =            (uint64_t)0x7ffc000000000000;
const uint64_t MASK_INT = MASK_NAN | (uint64_t)0x0002000000000000;
const uint64_t MASK_OBJ = MASK_NAN | (uint64_t)0x8000000000000000;

const uint64_t VAL_NONE  = MASK_NAN | (uint64_t)0x0001000000000000;
const uint64_t VAL_FALSE = MASK_NAN | (uint64_t)0x0001000000000001;
const uint64_t VAL_TRUE  = MASK_NAN | (uint64_t)0x0001000000000002;

const uint64_t PAYLOAD_MASK_INT = (uint64_t)0x00000000ffffffff;
const uint64_t PAYLOAD_MASK_OBJ = (uint64_t)0x0000ffffffffffff;

#define AS_INT(val)    ((int32_t) (val & PAYLOAD_MASK_INT))
#define AS_DOUBLE(val) ((double) (value_t_to_double(val)))

#define FROM_BOOL(b) (b ? VAL_TRUE : VAL_FALSE)

#define IS_INT(val) ((val & MASK_INT) == MASK_INT)
#define IS_FLOAT(val) ((val & MASK_NAN) != MASK_NAN)
#define IS_OBJ(val) ((val & MASK_OBJ) == MASK_OBJ)

#define AS_OBJ(val, typ)  ((typ*)(val & PAYLOAD_MASK_OBJ))
#define TAG_OBJ(val)      (MASK_OBJ | (uint64_t)val)

typedef union {
  value_t raw;
  double d;
} value_t_transmute;

value_t double_to_value_t(double value) {
  value_t_transmute t = {.d = value};
  return t.raw;
}

double value_t_to_double(value_t value) {
  value_t_transmute t = {.raw = value};
  return t.d;
}

// ------------------------ VTABLE ------------------------
value_t* vtable[256];
value_t* vtable_alloc_entry(uint32_t type_id, uint32_t size);
value_t vtable_lookup(value_t value, uint32_t idx);

// ------------------------ INTRINSICS ------------------------
void range_endpoints(int32_t len, int32_t* start, int32_t* end);

value_t value_to_string(value_t value);
value_t values_to_string(
  int32_t length,
  value_t* values,
  int32_t prefix_len, char* prefix,
  int32_t suffix_len, char* suffix,
  int32_t sep_len, char* sep
);
value_t value_eq(value_t v1, value_t v2);

#define DBG(v) printf("`" #v "` => %s\n", AS_OBJ(value_to_string(v), String)->chars);

// ------------------------ TYPE MANAGEMENT ------------------------
const uint32_t TOSTRING_IDX = 0;
typedef value_t (*tostring_method_t)(value_t*, int8_t, value_t);
const uint32_t EQ_IDX = 1;
typedef value_t (*eq_method_t)(value_t*, int8_t, value_t, value_t);

uint32_t next_type_id = 0;
uint32_t type_id_for_val(value_t value);

typedef struct obj_header_t {
  uint32_t type_id;
} obj_header_t;

// ------------------------ INT ------------------------
uint32_t type_id_Int;
value_t prelude__Int__toString(value_t* _env, int8_t _num_rcv_args, value_t _self);

// ------------------------ FLOAT ------------------------
uint32_t type_id_Float;
value_t prelude__Float__toString(value_t* _env, int8_t _num_rcv_args, value_t _self);

// ------------------------ BOOL ------------------------
uint32_t type_id_Bool;
value_t prelude__Bool__toString(value_t* _env, int8_t _num_rcv_args, value_t self);

// ------------------------ STRING ------------------------
uint32_t type_id_String;
typedef struct String {
  obj_header_t h;
  int32_t size;
  char* chars;
} String;

// String utils
value_t string_alloc(int32_t length, char* chars);
value_t string_concat(value_t _s1, value_t _s2);
value_t string_get(value_t _self, int32_t idx);
value_t string_range(value_t _self, value_t _start, value_t _end);
value_t string_split(value_t _self, int32_t idx);

// String methods
value_t prelude__String__toString(value_t* _env, int8_t _num_rcv_args, value_t self);
value_t prelude__String__toLower(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__String__toUpper(value_t* _env, int8_t _num_rcv_args, value_t _self);

// ------------------------ ARRAY ------------------------
uint32_t type_id_Array;
typedef struct Array {
  obj_header_t h;
  int32_t length;
  int32_t capacity;
  value_t* items;
} Array;

// Array utils
value_t array_alloc(int32_t length);
void array_insert(value_t _self, value_t _idx, value_t item);
value_t array_get(value_t _self, int32_t idx);
value_t array_range(value_t _self, value_t _start, value_t _end);
value_t array_split(value_t _self, int32_t idx);

// Array methods
value_t prelude__Array__toString(value_t* _env, int8_t _num_rcv_args, value_t _self);

// ------------------------ TUPLE ------------------------
uint32_t type_id_Tuple;
typedef struct Tuple {
  obj_header_t h;
  int32_t length;
  value_t* items;
} Tuple;

// Tuple utils
value_t tuple_alloc(int32_t length, ...);
value_t tuple_get(value_t _self, int32_t idx);

// Tuple methods
value_t prelude__Tuple__toString(value_t* _env, int8_t _num_rcv_args, value_t _self);

// ------------------------ FUNCTION ------------------------
uint32_t type_id_Function;
typedef struct Function {
  obj_header_t h;
  char* name;
  value_t fn_ptr;
  value_t* env;
} Function;

// Function utils
value_t function_alloc(char* name, value_t fn_ptr, value_t* env);

// Function methods
value_t prelude__Function__toString(value_t* _env, int8_t _num_rcv_args, value_t _self);

// ------------------------ PRELUDE FUNCTIONS ------------------------
void prelude__print(value_t* _env, int8_t _num_rcv_args, value_t varargs);
void prelude__println(value_t* _env, int8_t _num_rcv_args, value_t varargs);

#endif
