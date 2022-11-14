#ifndef __ABRA_RT_H
#define __ABRA_RT_H

#include "stdarg.h"
#include "stdio.h"
#include "stdlib.h"
#include "stdint.h"
#include "string.h"
#include "stdbool.h"
#include "math.h"
#include "nan.h"
#include "hashmap.h"

// ------------------------ VTABLE ------------------------
typedef struct vtable_entry_t {
  value_t* instance_methods;
  value_t* static_methods;
} vtable_entry_t;

vtable_entry_t vtable[256];

void vtable_alloc_entry(uint32_t type_id, uint32_t num_instance, uint32_t num_static);
value_t vtable_lookup(value_t value, uint32_t idx);
value_t vtable_lookup_static(uint32_t type_id, uint32_t idx);
void vtable_insert(uint32_t type_id, uint32_t idx, bool is_static, value_t fn_ptr);

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
bool value_eq(value_t v1, value_t v2);
uint32_t value_hash(value_t v);

#define DBG(v) printf("`" #v "` => %s\n", AS_OBJ(value_to_string(v), String)->chars);
#define UNIMPLEMENTED                                                          \
  do {                                                                         \
    printf("%s:%d: Unimplemented function %s\n", __FILE__, __LINE__, __func__);\
    exit(1);                                                                   \
  } while (0);

// ------------------------ UTILITIES ------------------------
value_t build_argv_array(int argc, char** argv);
value_t build_envp_map(char** envp);

// ------------------------ TYPE MANAGEMENT ------------------------
const uint32_t TOSTRING_IDX = 0;
typedef value_t (*tostring_method_t)(value_t*, int8_t, value_t);
const uint32_t EQ_IDX = 1;
typedef value_t (*eq_method_t)(value_t*, int8_t, value_t, value_t);
const uint32_t HASH_IDX = 2;
typedef value_t (*hash_method_t)(value_t*, int8_t, value_t);

uint32_t type_id_None = UINT32_MAX;

uint32_t next_type_id = 0;
uint32_t type_id_for_val(value_t value);

typedef struct obj_header_t {
  uint32_t type_id;
} obj_header_t;

// ------------------------ INT ------------------------
uint32_t type_id_Int;
value_t prelude__Int__toString(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__Int__abs(value_t* _env, int8_t _num_rcv_args, value_t _self);

// ------------------------ FLOAT ------------------------
uint32_t type_id_Float;
value_t prelude__Float__toString(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__Float__floor(value_t* _env, int8_t _num_rcv_args, value_t _self);

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
value_t prelude__String__padLeft(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _totalSize, value_t _padding);
value_t prelude__String__trim(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__String__trimStart(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _pattern);
value_t prelude__String__trimEnd(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _pattern);
value_t prelude__String__split(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _splitter);
value_t prelude__String__splitAt(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _index);
value_t prelude__String__lines(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__String__chars(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__String__parseInt(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _radix);
value_t prelude__String__parseFloat(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__String__concat(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _str, value_t _others);
value_t prelude__String__replaceAll(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _pattern, value_t _replacement);

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
value_t prelude__Array__isEmpty(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__Array__enumerate(value_t* _env, int8_t _num_rcv_args, value_t _self);
void prelude__Array__push(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _item, value_t _others);
value_t prelude__Array__pop(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__Array__popFront(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__Array__splitAt(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _index);
value_t prelude__Array__concat(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _other);
value_t prelude__Array__map(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _fn);
value_t prelude__Array__filter(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _fn);
value_t prelude__Array__reduce(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _initialValue, value_t _fn);
void prelude__Array__forEach(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _fn);
value_t prelude__Array__join(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _joiner);
value_t prelude__Array__contains(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _item);
value_t prelude__Array__find(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _fn);
value_t prelude__Array__findIndex(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _fn);
value_t prelude__Array__any(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _fn);
value_t prelude__Array__all(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _fn);
value_t prelude__Array__none(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _fn);
value_t prelude__Array__sortBy(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _fn, value_t _reverse);
value_t prelude__Array__dedupe(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__Array__dedupeBy(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _fn);
value_t prelude__Array__partition(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _fn);
value_t prelude__Array__tally(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__Array__tallyBy(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _fn);
value_t prelude__Array__asSet(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__Array__getOrDefault(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _key, value_t _default);
value_t prelude__Array__getOrElse(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _key, value_t _fn);
void prelude__Array__update(value_t* _env, int8_t _num_rcv_args, value_t _self, value_t _key, value_t _fn);
value_t prelude__Array__reverse(value_t* _env, int8_t _num_rcv_args, value_t _self);

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
void tuple_set(value_t _self, int32_t idx, value_t value);

// Tuple methods
value_t prelude__Tuple__toString(value_t* _env, int8_t _num_rcv_args, value_t _self);

// ------------------------ MAP ------------------------
uint32_t type_id_Map;
typedef struct Map {
  obj_header_t h;
  hashmap_t hash;
} Map;

// Map utils
value_t map_alloc(int32_t size);
void map_insert(value_t _self, value_t key, value_t value);
value_t map_get(value_t _self, value_t key);

// Map static
value_t prelude__Map__fromPairs(value_t* _env, int8_t _num_rcv_args, value_t _pairs);

// Map methods
value_t prelude__Map__toString(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__Map__isEmpty(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__Map__enumerate(value_t* _env, int8_t _num_rcv_args, value_t _self);

// ------------------------ SET ------------------------
uint32_t type_id_Set;
typedef struct Set {
  obj_header_t h;
  hashmap_t hash;
} Set;

// Set utils
value_t set_alloc(int32_t size);
void set_insert(value_t _self, value_t value);

// Set methods
value_t prelude__Set__toString(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__Set__isEmpty(value_t* _env, int8_t _num_rcv_args, value_t _self);
value_t prelude__Set__enumerate(value_t* _env, int8_t _num_rcv_args, value_t _self);

// ------------------------ FUNCTION ------------------------
uint32_t type_id_Function;
typedef struct Function {
  obj_header_t h;
  char* name;
  value_t fn_ptr;
  value_t* env;
  uint32_t id;
  value_t bound_self;
} Function;

// Function utils
value_t function_alloc(char* name, value_t fn_ptr, value_t* env);
value_t function_bind(value_t fn_val, value_t self);
value_t function_call(value_t fn_val, bool has_return, int8_t fn_arity, int8_t argc, ...);

// Function methods
value_t prelude__Function__toString(value_t* _env, int8_t _num_rcv_args, value_t _self);

// ------------------------ PRELUDE FUNCTIONS ------------------------
void prelude__print(value_t* _env, int8_t _num_rcv_args, value_t varargs);
void prelude__println(value_t* _env, int8_t _num_rcv_args, value_t varargs);
value_t prelude__range(value_t* _env, int8_t num_rcv_args, value_t _from, value_t _to, value_t _increment);

// ------------------------ IO FUNCTIONS ------------------------
value_t io__readFile(value_t* _env, int8_t num_rcv_args, value_t _path);

#endif
