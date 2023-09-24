#ifndef ABRA_PRELUDE_H
#define ABRA_PRELUDE_H

#include "stdio.h"
#include "stdint.h"
#include "inttypes.h"
#include "stdbool.h"
#include "stdarg.h"
#include "stdlib.h"

typedef void AbraUnit;

typedef struct AbraAny {
    size_t type_id;
    void* data;
} AbraAny;

#define REQUIRED_VALUE_SIZE sizeof(AbraAny)

typedef void (*Fn)(void);
typedef struct AbraFnObj {
    bool is_closure;
    Fn fn;
    size_t min_arity;
    size_t max_arity;
    AbraAny** captures;
} AbraFnObj;
typedef struct AbraFn {
    size_t type_id;
    AbraFnObj *value;
} AbraFn;
typedef struct VTableEntry {
    AbraFnObj fn_eq;
    AbraFnObj fn_hash;
    AbraFnObj* methods;
} VTableEntry;
void init_vtable(size_t num_types);
extern VTableEntry* VTABLE;

#define METHOD(fn_name, min, max) \
    ((AbraFnObj) { .is_closure = false, .fn = (Fn) &fn_name, .min_arity = min, .max_arity = max, .captures = (void*) 0 })

#define REINTERPRET_CAST(v, T) (*(T*)&(v))

typedef struct AbraInt {
    size_t type_id;
    int64_t value;
} AbraInt;

typedef struct AbraFloat {
    size_t type_id;
    double value;
} AbraFloat;

typedef struct AbraBool {
    size_t type_id;
    bool value;
} AbraBool;

typedef struct AbraString_Inner {
    char *chars;
    int64_t length;
} AbraString_Inner;
typedef struct AbraString {
    size_t type_id;
    AbraString_Inner * value;
} AbraString;

typedef struct AbraArray_Inner {
    AbraAny* items;
    int64_t length;
    int64_t _capacity;
}AbraArray_Inner;
typedef struct AbraArray {
    size_t type_id;
    AbraArray_Inner * value;
} AbraArray;

typedef struct AbraTuple_Inner {
    AbraAny* items;
    int64_t length;
} AbraTuple_Inner;
typedef struct AbraTuple {
    size_t type_id;
    AbraTuple_Inner * value;
} AbraTuple;

typedef struct hashmap_t hashmap_t;

typedef struct AbraSet {
    size_t type_id;
    hashmap_t* value;
} AbraSet;

typedef struct AbraMap {
    size_t type_id;
    hashmap_t* value;
} AbraMap;

AbraString prelude__tostring(AbraAny value);
AbraBool prelude__eq(AbraAny value, AbraAny other, bool neg);
AbraInt prelude__hash(AbraAny value);

#define PRELUDE_TOSTRING(v) (prelude__tostring(REINTERPRET_CAST((v), AbraAny)))
#define PRELUDE_EQ(v, o) (prelude__eq(REINTERPRET_CAST((v), AbraAny), REINTERPRET_CAST((o), AbraAny), false))
#define PRELUDE_HASH(v) (prelude__hash(REINTERPRET_CAST((v), AbraAny)))

#define ABRA_CL_CALL_0(func, nargs, rty) \
    ((rty (*)(size_t, AbraAny**)) ((*(AbraFn*)&(func)).value->fn))(nargs, (*(AbraFn*)&(func)).value->captures)
#define ABRA_CL_CALL_1(func, nargs, rty, a1ty, arg1) \
    ((rty (*)(size_t, AbraAny**, a1ty)) ((*(AbraFn*)&(func)).value->fn))(nargs, (*(AbraFn*)&(func)).value->captures, arg1)
#define ABRA_CL_CALL_2(func, nargs, rty, a1ty, arg1, a2ty, arg2) \
    ((rty (*)(size_t, AbraAny**, a1ty, a2ty)) ((*(AbraFn*)&(func)).value->fn))(nargs, (*(AbraFn*)&(func)).value->captures, arg1, arg2)

#define ABRA_FN_CALL_0(func, nargs, rty) \
  ((*(AbraFn*)&(func)).value->is_closure ? ABRA_CL_CALL_0(func, nargs, rty) : ((rty (*)(size_t)) ((*(AbraFn*)&(func)).value->fn))(nargs))
#define ABRA_FN_CALL_1(func, nargs, rty, a1ty, arg1) \
  ((*(AbraFn*)&(func)).value->is_closure ? ABRA_CL_CALL_1(func, nargs, rty, a1ty, arg1) : ((rty (*)(size_t, a1ty)) ((*(AbraFn*)&(func)).value->fn))(nargs, arg1))
#define ABRA_FN_CALL_2(func, nargs, rty, a1ty, arg1, a2ty, arg2) \
  ((*(AbraFn*)&(func)).value->is_closure ? ABRA_CL_CALL_2(func, nargs, rty, a1ty, arg1, a2ty, arg2) : ((rty (*)(size_t, a1ty, a2ty)) ((*(AbraFn*)&(func)).value->fn))(nargs, arg1, arg2))

AbraAny* copy_to_heap(AbraAny* value);

AbraFn AbraFn_make(Fn fn_ptr, size_t min_arity, size_t max_arity);
AbraFn AbraFn_make_closure(Fn fn_ptr, size_t min_arity, size_t max_arity, size_t ncaptures);
inline AbraAny AbraFn_call_0(AbraFn fn, size_t nargs);
inline AbraAny AbraFn_call_1(AbraFn fn, size_t nargs, AbraAny arg1);

#define IS_NONE(v) ((v).type_id == TYPE_ID_NONE)

extern AbraAny AbraNone;

AbraString AbraNone__toString(size_t nargs, AbraAny self);
AbraBool AbraNone__eq(size_t nargs, AbraAny self, AbraAny other);
AbraInt AbraNone__hash(size_t nargs, AbraAny self);

#define AbraInt_make(v) ((AbraInt) { .type_id=TYPE_ID_INT, .value=v })
AbraString AbraInt__toString(size_t nargs, AbraInt self);
AbraBool AbraInt__eq(size_t nargs, AbraInt self, AbraAny other);
AbraInt AbraInt__hash(size_t nargs, AbraInt self);

#define AbraFloat_make(v) ((AbraFloat) { .type_id=TYPE_ID_FLOAT, .value=v })
AbraString AbraFloat__toString(size_t nargs, AbraFloat self);
AbraBool AbraFloat__eq(size_t nargs, AbraFloat self, AbraAny other);
AbraInt AbraFloat__hash(size_t nargs, AbraFloat self);

extern AbraBool ABRA_BOOL_TRUE;
extern AbraBool ABRA_BOOL_FALSE;
#define AbraBool_make(v) ((AbraBool)(v ? ABRA_BOOL_TRUE : ABRA_BOOL_FALSE))
AbraString AbraBool__toString(size_t nargs, AbraBool self);
AbraBool AbraBool__eq(size_t nargs, AbraBool self, AbraAny other);
AbraInt AbraBool__hash(size_t nargs, AbraBool self);

AbraString AbraString_make(size_t len, char* chars);
AbraString AbraString_empty_string();
AbraString AbraString_get(AbraString self, int64_t index);
AbraString AbraString_slice(AbraString self, int64_t index);
AbraString AbraString_get_range(AbraString self, int64_t start, int64_t end);
AbraInt AbraString_field_length(AbraString self);
AbraString AbraString__toString(size_t nargs, AbraString self);
AbraBool AbraString__eq(size_t nargs, AbraString self, AbraAny other);
AbraInt AbraString__hash(size_t nargs, AbraString self);
AbraString AbraString__concat(size_t nargs, AbraString self, AbraAny other);

AbraArray AbraArray_make_with_capacity(size_t length, size_t cap);
AbraUnit AbraArray_set(AbraArray self, int64_t index, AbraAny item);
AbraAny AbraArray_get(AbraArray self, int64_t index);
AbraArray AbraArray_slice(AbraArray self, int64_t index);
AbraArray AbraArray_get_range(AbraArray self, int64_t start, int64_t end);
AbraInt AbraArray_field_length(AbraArray self);
AbraString AbraArray__toString(size_t nargs, AbraArray self);
AbraBool AbraArray__eq(size_t nargs, AbraArray self, AbraAny other);
AbraInt AbraArray__hash(size_t nargs, AbraArray self);

AbraTuple AbraTuple_make(size_t length, ...);
AbraAny AbraTuple_get(AbraTuple self, int64_t index);
AbraString AbraTuple__toString(size_t nargs, AbraTuple self);
AbraBool AbraTuple__eq(size_t nargs, AbraTuple self, AbraAny other);
AbraInt AbraTuple__hash(size_t nargs, AbraTuple self);

AbraSet AbraSet_make();
AbraUnit AbraSet_insert(AbraSet self, AbraAny value);
AbraInt AbraSet_field_size(AbraSet self);
AbraString AbraSet__toString(size_t nargs, AbraSet self);
AbraBool AbraSet__eq(size_t nargs, AbraSet self, AbraAny other);
AbraInt AbraSet__hash(size_t nargs, AbraSet self);

AbraMap AbraMap_make();
AbraUnit AbraMap_set(AbraMap self, AbraAny key, AbraAny value);
AbraAny AbraMap_get(AbraMap self, AbraAny key);
AbraInt AbraMap_field_size(AbraMap self);
AbraString AbraMap__toString(size_t nargs, AbraMap self);
AbraBool AbraMap__eq(size_t nargs, AbraMap self, AbraAny other);
AbraInt AbraMap__hash(size_t nargs, AbraMap self);

AbraUnit _0_0_0__print(size_t nargs, AbraArray args);
AbraUnit _0_0_1__println(size_t nargs, AbraArray args);

void entrypoint__0();

#include "hashmap.h"

#endif // ABRA_PRELUDE_H
