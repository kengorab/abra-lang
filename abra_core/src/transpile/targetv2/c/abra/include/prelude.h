#ifndef ABRA_PRELUDE_H
#define ABRA_PRELUDE_H

#include "stdio.h"
#include "stdint.h"
#include "inttypes.h"
#include "stdbool.h"
#include "stdarg.h"
#include "stdlib.h"
#include "assert.h"

typedef void AbraUnit;

typedef struct AbraAny {
    size_t type_id;
} AbraAny;

typedef void (*Fn)(void);
typedef struct AbraFn {
    bool is_closure;
    Fn fn;
    size_t min_arity;
    size_t max_arity;
    AbraAny** captures;
} AbraFn;
typedef struct VTableEntry {
    AbraFn fn_tostring;
    AbraFn fn_eq;
    AbraFn fn_hash;
    AbraFn* methods;
} VTableEntry;
void init_vtable(size_t num_types);

typedef struct AbraInt {
    AbraAny _base;
    int64_t value;
} AbraInt;

typedef struct AbraFloat {
    AbraAny _base;
    double value;
} AbraFloat;

typedef struct AbraBool {
    AbraAny _base;
    bool value;
} AbraBool;

typedef struct AbraString {
    AbraAny _base;
    char *chars;
    int64_t length;
} AbraString;

typedef struct AbraArray {
    AbraAny _base;
    AbraAny** items;
    int64_t length;
    int64_t _capacity;
} AbraArray;

typedef struct AbraTuple {
    AbraAny _base;
    AbraAny** items;
    int64_t length;
} AbraTuple;

AbraString* prelude__tostring(AbraAny* value);
AbraBool* prelude__eq(AbraAny* value, AbraAny* other, bool neg);


#define ABRA_CL_CALL_0(func, nargs, rty) \
    ((rty (*)(size_t, AbraAny**)) ((AbraFn*)(func)->fn))(nargs, ((AbraFn*)func)->captures)
#define ABRA_CL_CALL_1(func, nargs, rty, a1ty, arg1) \
    ((rty (*)(size_t, AbraAny**, a1ty)) (((AbraFn*)func)->fn))(nargs, ((AbraFn*)func)->captures, arg1)
#define ABRA_CL_CALL_2(func, nargs, rty, a1ty, arg1, a2ty, arg2) \
    ((rty (*)(size_t, AbraAny**, a1ty, a2ty)) (((AbraFn*)func)->fn))(nargs, ((AbraFn*)func)->captures, arg1, arg2)

#define ABRA_FN_CALL_0(func, nargs, rty) \
  (((AbraFn*)func)->is_closure ? ABRA_CL_CALL_0(func, nargs, rty) : ((rty (*)(size_t)) (((AbraFn*)func)->fn))(nargs))
#define ABRA_FN_CALL_1(func, nargs, rty, a1ty, arg1) \
  (((AbraFn*)func)->is_closure ? ABRA_CL_CALL_1(func, nargs, rty, a1ty, arg1) : ((rty (*)(size_t, a1ty)) (((AbraFn*)func)->fn))(nargs, arg1))
#define ABRA_FN_CALL_2(func, nargs, rty, a1ty, arg1, a2ty, arg2) \
  (((AbraFn*)func)->is_closure ? ABRA_CL_CALL_2(func, nargs, rty, a1ty, arg1, a2ty, arg2) : ((rty (*)(size_t, a1ty, a2ty)) (((AbraFn*)func)->fn))(nargs, arg1, arg2))

#define HEAP_TAG(p) ((int64_t)p | 1)
#define HEAP_IS_TAGGED(p) ((int64_t)p & 1)
#define HEAP_UNTAG(p) ((int64_t)p & 0xfffffffffffc)
#define HEAP_DEREF(p, ty) (*(ty*)HEAP_UNTAG(p))
#define HEAP_DEREF_IF_NEEDED(p, ty) (HEAP_IS_TAGGED(p) ? *(ty*)HEAP_UNTAG(p) : p)
#define MOVE_TO_HEAP(v, ty)        \
  do {                             \
    ty* v##_ = malloc(sizeof(ty)); \
    *v##_=v;                       \
    v = (ty)HEAP_TAG(v##_);        \
  } while (0)

AbraFn* AbraFn_make(Fn fn_ptr, size_t min_arity, size_t max_arity);
AbraFn* AbraFn_make_closure(Fn fn_ptr, size_t min_arity, size_t max_arity, size_t ncaptures, ...);
inline AbraAny* AbraFn_call_0(AbraFn* fn, size_t nargs);
inline AbraAny* AbraFn_call_1(AbraFn* fn, size_t nargs, AbraAny* arg1);

#define IS_NONE(v) (((AbraAny*)v)->type_id == TYPE_ID_NONE)

AbraAny* AbraNone_make();
AbraString* AbraNone__toString(size_t nargs, AbraAny* self);
AbraBool* AbraNone__eq(size_t nargs, AbraAny* self, AbraAny* other);

AbraInt* AbraInt_make(int64_t value);
AbraString* AbraInt__toString(size_t nargs, AbraInt* self);
AbraBool* AbraInt__eq(size_t nargs, AbraInt* self, AbraAny* other);

AbraFloat* AbraFloat_make(double value);
AbraString* AbraFloat__toString(size_t nargs, AbraFloat* self);
AbraBool* AbraFloat__eq(size_t nargs, AbraFloat* self, AbraAny* other);

AbraBool* AbraBool_make(bool value);
AbraString* AbraBool__toString(size_t nargs, AbraBool* self);
AbraBool* AbraBool__eq(size_t nargs, AbraBool* self, AbraAny* other);

AbraString* AbraString_make(size_t len, char* chars);
AbraString* AbraString_empty_string();
AbraString* AbraString_get(AbraString* self, int64_t index);
AbraString* AbraString_slice(AbraString* self, int64_t index);
AbraString* AbraString_get_range(AbraString* self, int64_t start, int64_t end);
AbraString* AbraString__toString(size_t nargs, AbraString* self);
AbraBool* AbraString__eq(size_t nargs, AbraString* self, AbraAny* other);
AbraString* AbraString__concat(size_t nargs, AbraString* self, AbraAny* other);

AbraArray* AbraArray_make_with_capacity(size_t length, size_t cap);
AbraUnit AbraArray_set(AbraArray* self, int64_t index, AbraAny* item);
AbraAny* AbraArray_get(AbraArray* self, int64_t index);
AbraArray* AbraArray_slice(AbraArray* self, int64_t index);
AbraArray* AbraArray_get_range(AbraArray* self, int64_t start, int64_t end);
AbraString* AbraArray__toString(size_t nargs, AbraArray* self);
AbraBool* AbraArray__eq(size_t nargs, AbraArray* self, AbraAny* other);

AbraTuple* AbraTuple_make(size_t length, ...);
AbraAny* AbraTuple_get(AbraTuple* self, int64_t index);
AbraString* AbraTuple__toString(size_t nargs, AbraTuple* self);
AbraBool* AbraTuple__eq(size_t nargs, AbraTuple* self, AbraAny* other);

AbraUnit _0_0_0__println(size_t nargs, AbraArray* args);

void entrypoint__0();

#endif // ABRA_PRELUDE_H
