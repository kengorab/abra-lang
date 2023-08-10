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
    AbraAny* captures;
} AbraFn;
typedef struct VTableEntry {
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

#define IS_NONE(v) (((AbraAny*)v)->type_id == TYPE_ID_NONE)

AbraAny* AbraNone_make();
AbraString* AbraNone__toString(size_t nargs, AbraAny* self);

AbraInt* AbraInt_make(int64_t value);
AbraString* AbraInt__toString(size_t nargs, AbraInt* self);

AbraFloat* AbraFloat_make(double value);
AbraString* AbraFloat__toString(size_t nargs, AbraFloat* self);

AbraBool* AbraBool_make(bool value);
AbraString* AbraBool__toString(size_t nargs, AbraBool* self);

AbraString* AbraString_make(size_t len, char* chars);
AbraString* AbraString_empty_string();
AbraString* AbraString_get(AbraString* self, int64_t index);
AbraString* AbraString_slice(AbraString* self, int64_t index);
AbraString* AbraString_get_range(AbraString* self, int64_t start, int64_t end);
AbraString* AbraString__toString(size_t nargs, AbraString* self);
AbraString* AbraString__concat(size_t nargs, AbraString* self, AbraAny* other);

AbraArray* AbraArray_make_with_capacity(size_t length, size_t cap);
AbraUnit AbraArray_set(AbraArray* self, size_t index, AbraAny* item);
AbraAny* AbraArray_get(AbraArray* self, int64_t index);
AbraArray* AbraArray_slice(AbraArray* self, int64_t index);
AbraArray* AbraArray_get_range(AbraArray* self, int64_t start, int64_t end);
AbraString* AbraArray__toString(size_t nargs, AbraArray* self);

AbraTuple* AbraTuple_make(size_t length, ...);
AbraAny* AbraTuple_get(AbraTuple* self, int64_t index);
AbraString* AbraTuple__toString(size_t nargs, AbraTuple* self);

AbraUnit _0_0_0__println(size_t nargs, AbraArray* args);

void entrypoint__0();

#endif // ABRA_PRELUDE_H
