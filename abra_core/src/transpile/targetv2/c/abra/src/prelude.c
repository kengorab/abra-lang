#include "prelude.h"

#include "stdio.h"
#include "stdint.h"
#include "inttypes.h"
#include "stdbool.h"
#include "stdarg.h"
#include "stdlib.h"
#include "assert.h"

extern const size_t TYPE_ID_NONE;// = 0;
extern const size_t TYPE_ID_INT;// = TYPE_ID_DEFAULT_VALUE;
extern const size_t TYPE_ID_FLOAT;// = TYPE_ID_DEFAULT_VALUE;
extern const size_t TYPE_ID_BOOL;// = TYPE_ID_DEFAULT_VALUE;
extern const size_t TYPE_ID_STRING;// = TYPE_ID_DEFAULT_VALUE;
extern const size_t TYPE_ID_ARRAY;// = TYPE_ID_DEFAULT_VALUE;

VTableEntry *VTABLE;

void init_vtable(size_t num_types) {
    VTABLE = malloc(sizeof(VTableEntry) * num_types);
}

#define INTRINSIC_TOSTRING_IDX  0
#define INTRINSIC_EQ_IDX        1
#define INTRINSIC_HASH_IDX      2

#define TYPE_ID_DEFAULT_VALUE 0

AbraInt *AbraInt_make(int64_t value) {
    AbraInt *self = malloc(sizeof(AbraInt));
    self->_base = (AbraAny) {.type_id = TYPE_ID_INT};
    self->value = value;
    return self;
}

AbraString *AbraInt__toString(size_t nargs, AbraInt *self) {
    assert(nargs == 1);

    int len = snprintf(NULL, 0, "%" PRId64, self->value);
    char *str = malloc(len + 1);
    snprintf(str, len + 1, "%" PRId64, self->value);

    return AbraString_make(len, str);
}

AbraFloat *AbraFloat_make(float value) {
    AbraFloat *self = malloc(sizeof(AbraFloat));
    self->_base = (AbraAny) {.type_id = TYPE_ID_FLOAT};
    self->value = value;
    return self;
}

AbraString* AbraFloat__toString(size_t nargs, AbraFloat* self) {
    assert(nargs == 1);

    int len = snprintf(NULL, 0, "%f", self->value);
    char* str = malloc(len + 1);
    snprintf(str, len + 1, "%f", self->value);
    // Trim trailing zeroes
    for (int i = len - 1; i >= 1; --i) {
        if (str[i] == '0' && str[i - 1] != '.') {
            str[i] = 0;
        } else {
            break;
        }
    }
    return AbraString_make(len, str);
}

AbraBool *AbraBool_make(bool value) {
    AbraBool *self = malloc(sizeof(AbraBool));
    self->_base = (AbraAny) {.type_id = TYPE_ID_BOOL};
    self->value = value;
    return self;
}

AbraString* AbraBool__toString(size_t nargs, AbraBool* self) {
    assert(nargs == 1);

    return self->value ? AbraString_make(4, "true") : AbraString_make(5, "false");
}

AbraString *AbraString_make(size_t len, char *chars) {
    AbraString *self = malloc(sizeof(AbraString));
    self->_base = (AbraAny) {.type_id = TYPE_ID_STRING};
    self->length = len;
    self->chars = chars;
    return self;
}

AbraString* AbraString__toString(size_t nargs, AbraString* self) {
    return self;
}

AbraArray *AbraArray_make_with_capacity(size_t length, size_t cap) {
    AbraArray *self = malloc(sizeof(AbraArray));
    self->_base = (AbraAny) {.type_id = TYPE_ID_ARRAY};
    self->items = malloc(sizeof(AbraAny * ) * cap);
    self->length = length;
    self->_capacity = cap;
    return self;
}

AbraUnit AbraArray_set(AbraArray *self, size_t idx, AbraAny *item) {
    self->items[idx] = item;
}

AbraUnit _0_0_0__println(size_t nargs, AbraArray *args) {
    assert(nargs == 1);

    if (args->length == 0) {
        printf("\n");
        return;
    }

    for (size_t i = 0; i < args->length; i++) {
        AbraAny *item = args->items[i];
        VTableEntry entry = VTABLE[item->type_id];
        AbraFn tostring = entry.methods[INTRINSIC_TOSTRING_IDX];

        // TODO: Support closures
        assert(!tostring.is_closure);
        AbraString * (*tostring_method)(size_t, AbraAny * ) = (AbraString * (*)(size_t, AbraAny * ))(tostring.fn);
        AbraString *repr = tostring_method(1, item);

        printf("%s", repr->chars);
        if (i != args->length - 1) {
            printf(" ");
        }
    }
    printf("\n");
}

void entrypoint__0() {
    VTABLE[TYPE_ID_INT] = (VTableEntry) {
        .methods = malloc(sizeof(AbraFn) * 1),
    };
    VTABLE[TYPE_ID_INT].methods[0] = (AbraFn) {
        .is_closure = false,
        .fn = (Fn) &AbraInt__toString,
        .min_arity = 1,
        .max_arity = 1,
        .captures = (void *) 0,
    };

    VTABLE[TYPE_ID_FLOAT] = (VTableEntry) {
            .methods = malloc(sizeof(AbraFn) * 1),
    };
    VTABLE[TYPE_ID_FLOAT].methods[0] = (AbraFn) {
            .is_closure = false,
            .fn = (Fn) &AbraFloat__toString,
            .min_arity = 1,
            .max_arity = 1,
            .captures = (void *) 0,
    };

    VTABLE[TYPE_ID_BOOL] = (VTableEntry) {
            .methods = malloc(sizeof(AbraFn) * 1),
    };
    VTABLE[TYPE_ID_BOOL].methods[0] = (AbraFn) {
            .is_closure = false,
            .fn = (Fn) &AbraBool__toString,
            .min_arity = 1,
            .max_arity = 1,
            .captures = (void *) 0,
    };

    VTABLE[TYPE_ID_STRING] = (VTableEntry) {
        .methods = malloc(sizeof(AbraFn) * 1),
    };
    VTABLE[TYPE_ID_STRING].methods[0] = (AbraFn) {
       .is_closure = false,
       .fn = (Fn) &AbraString__toString,
       .min_arity = 1,
       .max_arity = 1,
       .captures = (void *) 0,
    };
}
