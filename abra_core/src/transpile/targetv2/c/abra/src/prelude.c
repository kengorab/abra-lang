#include "prelude.h"

#include "stdio.h"
#include "stdint.h"
#include "inttypes.h"
#include "stdbool.h"
#include "stdlib.h"
#include "assert.h"
#include "string.h"

// For consistency, these are provided by the generated code
extern const size_t TYPE_ID_NONE;
extern const size_t TYPE_ID_INT;
extern const size_t TYPE_ID_FLOAT;
extern const size_t TYPE_ID_BOOL;
extern const size_t TYPE_ID_STRING;
extern const size_t TYPE_ID_ARRAY;

VTableEntry* VTABLE;

void init_vtable(size_t num_types) {
  VTABLE = malloc(sizeof(VTableEntry) * num_types);
}

#define INTRINSIC_TOSTRING_IDX  0
AbraString* call_to_string(AbraAny* value) {
  VTableEntry entry = VTABLE[value->type_id];
  AbraFn tostring = entry.methods[INTRINSIC_TOSTRING_IDX];

  // TODO: Support closures
  assert(!tostring.is_closure);
  AbraString* (* tostring_method)(size_t, AbraAny*) = (AbraString* (*)(size_t, AbraAny*)) (tostring.fn);
  return tostring_method(1, value);
}

#define INTRINSIC_EQ_IDX        1
#define INTRINSIC_HASH_IDX      2

#define METHOD(fn_name, min, max) \
    ((AbraFn) { .is_closure = false, .fn = (Fn) &fn_name, .min_arity = min, .max_arity = max, .captures = (void*) 0 })

// AbraInt methods
AbraFn INT_METHODS[] = {
    METHOD(AbraInt__toString, 1, 1)
};

AbraInt* AbraInt_make(int64_t value) {
  AbraInt* self = malloc(sizeof(AbraInt));
  self->_base = (AbraAny) { .type_id = TYPE_ID_INT };
  self->value = value;
  return self;
}

AbraString* AbraInt__toString(size_t nargs, AbraInt* self) {
  assert(nargs == 1);

  int len = snprintf(NULL, 0, "%" PRId64, self->value);
  char* str = malloc(len + 1);
  snprintf(str, len + 1, "%" PRId64, self->value);

  return AbraString_make(len, str);
}

// AbraFloat methods
AbraFn FLOAT_METHODS[] = {
    METHOD(AbraFloat__toString, 1, 1)
};

AbraFloat* AbraFloat_make(float value) {
  AbraFloat* self = malloc(sizeof(AbraFloat));
  self->_base = (AbraAny) { .type_id = TYPE_ID_FLOAT };
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
      len = i;
    } else {
      break;
    }
  }
  return AbraString_make(len, str);
}

// AbraBool methods
AbraFn BOOL_METHODS[] = {
    METHOD(AbraBool__toString, 1, 1)
};
AbraBool* AbraBool_make(bool value) {
  AbraBool* self = malloc(sizeof(AbraBool));
  self->_base = (AbraAny) { .type_id = TYPE_ID_BOOL };
  self->value = value;
  return self;
}

AbraString* AbraBool__toString(size_t nargs, AbraBool* self) {
  assert(nargs == 1);

  return self->value ? AbraString_make(4, "true") : AbraString_make(5, "false");
}

// AbraString methods
AbraFn STRING_METHODS[] = {
    METHOD(AbraString__toString, 1, 1)
};

AbraString* AbraString_make(size_t len, char* chars) {
  AbraString* self = malloc(sizeof(AbraString));
  self->_base = (AbraAny) { .type_id = TYPE_ID_STRING };
  self->length = len;
  self->chars = chars;
  return self;
}

AbraString* AbraString__toString(size_t nargs, AbraString* self) {
  return self;
}

// AbraArray methods
AbraFn ARRAY_METHODS[] = {
    METHOD(AbraArray__toString, 1, 1)
};

AbraArray* AbraArray_make_with_capacity(size_t length, size_t cap) {
  AbraArray* self = malloc(sizeof(AbraArray));
  self->_base = (AbraAny) { .type_id = TYPE_ID_ARRAY };
  self->items = malloc(sizeof(AbraAny*) * cap);
  self->length = length;
  self->_capacity = cap;
  return self;
}

AbraUnit AbraArray_set(AbraArray* self, size_t idx, AbraAny* item) {
  self->items[idx] = item;
}

AbraString* AbraArray__toString(size_t nargs, AbraArray* self) {
  assert(nargs == 1);

  if (self->length == 0) return AbraString_make(2, "[]");

  AbraString** strings = malloc(sizeof(AbraString*) * self->length);
  size_t total_length = 2; // account for "[]"
  for (size_t i = 0; i < self->length; i++) {
    AbraString* repr = call_to_string(self->items[i]);

    strings[i] = repr;
    total_length += repr->length;
    if (i != self->length - 1) {
      total_length += 2; // account for ", "
    }
  }

  char* res_str = malloc(sizeof(char) * total_length + 1);
  res_str[0] = '[';
  res_str[total_length - 1] = ']';
  res_str[total_length] = 0;

  char* ptr = res_str + 1;
  for (int i = 0; i < self->length; ++i) {
    AbraString* item = strings[i];
    char const* item_str = item->chars;
    size_t len = item->length;
    memcpy(ptr, item_str, len);
    ptr += len;
    if (i < self->length - 1) {
      memcpy(ptr, ", ", 2);
      ptr += 2;
    }
    free(item);
  }

  free(strings);

  return AbraString_make(total_length, res_str);
}

AbraUnit _0_0_0__println(size_t nargs, AbraArray* args) {
  assert(nargs == 1);

  if (args->length == 0) {
    printf("\n");
    return;
  }

  for (size_t i = 0; i < args->length; i++) {
    AbraString* repr = call_to_string(args->items[i]);

    printf("%s", repr->chars);
    if (i != args->length - 1) {
      printf(" ");
    }
  }
  printf("\n");
}

void entrypoint__0() {
  VTABLE[TYPE_ID_INT] = (VTableEntry) { .methods = INT_METHODS };
  VTABLE[TYPE_ID_FLOAT] = (VTableEntry) { .methods = FLOAT_METHODS };
  VTABLE[TYPE_ID_BOOL] = (VTableEntry) { .methods = BOOL_METHODS };
  VTABLE[TYPE_ID_STRING] = (VTableEntry) { .methods = STRING_METHODS };
  VTABLE[TYPE_ID_ARRAY] = (VTableEntry) { .methods = ARRAY_METHODS };
}
