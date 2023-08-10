#include "prelude.h"

#include "stdio.h"
#include "stdint.h"
#include "inttypes.h"
#include "stdbool.h"
#include "stdlib.h"
#include "assert.h"
#include "string.h"
#include "stdarg.h"

// For consistency, these are provided by the generated code
extern const size_t TYPE_ID_NONE;
extern const size_t TYPE_ID_INT;
extern const size_t TYPE_ID_FLOAT;
extern const size_t TYPE_ID_BOOL;
extern const size_t TYPE_ID_STRING;
extern const size_t TYPE_ID_ARRAY;
extern const size_t TYPE_ID_TUPLE;

VTableEntry* VTABLE;

void init_vtable(size_t num_types) {
  VTABLE = malloc(sizeof(VTableEntry) * num_types);
}

#define RANGE_ENDPOINTS(start, end, len) \
  do {                                   \
    if (start < 0) start += len;         \
    else if (len == 0) start = 0;        \
    if (end < 0) end += len;             \
    else if (end < start) end = start;   \
    else if (end >= len) end = len;      \
  } while (0)

#define INTRINSIC_TOSTRING_IDX  0

AbraString* prelude__tostring(AbraAny* value) {
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

// AbraNone methods
AbraFn NONE_METHODS[] = {
    METHOD(AbraNone__toString, 1, 1)
};

static AbraAny* ABRA_NONE = NULL;

AbraAny* AbraNone_make() {
  if (!ABRA_NONE) {
    ABRA_NONE = malloc(sizeof(AbraAny));
    ABRA_NONE->type_id = TYPE_ID_NONE;
  }
  return ABRA_NONE;
}

static AbraString* ABRA_NONE_STRING = NULL;

AbraString* AbraNone__toString(size_t nargs, AbraAny* _self) {
  assert(nargs == 1);

  if (!ABRA_NONE_STRING) ABRA_NONE_STRING = AbraString_make(4, "None");
  return ABRA_NONE_STRING;
}

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

AbraFloat* AbraFloat_make(double value) {
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

static AbraBool* ABRA_BOOL_TRUE = NULL;
static AbraBool* ABRA_BOOL_FALSE = NULL;

AbraBool* AbraBool_make(bool value) {
  if (value) {
    if (!ABRA_BOOL_TRUE) {
      ABRA_BOOL_TRUE = malloc(sizeof(AbraBool));
      ABRA_BOOL_TRUE->_base = (AbraAny) { .type_id = TYPE_ID_BOOL };
      ABRA_BOOL_TRUE->value = true;
    }
    return ABRA_BOOL_TRUE;
  } else {
    if (!ABRA_BOOL_FALSE) {
      ABRA_BOOL_FALSE = malloc(sizeof(AbraBool));
      ABRA_BOOL_FALSE->_base = (AbraAny) { .type_id = TYPE_ID_BOOL };
      ABRA_BOOL_FALSE->value = false;
    }
    return ABRA_BOOL_FALSE;
  }
}

static AbraString* ABRA_BOOL_TRUE_STRING = NULL;
static AbraString* ABRA_BOOL_FALSE_STRING = NULL;

AbraString* AbraBool__toString(size_t nargs, AbraBool* self) {
  assert(nargs == 1);

  if (self->value) {
    if (!ABRA_BOOL_TRUE_STRING) ABRA_BOOL_TRUE_STRING = AbraString_make(4, "true");
    return ABRA_BOOL_TRUE_STRING;
  } else {
    if (!ABRA_BOOL_FALSE_STRING) ABRA_BOOL_FALSE_STRING = AbraString_make(5, "false");
    return ABRA_BOOL_FALSE_STRING;
  }
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

AbraString* AbraString_empty_string() {
  return AbraString_make(0, "");
}

AbraString* AbraString_get(AbraString* self, int64_t index) {
  int64_t len = self->length;
  if (index < -len || index >= len) return AbraString_empty_string();

  if (index < 0) index += len;

  char* str = malloc(sizeof(char));
  str[0] = self->chars[index];
  return AbraString_make(1, str);
}

AbraString* AbraString_slice(AbraString* self, int64_t index) {
  if (index >= self->length) return AbraString_empty_string();
  if (index < 0) index = 0;

  size_t len = self->length - index;
  char* str = malloc(sizeof(char) * len + 1);
  str[len] = 0;

  memcpy(str, self->chars + index, len);
  memset(self->chars + index, 0, len);

  return AbraString_make(len, str);
}

AbraString* AbraString_get_range(AbraString* self, int64_t start, int64_t end) {
  int64_t len = self->length;
  RANGE_ENDPOINTS(start, end, len);

  if (start >= end) return AbraString_empty_string();

  int64_t slice_size = end - start;
  char* str = malloc(slice_size);
  memcpy(str, self->chars + start, slice_size);
  str[slice_size] = 0;

  return AbraString_make(slice_size, str);
}

AbraString* AbraString__toString(size_t nargs, AbraString* self) {
  return self;
}

AbraString* AbraString__concat(size_t nargs, AbraString* self, AbraAny* other) {
  assert(nargs == 2);

  AbraString* repr = prelude__tostring(other);

  size_t len = repr->length + self->length;
  char* concat = malloc(sizeof(char) * len + 1);
  concat[len] = 0;

  memcpy(concat, self->chars, self->length);
  memcpy(concat + self->length, repr->chars, repr->length);
  return AbraString_make(len, concat);
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

AbraArray* AbraArray_empty_array() {
  return AbraArray_make_with_capacity(0, 1);
}

AbraUnit AbraArray_set(AbraArray* self, size_t index, AbraAny* item) {
  self->items[index] = item;
}

AbraAny* AbraArray_get(AbraArray* self, int64_t index) {
  int64_t len = self->length;
  if (index < -len || index >= len) return AbraNone_make();

  if (index < 0) index += len;
  return self->items[index];
}

AbraArray* AbraArray_slice(AbraArray* self, int64_t index) {
  if (index >= self->length) return AbraArray_empty_array();
  if (index < 0) index = 0;

  size_t len = self->length - index;
  AbraArray* slice = AbraArray_make_with_capacity(len, len);

  size_t self_length = self->length;
  for (size_t i = index; i < self_length; i++) {
    slice->items[i - index] = self->items[i];
    self->items[i] = NULL;
    self->length -= 1;
  }

  return slice;
}

AbraArray* AbraArray_get_range(AbraArray* self, int64_t start, int64_t end) {
  int64_t len = self->length;
  RANGE_ENDPOINTS(start, end, len);

  if (start >= end) return AbraArray_empty_array();

  int64_t slice_size = end - start;
  AbraArray* new_array = AbraArray_make_with_capacity(slice_size, slice_size);
  for (int64_t i = start; i < end; i++) {
    new_array->items[i - start] = self->items[i];
  }
  return new_array;
}

AbraString* sequence_to_string(int64_t length, AbraAny** items) {
  if (length == 0) return AbraString_make(2, "[]");

  typedef struct item_t {
      AbraString* repr;
      bool item_is_string;
  } item_t;

  item_t strings[length];
  size_t total_length = 2; // account for "[]"
  for (size_t i = 0; i < length; i++) {
    AbraAny* item = items[i];
    AbraString* repr = prelude__tostring(item);

    strings[i] = (item_t) {
      .repr=repr,
      .item_is_string=item->type_id == TYPE_ID_STRING
    };
    total_length += repr->length;
    if (item->type_id == TYPE_ID_STRING) {
      total_length += 2; // account for '""'
    }
    if (i != length - 1) {
      total_length += 2; // account for ", "
    }
  }

  char* res_str = malloc(sizeof(char) * total_length + 1);
  res_str[0] = '[';
  res_str[total_length - 1] = ']';
  res_str[total_length] = 0;

  char* ptr = res_str + 1;
  for (int i = 0; i < length; i++) {
    bool item_is_string = strings[i].item_is_string;
    AbraString* repr = strings[i].repr;
    size_t len = repr->length;
    if (item_is_string) memcpy(ptr++, "\"", 1);
    memcpy(ptr, repr->chars, len);
    ptr += len;
    if (item_is_string) memcpy(ptr++, "\"", 1);

    if (i < length - 1) {
      memcpy(ptr, ", ", 2);
      ptr += 2;
    }
  }

  return AbraString_make(total_length, res_str);
}

AbraString* AbraArray__toString(size_t nargs, AbraArray* self) {
  assert(nargs == 1);

  return sequence_to_string(self->length, self->items);
}

// AbraTuple methods
AbraFn TUPLE_METHODS[] = {
    METHOD(AbraTuple__toString, 1, 1)
};

AbraTuple* AbraTuple_make(size_t length, ...) {
  AbraTuple* tuple = malloc(sizeof(AbraTuple));
  tuple->_base = (AbraAny) { .type_id=TYPE_ID_TUPLE };
  tuple->length = length;
  tuple->items = malloc(sizeof(AbraAny*) * length);

  va_list items;
  va_start(items, length);
  for (size_t i = 0; i < length; i++) {
    AbraAny* item = va_arg(items, AbraAny*);
    tuple->items[i] = item;
  }
  va_end(items);
  return tuple;
}

AbraAny* AbraTuple_get(AbraTuple* self, int64_t index) {
  return self->items[index];
}

AbraString* AbraTuple__toString(size_t nargs, AbraTuple* self) {
  assert(nargs == 1);

  AbraString* repr = sequence_to_string(self->length, self->items);
  repr->chars[0] = '(';
  repr->chars[repr->length - 1] = ')';
  return repr;
}

AbraUnit _0_0_0__println(size_t nargs, AbraArray* args) {
  assert(nargs == 1);

  if (args->length == 0) {
    printf("\n");
    return;
  }

  for (size_t i = 0; i < args->length; i++) {
    AbraString* repr = prelude__tostring(args->items[i]);

    printf("%s", repr->chars);
    if (i != args->length - 1) {
      printf(" ");
    }
  }
  printf("\n");
}

void entrypoint__0() {
  VTABLE[TYPE_ID_NONE] = (VTableEntry) { .methods = NONE_METHODS };
  VTABLE[TYPE_ID_INT] = (VTableEntry) { .methods = INT_METHODS };
  VTABLE[TYPE_ID_FLOAT] = (VTableEntry) { .methods = FLOAT_METHODS };
  VTABLE[TYPE_ID_BOOL] = (VTableEntry) { .methods = BOOL_METHODS };
  VTABLE[TYPE_ID_STRING] = (VTableEntry) { .methods = STRING_METHODS };
  VTABLE[TYPE_ID_ARRAY] = (VTableEntry) { .methods = ARRAY_METHODS };
  VTABLE[TYPE_ID_TUPLE] = (VTableEntry) { .methods = TUPLE_METHODS };
}
