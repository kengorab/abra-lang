#include "prelude.h"

#include "assert.h"
#include "string.h"

// For consistency, these are provided by the generated code
extern const size_t TYPE_ID_NONE;
extern const size_t TYPE_ID_INT;
extern const size_t TYPE_ID_FLOAT;
extern const size_t TYPE_ID_BOOL;
extern const size_t TYPE_ID_STRING;
extern const size_t TYPE_ID_ARRAY;
extern const size_t TYPE_ID_TUPLE;
extern const size_t TYPE_ID_SET;
extern const size_t TYPE_ID_MAP;

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

AbraString prelude__tostring(AbraAny value) {
  VTableEntry entry = VTABLE[value.type_id];
  AbraFnObj tostring = entry.methods[INTRINSIC_TOSTRING_IDX];

  // TODO: Support closures
  assert(!tostring.is_closure);
  AbraString (* tostring_method)(size_t, AbraAny) = (AbraString (*)(size_t, AbraAny)) (tostring.fn);
  return tostring_method(1, value);
}

AbraBool prelude__eq(AbraAny value, AbraAny other, bool neg) {
  VTableEntry entry = VTABLE[value.type_id];
  AbraBool (* eq_fn)(size_t, AbraAny, AbraAny) = (AbraBool (*)(size_t, AbraAny, AbraAny)) (entry.fn_eq.fn);

  AbraBool res = eq_fn(2, value, other);
  if (neg) return AbraBool_make(!res.value);
  return res;
}

AbraInt prelude__hash(AbraAny value) {
  VTableEntry entry = VTABLE[value.type_id];
  AbraInt (* hash_fn)(size_t, AbraAny) = (AbraInt (*)(size_t, AbraAny)) (entry.fn_hash.fn);

  return hash_fn(1, value);
}

AbraAny* copy_to_heap(AbraAny* value) {
  AbraAny* ref = malloc(sizeof(AbraAny));
  memcpy(ref, value, sizeof(AbraAny));
  return ref;
}

AbraFn AbraFn_make(Fn fn_ptr, size_t min_arity, size_t max_arity) {
  AbraFn fn = AbraFn_make_closure(fn_ptr, min_arity, max_arity, 0);
  fn.value->is_closure = false;
  return fn;
}

AbraFn AbraFn_make_closure(Fn fn_ptr, size_t min_arity, size_t max_arity, size_t ncaptures) {
  AbraFnObj* fn = malloc(sizeof(AbraFnObj));
  fn->is_closure = true;
  fn->min_arity = min_arity;
  fn->max_arity = max_arity;
  fn->fn = fn_ptr;

  fn->captures = malloc(sizeof(AbraAny*) * ncaptures);

  return (AbraFn) { .type_id=TYPE_ID_STRING, .value=fn };
}

inline AbraAny AbraFn_call_0(AbraFn fn, size_t nargs) {
  return (fn.value->is_closure)
         ? ((AbraAny (*)(size_t, AbraAny**)) (fn.value->fn))(nargs, fn.value->captures)
         : ((AbraAny (*)(size_t)) (fn.value->fn))(nargs);
}

inline AbraAny AbraFn_call_1(AbraFn fn, size_t nargs, AbraAny arg1) {
  if (fn.value->is_closure) {
    AbraAny (* f)(size_t, AbraAny**, AbraAny) = (AbraAny (*)(size_t, AbraAny**, AbraAny)) (fn.value->fn);
    return f(nargs, fn.value->captures, arg1);
  }

  AbraAny (* f)(size_t, AbraAny) = (AbraAny (*)(size_t, AbraAny)) (fn.value->fn);
  return f(nargs, arg1);
}

#define INITIALIZED_IN_ENTRYPOINT {0}

// AbraNone methods
AbraFnObj NONE_METHODS[] = {
    METHOD(AbraNone__toString, 1, 1)
};

AbraAny AbraNone = INITIALIZED_IN_ENTRYPOINT;

static AbraString ABRA_NONE_STRING = INITIALIZED_IN_ENTRYPOINT;

AbraString AbraNone__toString(size_t nargs, AbraAny self) {
  assert(self.type_id == TYPE_ID_NONE);
  assert(nargs == 1);

  return ABRA_NONE_STRING;
}

AbraBool AbraNone__eq(size_t nargs, AbraAny self, AbraAny other) {
  assert(self.type_id == TYPE_ID_NONE);
  assert(nargs == 2);

  return AbraBool_make(IS_NONE(other));
}

AbraInt AbraNone__hash(size_t nargs, AbraAny self) {
  assert(self.type_id == TYPE_ID_NONE);
  assert(nargs == 1);

  return AbraInt_make(INT64_MAX);
}

// AbraInt methods
AbraFnObj INT_METHODS[] = {
    METHOD(AbraInt__toString, 1, 1)
};

AbraString AbraInt__toString(size_t nargs, AbraInt self) {
  assert(self.type_id == TYPE_ID_INT);
  assert(nargs == 1);

  int len = snprintf(NULL, 0, "%" PRId64, self.value);
  char* str = malloc(len + 1);
  snprintf(str, len + 1, "%" PRId64, self.value);

  return AbraString_make(len, str);
}

AbraBool AbraInt__eq(size_t nargs, AbraInt self, AbraAny other) {
  assert(self.type_id == TYPE_ID_INT);
  assert(nargs == 2);

  if (other.type_id == TYPE_ID_INT)
    return AbraBool_make(self.value == REINTERPRET_CAST(other, AbraInt).value);
  if (other.type_id == TYPE_ID_FLOAT)
    return AbraBool_make((double) self.value == REINTERPRET_CAST(other, AbraFloat).value);

  return ABRA_BOOL_FALSE;
}

AbraInt AbraInt__hash(size_t nargs, AbraInt self) {
  assert(self.type_id == TYPE_ID_INT);
  assert(nargs == 1);

  return AbraInt_make(self.value * 7781);
}

// AbraFloat methods
AbraFnObj FLOAT_METHODS[] = {
    METHOD(AbraFloat__toString, 1, 1)
};

AbraString AbraFloat__toString(size_t nargs, AbraFloat self) {
  assert(self.type_id == TYPE_ID_FLOAT);
  assert(nargs == 1);

  int len = snprintf(NULL, 0, "%f", self.value);
  char* str = malloc(len + 1);
  snprintf(str, len + 1, "%f", self.value);
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

AbraBool AbraFloat__eq(size_t nargs, AbraFloat self, AbraAny other) {
  assert(self.type_id == TYPE_ID_FLOAT);
  assert(nargs == 2);

  if (other.type_id == TYPE_ID_INT)
    return AbraBool_make(self.value == (double) (REINTERPRET_CAST(other, AbraInt).value));
  if (other.type_id == TYPE_ID_FLOAT)
    return AbraBool_make(self.value == REINTERPRET_CAST(other, AbraFloat).value);

  return ABRA_BOOL_FALSE;
}

AbraInt AbraFloat__hash(size_t nargs, AbraFloat self) {
  assert(self.type_id == TYPE_ID_FLOAT);
  assert(nargs == 1);

  return AbraInt_make((int64_t) (self.value * 1000000000000 * 839));
}

// AbraBool methods
AbraFnObj BOOL_METHODS[] = {
    METHOD(AbraBool__toString, 1, 1)
};

AbraBool ABRA_BOOL_TRUE = INITIALIZED_IN_ENTRYPOINT;
AbraBool ABRA_BOOL_FALSE = INITIALIZED_IN_ENTRYPOINT;

static AbraString ABRA_BOOL_TRUE_STRING = INITIALIZED_IN_ENTRYPOINT;
static AbraString ABRA_BOOL_FALSE_STRING = INITIALIZED_IN_ENTRYPOINT;

AbraString AbraBool__toString(size_t nargs, AbraBool self) {
  assert(self.type_id == TYPE_ID_BOOL);
  assert(nargs == 1);
  return self.value ? ABRA_BOOL_TRUE_STRING : ABRA_BOOL_FALSE_STRING;
}

AbraBool AbraBool__eq(size_t nargs, AbraBool self, AbraAny other) {
  assert(self.type_id == TYPE_ID_BOOL);
  assert(nargs == 2);
  return AbraBool_make(other.type_id == TYPE_ID_BOOL && self.value == REINTERPRET_CAST(other, AbraBool).value);
}

AbraInt AbraBool__hash(size_t nargs, AbraBool self) {
  assert(self.type_id == TYPE_ID_BOOL);
  assert(nargs == 1);

  return AbraInt_make(self.value ? 42643801 : 43112609);
}

// AbraString methods
AbraFnObj STRING_METHODS[] = {
    METHOD(AbraString__toString, 1, 1)
};

AbraString AbraString_make(size_t len, char* chars) {
  AbraString_Inner* value = malloc(sizeof(AbraString_Inner));
  value->length = (int64_t) len;
  value->chars = chars;
  return (AbraString) { .type_id=TYPE_ID_STRING, .value=value };
}

AbraString AbraString_empty_string() {
  return AbraString_make(0, "");
}

AbraString AbraString_get(AbraString self, int64_t index) {
  assert(self.type_id == TYPE_ID_STRING);

  int64_t len = self.value->length;
  if (index < -len || index >= len) return AbraString_empty_string();

  if (index < 0) index += len;

  char* str = malloc(sizeof(char));
  str[0] = self.value->chars[index];
  return AbraString_make(1, str);
}

AbraString AbraString_slice(AbraString self, int64_t index) {
  assert(self.type_id == TYPE_ID_STRING);

  if (index >= self.value->length) return AbraString_empty_string();
  if (index < 0) index = 0;

  size_t len = self.value->length - index;
  char* str = malloc(sizeof(char) * len + 1);
  str[len] = 0;

  memcpy(str, self.value->chars + index, len);
  memset(self.value->chars + index, 0, len);

  return AbraString_make(len, str);
}

AbraString AbraString_get_range(AbraString self, int64_t start, int64_t end) {
  assert(self.type_id == TYPE_ID_STRING);

  int64_t len = self.value->length;
  RANGE_ENDPOINTS(start, end, len);

  if (start >= end) return AbraString_empty_string();

  int64_t slice_size = end - start;
  char* str = malloc(slice_size);
  memcpy(str, self.value->chars + start, slice_size);
  str[slice_size] = 0;

  return AbraString_make(slice_size, str);
}

AbraString AbraString__toString(size_t nargs, AbraString self) {
  assert(self.type_id == TYPE_ID_STRING);
  assert(nargs == 1);

  return self;
}

AbraBool AbraString__eq(size_t nargs, AbraString self, AbraAny other) {
  assert(self.type_id == TYPE_ID_STRING);
  assert(nargs == 2);

  return AbraBool_make(
      other.type_id == TYPE_ID_STRING &&
      self.value->length == REINTERPRET_CAST(other, AbraString).value->length &&
      (strcmp(self.value->chars, REINTERPRET_CAST(other, AbraString).value->chars) == 0)
  );
}

AbraInt AbraString__hash(size_t nargs, AbraString self) {
  assert(self.type_id == TYPE_ID_STRING);
  assert(nargs == 1);

  // Adapted from djb2 hashing algorithm
  size_t hash = 5381;
  for (size_t i = 0; i < self.value->length; ++i) {
    hash = ((hash << 5) + hash) ^ self.value->chars[i];
  }
  return AbraInt_make((int64_t) hash);
}

AbraString AbraString__concat(size_t nargs, AbraString self, AbraAny other) {
  assert(self.type_id == TYPE_ID_STRING);
  assert(nargs == 2);

  AbraString repr = PRELUDE_TOSTRING(other);

  size_t len = repr.value->length + self.value->length;
  char* concat = malloc(sizeof(char) * len + 1);
  concat[len] = 0;

  memcpy(concat, self.value->chars, self.value->length);
  memcpy(concat + self.value->length, repr.value->chars, repr.value->length);
  return AbraString_make(len, concat);
}

// AbraArray methods
AbraFnObj ARRAY_METHODS[] = {
    METHOD(AbraArray__toString, 1, 1)
};

AbraArray AbraArray_make_with_capacity(size_t length, size_t cap) {
  AbraArray_Inner* value = malloc(sizeof(AbraArray_Inner));
  value->items = malloc(sizeof(AbraAny) * cap);
  value->length = (int64_t) length;
  value->_capacity = (int64_t) cap;
  return (AbraArray) { .type_id=TYPE_ID_ARRAY, .value=value };
}

AbraArray AbraArray_empty_array() {
  return AbraArray_make_with_capacity(0, 1);
}

AbraUnit AbraArray_set(AbraArray self, int64_t index, AbraAny item) {
  assert(self.type_id == TYPE_ID_ARRAY);

  int64_t len = self.value->length;
  if (index < -len) return;
  if (index < 0) index += len;

  if (index >= len) {
    while (index >= self.value->_capacity) {
      self.value->_capacity *= 2;
    }

    AbraAny* new_items = (AbraAny*) realloc(self.value->items, sizeof(AbraAny) * self.value->_capacity);
    assert(new_items);
    self.value->items = new_items;

    for (size_t i = len; i <= index; i++) {
      self.value->items[i] = AbraNone;
      self.value->length += 1;
    }
  }

  self.value->items[index] = item;
}

AbraAny AbraArray_get(AbraArray self, int64_t index) {
  assert(self.type_id == TYPE_ID_ARRAY);

  int64_t len = self.value->length;
  if (index < -len || index >= len) return AbraNone;

  if (index < 0) index += len;
  return self.value->items[index];
}

AbraArray AbraArray_slice(AbraArray self, int64_t index) {
  assert(self.type_id == TYPE_ID_ARRAY);

  if (index >= self.value->length) return AbraArray_empty_array();
  if (index < 0) index = 0;

  size_t len = self.value->length - index;
  AbraArray slice = AbraArray_make_with_capacity(len, len);

  size_t self_length = self.value->length;
  for (size_t i = index; i < self_length; i++) {
    slice.value->items[i - index] = self.value->items[i];
    self.value->items[i] = AbraNone;
    self.value->length -= 1;
  }

  return slice;
}

AbraArray AbraArray_get_range(AbraArray self, int64_t start, int64_t end) {
  assert(self.type_id == TYPE_ID_ARRAY);

  int64_t len = self.value->length;
  RANGE_ENDPOINTS(start, end, len);

  if (start >= end) return AbraArray_empty_array();

  int64_t slice_size = end - start;
  AbraArray new_array = AbraArray_make_with_capacity(slice_size, slice_size);
  for (int64_t i = start; i < end; i++) {
    new_array.value->items[i - start] = self.value->items[i];
  }
  return new_array;
}

AbraString sequence_to_string(int64_t length, AbraAny* items, char pad_start, char start, char end) {
  if (length == 0) {
    size_t len = pad_start ? 3 : 2;
    char* empty = malloc(sizeof(char) * len + 1);
    memset(empty, 0, len + 1);
    size_t cursor = 0;
    if (pad_start != 0)
      empty[cursor++] = (char)pad_start;
    empty[cursor] = start;
    empty[cursor + 1] = end;
    return AbraString_make(len, empty);
  }

  typedef struct item_t {
      AbraString repr;
      bool item_is_string;
  } item_t;

  item_t strings[length];
  size_t total_length = pad_start ? 3 : 2; // account for "[]", or for leading "#" in sets
  for (size_t i = 0; i < length; i++) {
    AbraAny item = items[i];
    AbraString repr = PRELUDE_TOSTRING(item);

    strings[i] = (item_t) {
        .repr=repr,
        .item_is_string=item.type_id == TYPE_ID_STRING
    };
    total_length += repr.value->length;
    if (item.type_id == TYPE_ID_STRING) {
      total_length += 2; // account for '""'
    }
    if (i != length - 1) {
      total_length += 2; // account for ", "
    }
  }

  char* res_str = malloc(sizeof(char) * total_length + 1);
  res_str[total_length - 1] = end;
  res_str[total_length] = 0;

  char* ptr = res_str;
  if (pad_start) {
    *ptr = pad_start;
    ptr++;
  }
  *ptr = start;
  ptr++;

  for (int i = 0; i < length; i++) {
    bool item_is_string = strings[i].item_is_string;
    AbraString repr = strings[i].repr;
    size_t len = repr.value->length;
    if (item_is_string) memcpy(ptr++, "\"", 1);
    memcpy(ptr, repr.value->chars, len);
    ptr += len;
    if (item_is_string) memcpy(ptr++, "\"", 1);

    if (i < length - 1) {
      memcpy(ptr, ", ", 2);
      ptr += 2;
    }
  }

  return AbraString_make(total_length, res_str);
}

AbraString AbraArray__toString(size_t nargs, AbraArray self) {
  assert(self.type_id == TYPE_ID_ARRAY);
  assert(nargs == 1);

  return sequence_to_string(self.value->length, self.value->items, false, '[', ']');
}

AbraBool AbraArray__eq(size_t nargs, AbraArray self, AbraAny other) {
  assert(self.type_id == TYPE_ID_ARRAY);
  assert(nargs == 2);

  if (other.type_id != TYPE_ID_ARRAY) return ABRA_BOOL_FALSE;
  if (self.value->length != REINTERPRET_CAST(other, AbraArray).value->length) return ABRA_BOOL_FALSE;

  for (size_t i = 0; i < self.value->length; i++) {
    if (!PRELUDE_EQ(self.value->items[i], REINTERPRET_CAST(other, AbraArray).value->items[i]).value)
      return ABRA_BOOL_FALSE;
  }

  return ABRA_BOOL_TRUE;
}

AbraInt AbraArray__hash(size_t nargs, AbraArray self) {
  assert(self.type_id == TYPE_ID_ARRAY);
  assert(nargs == 1);

  // Adapted from djb2 hashing algorithm
  size_t hash = 5381;
  for (int i = 0; i < self.value->length; ++i) {
    hash = ((hash << 5) + hash) ^ PRELUDE_HASH(self.value->items[i]).value;
  }
  return AbraInt_make(hash);
}

// AbraTuple methods
AbraFnObj TUPLE_METHODS[] = {
    METHOD(AbraTuple__toString, 1, 1)
};

AbraTuple AbraTuple_make(size_t length, ...) {
  AbraTuple_Inner* inner = malloc(sizeof(AbraTuple_Inner));
  inner->length = (int64_t) length;
  inner->items = malloc(sizeof(AbraAny) * length);

  va_list items;
  va_start(items, length);
  for (size_t i = 0; i < length; i++) {
    AbraAny item = va_arg(items, AbraAny);
    inner->items[i] = item;
  }
  va_end(items);
  return (AbraTuple) { .type_id=TYPE_ID_TUPLE, .value=inner };
}

AbraAny AbraTuple_get(AbraTuple self, int64_t index) {
  return self.value->items[index];
}

AbraString AbraTuple__toString(size_t nargs, AbraTuple self) {
  assert(self.type_id == TYPE_ID_TUPLE);
  assert(nargs == 1);

  return sequence_to_string(self.value->length, self.value->items, false, '(', ')');
}

AbraBool AbraTuple__eq(size_t nargs, AbraTuple self, AbraAny other) {
  assert(self.type_id == TYPE_ID_TUPLE);
  assert(nargs == 2);

  if (other.type_id != TYPE_ID_TUPLE) return ABRA_BOOL_FALSE;
  if (self.value->length != REINTERPRET_CAST(other, AbraTuple).value->length) return ABRA_BOOL_FALSE;

  for (size_t i = 0; i < self.value->length; i++) {
    if (!PRELUDE_EQ(self.value->items[i], REINTERPRET_CAST(other, AbraTuple).value->items[i]).value)
      return ABRA_BOOL_FALSE;
  }

  return ABRA_BOOL_TRUE;
}

AbraInt AbraTuple__hash(size_t nargs, AbraTuple self) {
  assert(self.type_id == TYPE_ID_TUPLE);
  assert(nargs == 1);

  // Adapted from djb2 hashing algorithm
  size_t hash = 4253;
  for (int i = 0; i < self.value->length; ++i) {
    hash = ((hash << 5) + hash) ^ PRELUDE_HASH(self.value->items[i]).value;
  }
  return AbraInt_make(hash);
}

// AbraSet methods
AbraFnObj SET_METHODS[] = {
    METHOD(AbraSet__toString, 1, 1)
};

AbraSet AbraSet_make() {
  hashmap_t h = new_hashmap();
  hashmap_t* hh = malloc(sizeof(hashmap_t));
  memcpy(hh, &h, sizeof(hashmap_t));
  return (AbraSet) {.type_id=TYPE_ID_SET, .value=hh};
}

AbraUnit AbraSet_insert(AbraSet self, AbraAny value) {
  hashmap_insert(self.value, value, REINTERPRET_CAST(ABRA_BOOL_TRUE, AbraAny));
}

AbraString AbraSet__toString(size_t nargs, AbraSet self) {
  assert(self.type_id == TYPE_ID_SET);
  assert(nargs == 1);

  return sequence_to_string((int64_t) self.value->size, hashmap_keys(self.value), '#', '{', '}');
}

AbraBool AbraSet__eq(size_t nargs, AbraSet self, AbraAny _other) {
  assert(self.type_id == TYPE_ID_SET);
  assert(nargs == 2);

  if (_other.type_id != TYPE_ID_SET) return ABRA_BOOL_FALSE;
  AbraSet other = REINTERPRET_CAST(_other, AbraSet);

  if (self.value->size != other.value->size) return ABRA_BOOL_FALSE;
  AbraAny* self_keys = hashmap_keys(self.value);
  for (size_t i = 0; i < self.value->size; i++) {
    AbraAny other_value = hashmap_get(other.value, self_keys[i]);
    if (other_value.type_id != TYPE_ID_BOOL) {
      return ABRA_BOOL_FALSE;
    }
  }

  return ABRA_BOOL_TRUE;
}

AbraInt AbraSet__hash(size_t nargs, AbraSet self) {
  assert(self.type_id == TYPE_ID_SET);
  assert(nargs == 1);

  // Adapted from djb2 hashing algorithm
  size_t hash = 4253;
  AbraAny* keys = hashmap_keys(self.value);
  for (int i = 0; i < self.value->size; ++i) {
    AbraAny key = keys[i];
    hash = ((hash << 5) + hash) ^ PRELUDE_HASH(key).value;
  }
  return AbraInt_make(hash);
}

// AbraMap methods
AbraFnObj MAP_METHODS[] = {
    METHOD(AbraMap__toString, 1, 1)
};

AbraMap AbraMap_make() {
  hashmap_t h = new_hashmap();
  hashmap_t* hh = malloc(sizeof(hashmap_t));
  memcpy(hh, &h, sizeof(hashmap_t));
  return (AbraMap) {.type_id=TYPE_ID_MAP, .value=hh};
}

AbraUnit AbraMap_set(AbraMap self, AbraAny key, AbraAny value) {
  hashmap_insert(self.value, key, value);
}

AbraAny AbraMap_get(AbraMap self, AbraAny key) {
  return hashmap_get(self.value, key);
}

AbraString AbraMap__toString(size_t nargs, AbraMap self) {
  assert(self.type_id == TYPE_ID_MAP);
  assert(nargs == 1);

  size_t length = self.value->size;
  if (length == 0) return AbraString_make(2, "{}");

  typedef struct item_t {
      AbraString key_repr;
      AbraString val_repr;
      bool key_is_string;
      bool val_is_string;
  } item_t;

  AbraAny* keys = hashmap_keys(self.value);
  item_t strings[length];
  size_t total_length = 4; // account for "{ [items here] }"
  for (size_t i = 0; i < length; i++) {
    AbraAny key = keys[i];
    AbraString key_repr = PRELUDE_TOSTRING(key);
    AbraAny val = hashmap_get(self.value, key);
    AbraString val_repr = PRELUDE_TOSTRING(val);

    strings[i] = (item_t) {
        .key_repr=key_repr,
        .key_is_string=key.type_id == TYPE_ID_STRING,
        .val_repr=val_repr,
        .val_is_string=val.type_id == TYPE_ID_STRING,
    };
    total_length += key_repr.value->length;
    if (key.type_id == TYPE_ID_STRING) {
      total_length += 2; // account for '""'
    }
    total_length += val_repr.value->length;
    if (val.type_id == TYPE_ID_STRING) {
      total_length += 2; // account for '""'
    }
    total_length += 2; // account for ": "
    if (i != length - 1) {
      total_length += 2; // account for ", "
    }
  }

  char* res_str = malloc(sizeof(char) * total_length + 1);
  res_str[0] = '{';
  res_str[1] = ' ';
  res_str[total_length - 2] = ' ';
  res_str[total_length - 1] = '}';
  res_str[total_length] = 0;

  char* ptr = res_str + 2;
  for (int i = 0; i < length; i++) {
    bool key_is_string = strings[i].key_is_string;
    AbraString key_repr = strings[i].key_repr;
    size_t len = key_repr.value->length;
    if (key_is_string) memcpy(ptr++, "\"", 1);
    memcpy(ptr, key_repr.value->chars, len);
    ptr += len;
    if (key_is_string) memcpy(ptr++, "\"", 1);

    memcpy(ptr, ": ", 2);
    ptr += 2;

    bool val_is_string = strings[i].val_is_string;
    AbraString val_repr = strings[i].val_repr;
    len = val_repr.value->length;
    if (val_is_string) memcpy(ptr++, "\"", 1);
    memcpy(ptr, val_repr.value->chars, len);
    ptr += len;
    if (val_is_string) memcpy(ptr++, "\"", 1);

    if (i < length - 1) {
      memcpy(ptr, ", ", 2);
      ptr += 2;
    }
  }

  return AbraString_make(total_length, res_str);
}

AbraBool AbraMap__eq(size_t nargs, AbraMap self, AbraAny _other) {
  assert(self.type_id == TYPE_ID_MAP);
  assert(nargs == 2);

  if (_other.type_id != TYPE_ID_MAP) return ABRA_BOOL_FALSE;
  AbraMap other = REINTERPRET_CAST(_other, AbraMap);

  if (self.value->size != other.value->size) return ABRA_BOOL_FALSE;
  AbraAny* self_keys = hashmap_keys(self.value);
  for (size_t i = 0; i < self.value->size; i++) {
    AbraAny self_value = hashmap_get(self.value, self_keys[i]);
    AbraAny other_value = hashmap_get(other.value, self_keys[i]);
    if (!PRELUDE_EQ(self_value, other_value).value) {
      return ABRA_BOOL_FALSE;
    }
  }

  return ABRA_BOOL_TRUE;
}

AbraInt AbraMap__hash(size_t nargs, AbraMap self) {
  assert(self.type_id == TYPE_ID_MAP);
  assert(nargs == 1);

  // Adapted from djb2 hashing algorithm
  size_t hash = 4253;
  AbraAny* keys = hashmap_keys(self.value);
  for (int i = 0; i < self.value->size; ++i) {
    AbraAny key = keys[i];
    AbraAny val = hashmap_get(self.value, key);
    hash = ((hash << 5) + hash) ^ PRELUDE_HASH(key).value;
    hash = ((hash << 5) + hash) ^ PRELUDE_HASH(val).value;
  }
  return AbraInt_make(hash);
}

AbraUnit _0_0_0__print(size_t nargs, AbraArray args) {
  assert(nargs == 1);

  if (args.value->length == 0) {
    printf("\n");
    return;
  }

  for (size_t i = 0; i < args.value->length; i++) {
    AbraString repr = PRELUDE_TOSTRING(args.value->items[i]);

    printf("%s", repr.value->chars);
    if (i != args.value->length - 1) {
      printf(" ");
    }
  }
}

AbraUnit _0_0_1__println(size_t nargs, AbraArray args) {
  _0_0_0__print(nargs, args);
  printf("\n");
}

void entrypoint__0() {
  assert(sizeof(AbraInt) == REQUIRED_VALUE_SIZE);
  assert(sizeof(AbraFloat) == REQUIRED_VALUE_SIZE);
  assert(sizeof(AbraBool) == REQUIRED_VALUE_SIZE);
  assert(sizeof(AbraString) == REQUIRED_VALUE_SIZE);
  assert(sizeof(AbraArray) == REQUIRED_VALUE_SIZE);
  assert(sizeof(AbraTuple) == REQUIRED_VALUE_SIZE);
  assert(sizeof(AbraSet) == REQUIRED_VALUE_SIZE);
  assert(sizeof(AbraMap) == REQUIRED_VALUE_SIZE);

  // Pre-allocated cached values
  AbraNone = (AbraAny) { .type_id=TYPE_ID_NONE, .data=NULL };
  ABRA_NONE_STRING = AbraString_make(4, "None");
  ABRA_BOOL_TRUE = (AbraBool) { .type_id=TYPE_ID_BOOL, .value=true };
  ABRA_BOOL_FALSE = (AbraBool) { .type_id=TYPE_ID_BOOL, .value=false };
  ABRA_BOOL_TRUE_STRING = AbraString_make(4, "true");
  ABRA_BOOL_FALSE_STRING = AbraString_make(5, "false");

  VTABLE[TYPE_ID_NONE]   = (VTableEntry) { .fn_eq=METHOD(AbraNone__eq, 2, 2),   .fn_hash=METHOD(AbraNone__hash, 1, 1),   .methods=NONE_METHODS };
  VTABLE[TYPE_ID_INT]    = (VTableEntry) { .fn_eq=METHOD(AbraInt__eq, 2, 2),    .fn_hash=METHOD(AbraInt__hash, 1, 1),    .methods=INT_METHODS };
  VTABLE[TYPE_ID_FLOAT]  = (VTableEntry) { .fn_eq=METHOD(AbraFloat__eq, 2, 2),  .fn_hash=METHOD(AbraFloat__hash, 1, 1),  .methods=FLOAT_METHODS };
  VTABLE[TYPE_ID_BOOL]   = (VTableEntry) { .fn_eq=METHOD(AbraBool__eq, 2, 2),   .fn_hash=METHOD(AbraBool__hash, 1, 1),   .methods=BOOL_METHODS };
  VTABLE[TYPE_ID_STRING] = (VTableEntry) { .fn_eq=METHOD(AbraString__eq, 2, 2), .fn_hash=METHOD(AbraString__hash, 1, 1), .methods=STRING_METHODS };
  VTABLE[TYPE_ID_ARRAY]  = (VTableEntry) { .fn_eq=METHOD(AbraArray__eq, 2, 2),  .fn_hash=METHOD(AbraArray__hash, 1, 1),  .methods=ARRAY_METHODS };
  VTABLE[TYPE_ID_TUPLE]  = (VTableEntry) { .fn_eq=METHOD(AbraTuple__eq, 2, 2),  .fn_hash=METHOD(AbraTuple__hash, 1, 1),  .methods=TUPLE_METHODS };
  VTABLE[TYPE_ID_SET]    = (VTableEntry) { .fn_eq=METHOD(AbraSet__eq, 2, 2),    .fn_hash=METHOD(AbraSet__hash, 1, 1),    .methods=SET_METHODS };
  VTABLE[TYPE_ID_MAP]    = (VTableEntry) { .fn_eq=METHOD(AbraMap__eq, 2, 2),    .fn_hash=METHOD(AbraMap__hash, 1, 1),    .methods=MAP_METHODS };
}
