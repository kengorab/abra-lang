#include "rt.h"

// ------------------------ VTABLE ------------------------
value_t* vtable_alloc_entry(uint32_t type_id, uint32_t size) {
  value_t* entry = GC_MALLOC(sizeof(value_t) * size);
  vtable[type_id] = entry;
  return entry;
}

value_t vtable_lookup(value_t value, uint32_t idx) {
  uint32_t type_id = type_id_for_val(value);
  return vtable[type_id][idx];
}

// ------------------------ TYPE MANAGEMENT ------------------------
uint32_t type_id_for_val(value_t value) {
  if (IS_INT(value)) return type_id_Int;
  if (IS_FLOAT(value)) return type_id_Float;
  if (value == VAL_TRUE || value == VAL_FALSE) return type_id_Bool;

  obj_header_t* header = AS_OBJ(value, obj_header_t);
  return header->type_id;
}

// ------------------------ INTRINSICS ------------------------
void range_endpoints(int32_t len, int32_t* start, int32_t* end) {
  if (*start < 0) {
    *start += len;
  } else if (len == 0) {
    *start = 0;
  }

  if (*end < 0) {
    *end += len;
  } else if (*end < *start) {
    *end = *start;
  } else if (*end >= len) {
    *end = len;
  }
}

value_t value_to_string(value_t value) {
  if (value == VAL_NONE) return string_alloc(4, "None");

  tostring_method_t tostring_method = (tostring_method_t)vtable_lookup(value, TOSTRING_IDX);
  return tostring_method(NULL, 1, value);
}

value_t values_to_string(
  int32_t length,
  value_t* values,
  int32_t prefix_len, char* prefix,
  int32_t suffix_len, char* suffix,
  int32_t sep_len, char* sep
) {
  if (length == 0) {
    char* empty_str = GC_MALLOC(sizeof(char) * (prefix_len + suffix_len));
    memcpy(empty_str, prefix, prefix_len);
    memcpy(empty_str + prefix_len, suffix, suffix_len);
    return string_alloc(prefix_len + suffix_len, empty_str);
  }

  String** strings = GC_MALLOC(sizeof(String*) * length);
  int32_t total_length = prefix_len + suffix_len;
  for (int i = 0; i < length; i++) {
    value_t str_val = value_to_string(values[i]);
    String* s = AS_OBJ(str_val, String);
    strings[i] = s;
    total_length += s->size;
    if (i != length - 1) {
      total_length += sep_len;
    }
  }

  char* chars = GC_MALLOC(sizeof(char) * total_length);
  memcpy(chars, prefix, prefix_len);
  int32_t offset = prefix_len;
  for (int i = 0; i < length; i++) {
    memcpy(chars + offset, strings[i]->chars, strings[i]->size);
    offset += strings[i]->size;

    if (i != length - 1) {
      memcpy(chars + offset, sep, sep_len);
      offset += sep_len;
    }
  }
  memcpy(chars + offset, suffix, suffix_len);

  return string_alloc(total_length, chars);
}

value_t value_eq(value_t v1, value_t v2) {
  if ((IS_OBJ(v1) && !IS_OBJ(v2)) || (!IS_OBJ(v1) && IS_OBJ(v2))) {
    return VAL_FALSE;
  }

  if (IS_OBJ(v1) && IS_OBJ(v2)) {
    uint32_t tid1 = AS_OBJ(v1, obj_header_t)->type_id;
    uint32_t tid2 = AS_OBJ(v2, obj_header_t)->type_id;

    if (tid1 != tid2) return VAL_FALSE;

    if (tid1 == type_id_String) {
      String* self = AS_OBJ(v1, String);
      String* other = AS_OBJ(v2, String);

      if (self->size != other->size) return VAL_FALSE;

      for (int i = 0; i < self->size; ++i) {
        if (self->chars[i] != other->chars[i]) return VAL_FALSE;
      }

      return VAL_TRUE;
    }

    if (tid1 == type_id_Array || tid1 == type_id_Tuple) {
      int32_t len1 = (tid1 == type_id_Array) ? AS_OBJ(v1, Array)->length : AS_OBJ(v1, Tuple)->length;
      int32_t len2 = (tid1 == type_id_Array) ? AS_OBJ(v2, Array)->length : AS_OBJ(v2, Tuple)->length;

      if (len1 != len2) return VAL_FALSE;

      value_t* items1 = (tid1 == type_id_Array) ? AS_OBJ(v1, Array)->items : AS_OBJ(v1, Tuple)->items;
      value_t* items2 = (tid1 == type_id_Array) ? AS_OBJ(v2, Array)->items : AS_OBJ(v2, Tuple)->items;
      for (int i = 0; i < len1; i++) {
        value_t eq = value_eq(items1[i], items2[i]);
        if (eq == VAL_FALSE) return VAL_FALSE;
      }

      return VAL_TRUE;
    }

    if (tid1 == type_id_Function) {
      return VAL_FALSE;
    }

    eq_method_t eq_method = (eq_method_t) vtable_lookup(v1, EQ_IDX);
    return eq_method(NULL, 2, v1, v2);
  } else if (IS_INT(v1) && IS_FLOAT(v2)) {
    double d1 = (double)AS_INT(v1);
    double d2 = AS_DOUBLE(v2);
    return d1 == d2 ? VAL_TRUE : VAL_FALSE;
  } else if (IS_FLOAT(v1) && IS_INT(v2)) {
    double d1 = AS_DOUBLE(v1);
    double d2 = (double)AS_INT(v2);
    return d1 == d2 ? VAL_TRUE : VAL_FALSE;
  } else {
    return v1 == v2 ? VAL_TRUE : VAL_FALSE;
  }
}

// ------------------------ INT ------------------------
value_t prelude__Int__toString(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  int32_t self = AS_INT(_self);

  int len = snprintf(NULL, 0, "%d", self);
  char *result = GC_MALLOC(len + 1);
  snprintf(result, len + 1, "%d", self);

  return string_alloc(len, result);
}

// ------------------------ FLOAT ------------------------
value_t prelude__Float__toString(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  double self = AS_DOUBLE(_self);

  int len = snprintf(NULL, 0, "%f", self);
  char *result = GC_MALLOC(len + 1);
  snprintf(result, len + 1, "%f", self);

  return string_alloc(len, result);
}

// ------------------------ BOOL ------------------------
value_t prelude__Bool__toString(value_t* _env, int8_t _num_rcv_args, value_t self) {
  if (self == VAL_TRUE) return string_alloc(4, "true");
  return string_alloc(5, "false");
}

// ------------------------ STRING ------------------------
value_t string_alloc(int32_t length, char* chars) {
  String* string = GC_MALLOC(sizeof(String));

  string->h.type_id = type_id_String;
  string->size = length;
  string->chars = chars;
  return TAG_OBJ(string);
}

value_t string_concat(value_t _s1, value_t _s2) {
  String* s1 = AS_OBJ(value_to_string(_s1), String);
  String* s2 = AS_OBJ(value_to_string(_s2), String);

  char* chars = GC_MALLOC(sizeof(char) * (s1->size + s2->size));
  memcpy(chars, s1->chars, s1->size);
  memcpy(chars + s1->size, s2->chars, s2->size);
  return string_alloc(s1->size + s2->size, chars);
}

value_t string_get(value_t _self, int32_t idx) {
  if (_self == VAL_NONE) return _self;

  String* self = AS_OBJ(_self, String);

  if (idx < -self->size || idx >= self->size) return VAL_NONE;

  char* s = GC_MALLOC(sizeof(char) * 1);
  s[0] = idx < 0 ? self->chars[idx + self->size] : self->chars[idx];
  return string_alloc(1, s);
}

value_t string_range(value_t _self, value_t _start, value_t _end) {
  String* self = AS_OBJ(_self, String);
  int32_t start = _start == VAL_NONE ? 0 : AS_INT(_start);
  int32_t end = _end == VAL_NONE ? self->size : AS_INT(_end);
  range_endpoints(self->size, &start, &end);

  if (start >= end) return string_alloc(0, "");

  int32_t slice_size = end - start;
  char* tmp = GC_MALLOC(sizeof(char) * slice_size);
  memcpy(tmp, self->chars + (start * sizeof(char)), slice_size);

  return string_alloc(slice_size, tmp);
}

value_t string_split(value_t _self, int32_t idx) {
  String* self = AS_OBJ(_self, String);

  if (idx >= self->size) {
    char* left_chars = GC_MALLOC(sizeof(char) * self->size);
    memcpy(left_chars, self->chars, self->size);
    value_t left = string_alloc(self->size, left_chars);
    value_t right = string_alloc(0, "");
    return tuple_alloc(2, left, right);
  }
  if (idx < (-self->size)) {
    value_t left = string_alloc(0, "");
    char* right_chars = GC_MALLOC(sizeof(char) * self->size);
    memcpy(right_chars, self->chars, self->size);
    value_t right = string_alloc(self->size, right_chars);
    return tuple_alloc(2, left, right);
  }

  int32_t split_idx = (self->size + idx) % self->size;

  char* left_chars = GC_MALLOC(sizeof(char) * split_idx);
  memcpy(left_chars, self->chars, split_idx);
  value_t left = string_alloc(split_idx, left_chars);

  char* right_chars = GC_MALLOC(sizeof(char) * (self->size - split_idx));
  memcpy(right_chars, self->chars + split_idx, self->size - split_idx);
  value_t right = string_alloc(self->size - split_idx, right_chars);

  return tuple_alloc(2, left, right);
}

value_t prelude__String__toString(value_t* _env, int8_t _num_rcv_args, value_t self) {
  return self;
}

value_t prelude__String__toLower(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  String* self = AS_OBJ(_self, String);

  char* chars = GC_MALLOC(sizeof(char) * self->size);
  for (int i = 0; i < self->size; ++i) {
    char ch = self->chars[i];
    chars[i] = (ch >= 'A' && ch <= 'Z') ? ch + 32 : ch;
  }

  return string_alloc(self->size, chars);
}

value_t prelude__String__toUpper(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  String* self = AS_OBJ(_self, String);

  char* chars = GC_MALLOC(sizeof(char) * self->size);
  for (int i = 0; i < self->size; ++i) {
    char ch = self->chars[i];
    chars[i] = (ch >= 'a' && ch <= 'z') ? ch - 32 : ch;
  }

  return string_alloc(self->size, chars);
}

// ------------------------ ARRAY ------------------------
value_t array_alloc(int32_t length) {
  Array* array = GC_MALLOC(sizeof(Array));
  value_t* array_items = GC_MALLOC(sizeof(value_t) * length);

  array->h.type_id = type_id_Array;
  array->length = length;
  array->capacity = length;
  array->items = array_items;
  return TAG_OBJ(array);
}

void array_insert(value_t _self, value_t _idx, value_t item) {
  Array* self = AS_OBJ(_self, Array);
  int32_t idx = AS_INT(_idx);

  self->items[idx] = item;
}

value_t array_get(value_t _self, int32_t idx) {
  if (_self == VAL_NONE) return _self;

  Array* self = AS_OBJ(_self, Array);

  if (idx < -self->length || idx >= self->length) return VAL_NONE;
  if (idx < 0) return self->items[idx + self->length];
  return self->items[idx];
}

value_t array_range(value_t _self, value_t _start, value_t _end) {
  Array* self = AS_OBJ(_self, Array);
  int32_t start = _start == VAL_NONE ? 0 : AS_INT(_start);
  int32_t end = _end == VAL_NONE ? self->length : AS_INT(_end);
  range_endpoints(self->length, &start, &end);

  if (start >= end) return array_alloc(0);

  int32_t slice_size = end - start;
  value_t sub_array_v = array_alloc(slice_size);
  Array* sub_array = AS_OBJ(sub_array_v, Array);
  memcpy(sub_array->items, self->items + start, slice_size * sizeof(value_t));

  return sub_array_v;
}

value_t array_split(value_t _self, int32_t idx) {
  Array* self = AS_OBJ(_self, Array);

  if (idx >= self->length) {
    value_t left = array_alloc(self->length);
    memcpy(AS_OBJ(left, Array)->items, self->items, self->length * sizeof(value_t));
    value_t right = array_alloc(0);
    return tuple_alloc(2, left, right);
  }
  if (idx < (-self->length)) {
    value_t left = array_alloc(0);
    value_t right = array_alloc(self->length);
    memcpy(AS_OBJ(right, Array)->items, self->items, self->length * sizeof(value_t));
    return tuple_alloc(2, left, right);
  }

  int32_t split_idx = (self->length + idx) % self->length;

  value_t left = array_alloc(split_idx);
  memcpy(AS_OBJ(left, Array)->items, self->items, split_idx * sizeof(value_t));

  value_t right = array_alloc(self->length - split_idx);
  memcpy(AS_OBJ(right, Array)->items, self->items + split_idx, (self->length - split_idx) * sizeof(value_t));

  return tuple_alloc(2, left, right);
}

value_t prelude__Array__toString(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  Array* self = AS_OBJ(_self, Array);
  return values_to_string(self->length, self->items, 1, "[", 1, "]", 2, ", ");
}

// ------------------------ TUPLE ------------------------
value_t tuple_alloc(int32_t length, ...) {
  Tuple* tuple = GC_MALLOC(sizeof(Tuple));
  value_t* tuple_items = GC_MALLOC(sizeof(value_t) * length);

  va_list ptr;
  va_start(ptr, length);
  for (int i = 0; i < length; i++) {
    tuple_items[i] = va_arg(ptr, value_t);
  }
  va_end(ptr);

  tuple->h.type_id = type_id_Tuple;
  tuple->length = length;
  tuple->items = tuple_items;
  return TAG_OBJ(tuple);
}

value_t tuple_get(value_t _self, int32_t idx) {
  if (_self == VAL_NONE) return _self;

  Tuple* self = AS_OBJ(_self, Tuple);

  if (idx < -self->length || idx >= self->length) return VAL_NONE;
  if (idx < 0) return self->items[idx + self->length];
  return self->items[idx];
}

value_t prelude__Tuple__toString(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  Tuple* self = AS_OBJ(_self, Tuple);
  return values_to_string(self->length, self->items, 1, "(", 1, ")", 2, ", ");
}

// ------------------------ FUNCTION ------------------------
value_t function_alloc(char* name, value_t fn_ptr) {
  Function* fn = GC_MALLOC(sizeof(Function));

  fn->h.type_id = type_id_Function;
  fn->name = name;
  fn->fn_ptr = fn_ptr;
  return TAG_OBJ(fn);
}

value_t closure_alloc(char* name, value_t fn_ptr, value_t* env) {
  Function* fn = GC_MALLOC(sizeof(Function));

  fn->h.type_id = type_id_Function;
  fn->name = name;
  fn->fn_ptr = fn_ptr;
  fn->env = env;
  return TAG_OBJ(fn);
}

value_t prelude__Function__toString(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  Function* self = AS_OBJ(_self, Function);

  int len = snprintf(NULL, 0, "<func %s>", self->name);
  char *result = GC_MALLOC(len + 1);
  snprintf(result, len + 1, "<func %s>", self->name);

  return string_alloc(len, result);
}

// ------------------------ PRELUDE FUNCTIONS ------------------------
char* print_impl(value_t varargs) {
  if (varargs == VAL_NONE) return "";

  Array* args = AS_OBJ(varargs, Array);
  value_t str = values_to_string(args->length, args->items, 0, "", 0, "", 1, " ");
  String* s = AS_OBJ(str, String);
  return s->chars;
}

void prelude__print(value_t* _env, int8_t _num_rcv_args, value_t varargs) {
  printf("%s", print_impl(varargs));
}

void prelude__println(value_t* _env, int8_t _num_rcv_args, value_t varargs) {
  printf("%s\n", print_impl(varargs));
}
