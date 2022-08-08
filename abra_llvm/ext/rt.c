#include "gc.h"
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

  tostring_method_t tostring_method;
  value_t* env = NULL;

  // A type's toString method can have an explicit implementation provided, which in theory can be a closure. We know
  // however, that none of the builtin types' toString methods are closures, so we can take a shortcut here.
  uint32_t type_id = type_id_for_val(value);
  if (
    type_id == type_id_Int ||
    type_id == type_id_Float ||
    type_id == type_id_Bool ||
    type_id == type_id_String ||
    type_id == type_id_Array ||
    type_id == type_id_Tuple ||
    type_id == type_id_Map ||
    type_id == type_id_Set ||
    type_id == type_id_Function
  ) {
    tostring_method = (tostring_method_t)vtable_lookup(value, TOSTRING_IDX);
  } else {
    Function* f = AS_OBJ(vtable_lookup(value, TOSTRING_IDX), Function);
    tostring_method = (tostring_method_t)f->fn_ptr;
    env = f->env;
  }

  return tostring_method(env, 1, value);
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

bool value_eq(value_t v1, value_t v2) {
  if ((IS_OBJ(v1) && !IS_OBJ(v2)) || (!IS_OBJ(v1) && IS_OBJ(v2))) {
    return false;
  }

  if (IS_OBJ(v1) && IS_OBJ(v2)) {
    uint32_t tid1 = AS_OBJ(v1, obj_header_t)->type_id;
    uint32_t tid2 = AS_OBJ(v2, obj_header_t)->type_id;

    if (tid1 != tid2) return false;

    if (tid1 == type_id_String) {
      String* self = AS_OBJ(v1, String);
      String* other = AS_OBJ(v2, String);

      if (self->size != other->size) return false;

      for (int i = 0; i < self->size; ++i) {
        if (self->chars[i] != other->chars[i]) return false;
      }

      return true;
    }

    if (tid1 == type_id_Array || tid1 == type_id_Tuple) {
      int32_t len1 = (tid1 == type_id_Array) ? AS_OBJ(v1, Array)->length : AS_OBJ(v1, Tuple)->length;
      int32_t len2 = (tid1 == type_id_Array) ? AS_OBJ(v2, Array)->length : AS_OBJ(v2, Tuple)->length;

      if (len1 != len2) return false;

      value_t* items1 = (tid1 == type_id_Array) ? AS_OBJ(v1, Array)->items : AS_OBJ(v1, Tuple)->items;
      value_t* items2 = (tid1 == type_id_Array) ? AS_OBJ(v2, Array)->items : AS_OBJ(v2, Tuple)->items;
      for (int i = 0; i < len1; i++) {
        if (!value_eq(items1[i], items2[i])) return false;
      }

      return true;
    }

    if (tid1 == type_id_Map) {
      Map* map1 = AS_OBJ(v1, Map);
      Map* map2 = AS_OBJ(v2, Map);
      if (map1->hash.size != map2->hash.size) return false;

      value_t* keys1 = hashmap_keys(&map1->hash);
      for (int i = 0; i < map1->hash.size; i++) {
          value_t key = keys1[i];
          value_t val1 = hashmap_get(&map1->hash, key);
          value_t val2 = hashmap_get(&map2->hash, key);
          if (!value_eq(val1, val2)) return false;
      }

      return true;
    }

    if (tid1 == type_id_Set) {
      Set* set1 = AS_OBJ(v1, Set);
      Set* set2 = AS_OBJ(v2, Set);
      if (set1->hash.size != set2->hash.size) return false;

      value_t* keys1 = hashmap_keys(&set1->hash);
      for (int i = 0; i < set1->hash.size; i++) {
        value_t key = keys1[i];
        value_t val1 = hashmap_get(&set1->hash, key);
        value_t val2 = hashmap_get(&set2->hash, key);
        if (!value_eq(val1, val2)) return false;
      }

      return true;
    }

    if (tid1 == type_id_Function) {
      uint32_t id1 = AS_OBJ(v1, Function)->id;
      uint32_t id2 = AS_OBJ(v2, Function)->id;

      return id1 == id2;
    }

    eq_method_t eq_method = (eq_method_t) vtable_lookup(v1, EQ_IDX);
    return eq_method(NULL, 2, v1, v2) == VAL_TRUE;
  } else if (IS_INT(v1) && IS_FLOAT(v2)) {
    double d1 = (double)AS_INT(v1);
    double d2 = AS_DOUBLE(v2);
    return d1 == d2;
  } else if (IS_FLOAT(v1) && IS_INT(v2)) {
    double d1 = AS_DOUBLE(v1);
    double d2 = (double)AS_INT(v2);
    return d1 == d2;
  } else {
    return v1 == v2;
  }
}

uint32_t value_hash(value_t v) {
  if (v == VAL_NONE) return 0;

  uint32_t type_id = type_id_for_val(v);
  if (type_id == type_id_Int) {
    return AS_INT(v) * 719;
  }

  if (type_id == type_id_Float) {
    return (uint32_t)(AS_DOUBLE(v) * 1000000000000 * 839);
  }

  if (type_id == type_id_Bool) {
    return v == VAL_TRUE ? 42643801 : 43112609;
  }

  if (type_id == type_id_String) {
    String* str = AS_OBJ(v, String);

    // Adapted from djb2 hashing algorithm
    uint32_t hash = 5381;
    for (uint32_t i = 0; i < str->size; ++i) {
      hash = ((hash << 5) + hash) ^ str->chars[i];
    }
    return hash;
  }

  if (type_id == type_id_Array) {
    Array* arr = AS_OBJ(v, Array);

    // Adapted from djb2 hashing algorithm
    size_t hash = 1823;
    for (int i = 0; i < arr->length; ++i) {
      hash = ((hash << 5) + hash) ^ value_hash(arr->items[i]);
    }
    return hash;
  }

  if (type_id == type_id_Tuple) {
    Tuple* tuple = AS_OBJ(v, Tuple);

    // Adapted from djb2 hashing algorithm
    size_t hash = 4253;
    for (int i = 0; i < tuple->length; ++i) {
      hash = ((hash << 5) + hash) ^ value_hash(tuple->items[i]);
    }
    return hash;
  }

  if (type_id == type_id_Map) {
    Map* map = AS_OBJ(v, Map);

    uint32_t hash = 4253;
    value_t* keys = hashmap_keys(&map->hash);
    for (int i = 0; i < map->hash.size; ++i) {
      value_t key = keys[i];
      value_t val = hashmap_get(&map->hash, key);
      hash = ((hash << 5) + hash) ^ value_hash(key);
      hash = ((hash << 5) + hash) ^ value_hash(val);
    }
    return hash;
  }

  if (type_id == type_id_Set) {
    Set* set = AS_OBJ(v, Set);

    uint32_t hash = 4253;
    value_t* values = hashmap_keys(&set->hash);
    for (int i = 0; i < set->hash.size; ++i) {
      value_t value = values[i];
      hash = ((hash << 5) + hash) ^ value_hash(value);
    }
    return hash;
  }

  if (type_id == type_id_Function) {
    return AS_OBJ(v, Function)->id;
  }

  hash_method_t hash_method = (hash_method_t)vtable_lookup(v, HASH_IDX);
  return (uint32_t) AS_INT(hash_method(NULL, 1, v));
}

// ------------------------ INT ------------------------
value_t prelude__Int__toString(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  int32_t self = AS_INT(_self);

  int len = snprintf(NULL, 0, "%d", self);
  char *result = GC_MALLOC(len + 1);
  snprintf(result, len + 1, "%d", self);

  return string_alloc(len, result);
}

value_t prelude__Int__abs(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  int32_t self = AS_INT(_self);

  if (self < 0) return TAG_INT(-self);
  return _self;
}

// ------------------------ FLOAT ------------------------
value_t prelude__Float__toString(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  double self = AS_DOUBLE(_self);

  int len = snprintf(NULL, 0, "%f", self);
  char *result = GC_MALLOC(len + 1);
  snprintf(result, len + 1, "%f", self);

  return string_alloc(len, result);
}

value_t prelude__Float__floor(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  double self = AS_DOUBLE(_self);
  int32_t i = (int32_t)floor(self);

  return TAG_INT(i);
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

  if (idx < -self->length) return;
  if (idx < 0) idx += self->length;
  if (idx >= self->length) {
    int32_t old_size = self->length;
    int32_t new_size = idx + 1;
    self->items = GC_REALLOC(self->items, sizeof(value_t) * new_size);
    self->length = new_size;

    for (int i = old_size; i < new_size; i++) {
      self->items[i] = VAL_NONE;
    }
  }

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

value_t prelude__Array__isEmpty(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  Array* self = AS_OBJ(_self, Array);

  return self->length == 0 ? VAL_TRUE : VAL_FALSE;
}

value_t prelude__Array__enumerate(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  Array* self = AS_OBJ(_self, Array);

  Array* pairs = AS_OBJ(array_alloc(self->length), Array);
  for (int i = 0; i < self->length; i++) {
    value_t item = self->items[i];
    value_t idx = TAG_INT(i);
    pairs->items[i] = tuple_alloc(2, item, idx);
  }

  return TAG_OBJ(pairs);
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
  return self->items[idx];
}

void tuple_set(value_t _self, int32_t idx, value_t value) {
  if (_self == VAL_NONE) return;

  Tuple* self = AS_OBJ(_self, Tuple);
  self->items[idx] = value;
}

value_t prelude__Tuple__toString(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  Tuple* self = AS_OBJ(_self, Tuple);
  return values_to_string(self->length, self->items, 1, "(", 1, ")", 2, ", ");
}

// ------------------------ MAP ------------------------
value_t map_alloc(int32_t size) {
  Map* map = GC_MALLOC(sizeof(Map));

  map->h.type_id = type_id_Map;
  map->hash = new_hashmap((uint32_t)size, &value_hash, &value_eq);

  return TAG_OBJ(map);
}

void map_insert(value_t _self, value_t key, value_t value) {
  Map* self = AS_OBJ(_self, Map);
  hashmap_insert(&self->hash, key, value);
}

value_t map_get(value_t _self, value_t key) {
  Map* self = AS_OBJ(_self, Map);
  return hashmap_get(&self->hash, key);
}

value_t prelude__Map__toString(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  Map* self = AS_OBJ(_self, Map);
  uint32_t size = (uint32_t)self->hash.size;
  value_t* keys = hashmap_keys(&self->hash);

  if (size == 0) {
    return string_alloc(2, "{}");
  }

  String** strings = GC_MALLOC(sizeof(String*) * size * 2);
  int32_t total_length = 4;
  for (int i = 0; i < size; i++) {
    value_t key = keys[i];
    value_t val = hashmap_get(&self->hash, key);

    String* key_str = AS_OBJ(value_to_string(key), String);
    String* val_str = AS_OBJ(value_to_string(val), String);

    strings[2 * i] = key_str;
    strings[2 * i + 1] = val_str;

    total_length += (key_str->size + val_str->size + 2); // ": "
    if (i != size - 1) {
      total_length += 2;
    }
  }

  char* chars = GC_MALLOC(sizeof(char) * total_length);
  chars[0] = '{';
  chars[1] = ' ';

  int32_t offset = 2;
  for (int i = 0; i < size; i++) {
    String* key = strings[2 * i];
    memcpy(chars + offset, key->chars, key->size);
    offset += key->size;

    chars[offset++] = ':';
    chars[offset++] = ' ';

    String* val = strings[2 * i + 1];
    memcpy(chars + offset, val->chars, val->size);
    offset += val->size;

    if (i != size - 1) {
      chars[offset++] = ',';
      chars[offset++] = ' ';
    }
  }
  chars[offset++] = ' ';
  chars[offset] = '}';

  return string_alloc(total_length, chars);
}

value_t prelude__Map__isEmpty(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  Map* self = AS_OBJ(_self, Map);

  return self->hash.size == 0 ? VAL_TRUE : VAL_FALSE;
}

value_t prelude__Map__enumerate(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  Map* self = AS_OBJ(_self, Map);

  size_t size = self->hash.size;
  Array* pairs = AS_OBJ(array_alloc(size), Array);

  value_t* map_keys = hashmap_keys(&self->hash);
  for (int i = 0; i < size; i++) {
    value_t key = map_keys[i];
    value_t val = hashmap_get(&self->hash, key);

    pairs->items[i] = tuple_alloc(2, key, val);
  }

  return TAG_OBJ(pairs);
}


// ------------------------ SET ------------------------
value_t set_alloc(int32_t size) {
  Set* set = GC_MALLOC(sizeof(Set));

  set->h.type_id = type_id_Set;
  set->hash = new_hashmap((uint32_t)size, &value_hash, &value_eq);

  return TAG_OBJ(set);
}

void set_insert(value_t _self, value_t value) {
  Set* self = AS_OBJ(_self, Set);
  hashmap_insert(&self->hash, value, VAL_TRUE);
}

value_t prelude__Set__toString(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  Set* self = AS_OBJ(_self, Set);

  value_t* keys = hashmap_keys(&self->hash);
  return values_to_string((int32_t)self->hash.size, keys, 2, "#{", 1, "}", 2, ", ");
}

value_t prelude__Set__isEmpty(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  Set* self = AS_OBJ(_self, Set);

  return self->hash.size == 0 ? VAL_TRUE : VAL_FALSE;
}

value_t prelude__Set__enumerate(value_t* _env, int8_t _num_rcv_args, value_t _self) {
  Set* self = AS_OBJ(_self, Set);

  size_t size = self->hash.size;
  Array* pairs = AS_OBJ(array_alloc(size), Array);

  value_t* map_keys = hashmap_keys(&self->hash);
  for (int i = 0; i < size; i++) {
    value_t item = map_keys[i];
    value_t idx = TAG_INT(i);

    pairs->items[i] = tuple_alloc(2, item, idx);
  }

  return TAG_OBJ(pairs);
}

// ------------------------ FUNCTION ------------------------
value_t function_alloc(char* name, value_t fn_ptr, value_t* env) {
  Function* fn = GC_MALLOC(sizeof(Function));

  fn->h.type_id = type_id_Function;
  fn->name = name;
  fn->fn_ptr = fn_ptr;
  fn->env = env; // env will be NULL for non-closures
  fn->id = (uint32_t)rand();
  fn->bound_self = VAL_NONE;
  return TAG_OBJ(fn);
}

value_t function_bind(value_t fn_val, value_t self) {
  Function* fn = AS_OBJ(fn_val, Function);
  Function* fn_copy = GC_MALLOC(sizeof(Function));

  fn_copy->h.type_id = type_id_Function;
  fn_copy->name = fn->name;
  fn_copy->fn_ptr = fn->fn_ptr;
  fn_copy->env = fn->env;
  fn_copy->id = (uint32_t)rand();
  fn_copy->bound_self = self;

  return TAG_OBJ(fn_copy);
}


value_t function_call(value_t fn_val, bool has_return, int8_t fn_arity, int8_t num_args, ...) {
#define FN_LOGIC(n, nplus1, ...) \
    if (fn->bound_self != VAL_NONE) { \
        if (has_return) return ((fn_##nplus1##_t)(fn->fn_ptr))(fn->env, num_args + 1, fn->bound_self, ##__VA_ARGS__); \
        ((fn_void_##nplus1##_t)(fn->fn_ptr))(fn->env, num_args + 1, fn->bound_self, ##__VA_ARGS__); \
        return VAL_NONE; \
    } \
    if (has_return) return ((fn_##n##_t)(fn->fn_ptr))(fn->env, num_args, ##__VA_ARGS__); \
    ((fn_void_##n##_t)(fn->fn_ptr))(fn->env, num_args, ##__VA_ARGS__); \
    return VAL_NONE;

  Function* fn = AS_OBJ(fn_val, Function);

  va_list args;
  va_start(args, num_args);

  typedef value_t (*fn_0_t)(value_t*, int8_t);
  typedef void (*fn_void_0_t)(value_t*, int8_t);
  typedef value_t (*fn_1_t)(value_t*, int8_t, value_t);
  typedef void (*fn_void_1_t)(value_t*, int8_t, value_t);
  if (fn_arity == 0) { FN_LOGIC(0, 1) }

  typedef value_t (*fn_2_t)(value_t*, int8_t, value_t, value_t);
  typedef void (*fn_void_2_t)(value_t*, int8_t, value_t, value_t);
  value_t arg0 = va_arg(args, value_t);
  if (fn_arity == 1) { va_end(args); FN_LOGIC(1, 2, arg0) }

  typedef value_t (*fn_3_t)(value_t*, int8_t, value_t, value_t, value_t);
  typedef void (*fn_void_3_t)(value_t*, int8_t, value_t, value_t, value_t);
  value_t arg1 = va_arg(args, value_t);
  if (fn_arity == 2) { va_end(args); FN_LOGIC(2, 3, arg0, arg1) }

  typedef value_t (*fn_4_t)(value_t*, int8_t, value_t, value_t, value_t, value_t);
  typedef void (*fn_void_4_t)(value_t*, int8_t, value_t, value_t, value_t, value_t);
  value_t arg2 = va_arg(args, value_t);
  if (fn_arity == 3) { va_end(args); FN_LOGIC(3, 4, arg0, arg1, arg2) }

  typedef value_t (*fn_5_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t);
  typedef void (*fn_void_5_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t);
  value_t arg3 = va_arg(args, value_t);
  if (fn_arity == 4) { va_end(args); FN_LOGIC(4, 5, arg0, arg1, arg2, arg3) }

  typedef value_t (*fn_6_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t);
  typedef void (*fn_void_6_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t);
  value_t arg4 = va_arg(args, value_t);
  if (fn_arity == 5) { va_end(args); FN_LOGIC(5, 6, arg0, arg1, arg2, arg3, arg4) }

  typedef value_t (*fn_7_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  typedef void (*fn_void_7_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  value_t arg5 = va_arg(args, value_t);
  if (fn_arity == 6) { va_end(args); FN_LOGIC(6, 7, arg0, arg1, arg2, arg3, arg4, arg5) }

  typedef value_t (*fn_8_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  typedef void (*fn_void_8_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  value_t arg6 = va_arg(args, value_t);
  if (fn_arity == 7) { va_end(args); FN_LOGIC(7, 8, arg0, arg1, arg2, arg3, arg4, arg5, arg6) }

  typedef value_t (*fn_9_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  typedef void (*fn_void_9_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  value_t arg7 = va_arg(args, value_t);
  if (fn_arity == 8) { va_end(args); FN_LOGIC(8, 9, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) }

  typedef value_t (*fn_10_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  typedef void (*fn_void_10_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  value_t arg8 = va_arg(args, value_t);
  if (fn_arity == 9) { va_end(args); FN_LOGIC(9, 10, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) }

  typedef value_t (*fn_11_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  typedef void (*fn_void_11_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  value_t arg9 = va_arg(args, value_t);
  if (fn_arity == 10) { va_end(args); FN_LOGIC(10, 11, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) }

  typedef value_t (*fn_12_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  typedef void (*fn_void_12_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  value_t arg10 = va_arg(args, value_t);
  if (fn_arity == 11) { va_end(args); FN_LOGIC(11, 12, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) }

  typedef value_t (*fn_13_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  typedef void (*fn_void_13_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  value_t arg11 = va_arg(args, value_t);
  if (fn_arity == 12) { va_end(args); FN_LOGIC(12, 13, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) }

  typedef value_t (*fn_14_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  typedef void (*fn_void_14_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  value_t arg12 = va_arg(args, value_t);
  if (fn_arity == 13) { va_end(args); FN_LOGIC(13, 14, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12) }

  typedef value_t (*fn_15_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  typedef void (*fn_void_15_t)(value_t*, int8_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t, value_t);
  value_t arg13 = va_arg(args, value_t);
  if (fn_arity == 14) { va_end(args); FN_LOGIC(14, 15, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13) }

  return VAL_NONE;
#undef FN_LOGIC
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

value_t prelude__range(value_t* _env, int8_t num_rcv_args, value_t _from, value_t _to, value_t _increment) {
  int32_t from = AS_INT(_from);
  int32_t to = AS_INT(_to);
  int32_t increment = num_rcv_args < 3 || _increment == VAL_NONE ? 1 : AS_INT(_increment);

  size_t size = ceil(abs(to - from) / (double)increment);
  Array* values = AS_OBJ(array_alloc(size), Array);
  size_t i = 0;
  while (from < to) {
    values->items[i++] = TAG_INT(from);
    from += increment;
  }

  return TAG_OBJ(values);
}
