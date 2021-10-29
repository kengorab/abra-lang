#ifndef __ABRA_H
#define __ABRA_H

// To print debug information, run binary with GC_PRINT_STATS=1 env var
#define GC_DEBUG

#include "gc.h"
#include "inttypes.h"
#include "math.h"
#include "stdbool.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#define TODO(m)                                         \
  do {                                                  \
    printf("%s:%d: TODO: " m "\n", __FILE__, __LINE__); \
    exit(1);                                            \
  } while (0);
#define UNREACHABLE                                                  \
  do {                                                               \
    printf("%s:%d: Entered unreachable code\n", __FILE__, __LINE__); \
    exit(1);                                                         \
  } while (0);

typedef enum {
  OBJ_STR = 0,
  OBJ_ARRAY,
  OBJ_TUPLE,
  OBJ_MAP,
  OBJ_CLOSURE,
} ObjectType;
typedef struct Obj {
  ObjectType type;
} Obj;

typedef enum {
  ABRA_TYPE_NONE = 0,
  ABRA_TYPE_INT,
  ABRA_TYPE_FLOAT,
  ABRA_TYPE_BOOL,
  ABRA_TYPE_OBJ,
} AbraType;
typedef struct AbraValue {
  AbraType type;
  union {
    int64_t abra_int;
    double abra_float;
    bool abra_bool;
    Obj* obj;
  } as;
} AbraValue;

static AbraValue ABRA_NONE = {.type = ABRA_TYPE_NONE};
#define IS_NONE(v) (v.type == ABRA_TYPE_NONE)

static AbraValue ABRA_TRUE = {.type = ABRA_TYPE_BOOL, .as = {.abra_bool = true}};
static AbraValue ABRA_FALSE = {.type = ABRA_TYPE_BOOL, .as = {.abra_bool = false}};

#define NEW_INT(i) ((AbraValue){.type = ABRA_TYPE_INT, .as = {.abra_int = i}})
#define AS_INT(v) v.as.abra_int
#define NEW_FLOAT(f) ((AbraValue){.type = ABRA_TYPE_FLOAT, .as = {.abra_float = f}})
#define AS_FLOAT(v) v.as.abra_float
#define NEW_BOOL(b) (b ? ABRA_TRUE : ABRA_FALSE)
#define AS_BOOL(v) v.as.abra_bool
#define AS_OBJ(v) v.as.obj

bool std__eq(AbraValue v1, AbraValue v2);
char const* std__to_string(AbraValue val);
size_t std__hash(AbraValue val);

// Common utility function shared among array/tuple to_string impls
char const* array_to_string(AbraValue* items, size_t count, size_t* out_str_len) {
  if (count == 0) {
    if (out_str_len != NULL) *out_str_len = 2;
    return "[]";
  }

  // Convert each element to string and compute total size
  char const** item_strs = malloc(sizeof(char*) * count);
  size_t size = 2 + (2 * (count - 1));  // Account for "[" and "]", plus ", " for all but last item
  for (int i = 0; i < count; ++i) {
    AbraValue item = items[i];
    char const* item_str = std__to_string(item);
    item_strs[i] = item_str;
    size += strlen(item_str);
  }

  // Allocate necessary string and copy items' strings into place
  if (out_str_len != NULL) *out_str_len = size;
  char* res_str = GC_MALLOC(sizeof(char) * size);
  res_str[0] = '[';
  char* ptr = res_str + 1;
  for (int i = 0; i < count; ++i) {
    char const* item_str = item_strs[i];
    size_t len = strlen(item_str);
    memcpy(ptr, item_str, len);
    ptr += len;
    if (i < count - 1) {
      memcpy(ptr, ", ", 2);
      ptr += 2;
    }
  }
  res_str[size - 1] = ']';
  res_str[size] = 0;

  free(item_strs);

  return res_str;
}

void range_endpoints(int64_t len, int64_t* start, int64_t* end) {
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

typedef struct hash_entry_t {
  AbraValue key;
  AbraValue value;
  struct hash_entry_t* next;
} hash_entry_t;

hash_entry_t* new_hash_entry(AbraValue key, AbraValue value) {
    hash_entry_t* new_entry = GC_MALLOC(sizeof(hash_entry_t));
    new_entry->key = key;
    new_entry->value = value;
    new_entry->next = NULL;
    return new_entry;
}

typedef struct hashmap_t {
  size_t size;
  size_t capacity;
  hash_entry_t** buckets;
  size_t (*hash_fn)(AbraValue);
  bool (*eq_fn)(AbraValue, AbraValue);
} hashmap_t;

hashmap_t new_hashmap(
  size_t (*hash_fn)(AbraValue),
  bool (*eq_fn)(AbraValue, AbraValue)
) {
  hashmap_t h;
  h.size = 0;
  h.capacity = 24;
  h.buckets = GC_MALLOC(sizeof(hash_entry_t) * h.capacity);
  h.hash_fn = hash_fn;
  h.eq_fn = eq_fn;
  return h;
}

void _hashmap_resize(hashmap_t* h);
void hashmap_insert(hashmap_t* h, AbraValue key, AbraValue value) {
    if (h->size > h->capacity * 0.75) {
        _hashmap_resize(h);
    }

    size_t hash = h->hash_fn(key);
    size_t idx = hash % h->capacity;

    if (h->buckets[idx] == NULL) {
        h->buckets[idx] = new_hash_entry(key, value);
    } else {
        hash_entry_t* node;
        for (node = h->buckets[idx]; node; ) {
            if (h->eq_fn(node->key, key)) {
                node->value = value;
                return;
            }
            if (node->next) {
                node = node->next;
            } else {
                break;
            }
        }
        node->next = new_hash_entry(key, value);
    }

    h->size++;
}

AbraValue hashmap_get(hashmap_t* h, AbraValue key) {
    size_t hash = h->hash_fn(key);
    size_t idx = hash % h->capacity;

    if (h->buckets[idx] == NULL) return ABRA_NONE;

    hash_entry_t* node;
    for (node = h->buckets[idx]; node; node = node->next) {
        if (h->eq_fn(node->key, key)) {
            return node->value;
        }
    }
    return ABRA_NONE;
}

AbraValue* hashmap_keys(hashmap_t* h) {
    AbraValue* keys = GC_MALLOC(sizeof(AbraValue) * h->size);
    size_t idx = 0;
    for (int i = 0; i < h->capacity; ++i) {
        if (h->buckets[i] == NULL) continue;

        hash_entry_t* node;
        for (node = h->buckets[i]; node; node = node->next) {
            keys[idx++] = node->key;
        }
    }
    return keys;
}

void _hashmap_resize(hashmap_t* h) {
    size_t new_capacity = h->capacity * 2;
    size_t old_capacity = h->capacity;
    h->capacity = new_capacity;

    hash_entry_t** old_buckets = h->buckets;
    h->buckets = GC_MALLOC(sizeof(hash_entry_t) * h->capacity);
    h->size = 0;

    for (int i = 0; i < old_capacity; ++i) {
        hash_entry_t* node;
        for (node = old_buckets[i]; node; node = node->next) {
            hashmap_insert(h, node->key, node->value);
        }
    }
}

typedef struct AbraString {
  Obj _header;
  uint32_t size;
  char* data;
} AbraString;

AbraValue alloc_string(char* data, size_t size) {
  AbraString* str = GC_MALLOC(sizeof(AbraString));

  str->_header.type = OBJ_STR;
  str->size = size;
  if (data != NULL) {
    str->data = strdup(data);
  }
  str->data[size] = 0;

  return ((AbraValue){.type = ABRA_TYPE_OBJ, .as = {.obj = ((Obj*)str)}});
}

AbraValue std_string__concat(AbraValue str1, AbraValue str2) {
  AbraString* s1 = (AbraString*)str1.as.obj;
  AbraString* s2 = (AbraString*)str2.as.obj;

  size_t new_str_len = s1->size + s2->size + 1;
  char* tmp = malloc(new_str_len);
  memcpy(tmp, s1->data, s1->size);
  memcpy(tmp + s1->size, s2->data, s2->size);
  tmp[new_str_len] = 0;

  size_t size = s1->size + s2->size;
  AbraValue new_string = alloc_string(tmp, size);
  free(tmp);
  return new_string;
}

bool std_string__eq(Obj* o1, Obj* o2) {
  AbraString* s1 = (AbraString*)o1;
  AbraString* s2 = (AbraString*)o2;
  if (s1->size != s2->size) return false;

  for (int i = 0; i < s1->size; ++i) {
    if (s1->data[i] != s2->data[i]) return false;
  }
  return true;
}

char const* std_string__to_string(Obj* obj) {
  return ((AbraString*)obj)->data;
}

size_t std_string__hash(Obj* obj) {
  AbraString* self = (AbraString*)obj;

  // Adapted from djb2 hashing algorithm
  size_t hash = 5381;
  for (size_t i = 0; i < self->size; ++i) {
    hash = ((hash << 5) + hash) ^ self->data[i];
  }
  return hash;
}

AbraValue std_string__index(Obj* obj, int64_t index) {
    AbraString* self = (AbraString*)obj;
    int64_t len = (int64_t)self->size;
    if (index < -len || index >= len) return ABRA_NONE;

    if (index < 0) index += len;
    return alloc_string(self->data + index, 1);
}

AbraValue std_string__range(Obj* obj, int64_t start, int64_t end) {
    AbraString* self = (AbraString*)obj;
    int64_t len = self->size;
    range_endpoints(len, &start, &end);

    int64_t slice_size = end - start;
    char* tmp = malloc(slice_size);
    memcpy(tmp, self->data+start, slice_size);
    tmp[slice_size] = 0;

    AbraValue new_string = alloc_string(tmp, slice_size);
    free(tmp);
    return new_string;
}

AbraValue std_string__range_from_start(Obj* obj, int64_t end) {
    return std_string__range(obj, 0, end);
}

AbraValue std_string__range_to_end(Obj* obj, int64_t start) {
    AbraString* self = (AbraString*)obj;
    return std_string__range(obj, start, self->size);
}

typedef struct AbraArray {
  Obj _header;
  uint32_t size;
  uint32_t capacity;
  AbraValue* items;
} AbraArray;

AbraValue alloc_array(AbraValue* values, size_t size) {
  AbraArray* arr = GC_MALLOC(sizeof(AbraArray));

  arr->_header.type = OBJ_ARRAY;
  arr->size = size;
  arr->capacity = size;
  arr->items = values;

  return ((AbraValue){.type = ABRA_TYPE_OBJ, .as = {.obj = ((Obj*)arr)}});
}

bool std_array__eq(Obj* o1, Obj* o2) {
  AbraArray* self = (AbraArray*)o1;
  AbraArray* other = (AbraArray*)o2;
  if (self->size != other->size) return false;

  for (int i = 0; i < self->size; ++i) {
    if (!std__eq(self->items[i], other->items[i])) return false;
  }
  return true;
}

char const* std_array__to_string(Obj* obj) {
  AbraArray* self = (AbraArray*)obj;
  return array_to_string(self->items, self->size, NULL);
}

size_t std_array__hash(Obj* obj) {
  AbraArray* arr = (AbraArray*)obj;

  // Adapted from djb2 hashing algorithm
  size_t hash = 5381;
  for (int i = 0; i < arr->size; ++i) {
    hash = ((hash << 5) + hash) ^ std__hash(arr->items[i]);
  }
  return hash;
}

AbraValue std_array__index(Obj* obj, int64_t index) {
    AbraArray* self = (AbraArray*)obj;
    int64_t len = (int64_t) self->size;
    if (index < -len || index >= len) return ABRA_NONE;

    if (index < 0) index += len;
    return self->items[index];
}

AbraValue std_array__range(Obj* obj, int64_t start, int64_t end) {
  AbraArray* self = (AbraArray*)obj;
  int64_t len = (int64_t) self->size;
  range_endpoints(len, &start, &end);

  int64_t slice_size = end - start;
  AbraValue* items = GC_MALLOC(sizeof(AbraValue) * slice_size);
  for (int i = start; i < end; ++i) {
    items[i - start] = self->items[i];
  }
  return alloc_array(items, slice_size);
}

AbraValue std_array__range_from_start(Obj* obj, int64_t end) {
  return std_array__range(obj, 0, end);
}

AbraValue std_array__range_to_end(Obj* obj, int64_t start) {
  AbraArray* self = (AbraArray*)obj;
  return std_array__range(obj, start, self->size);
}

typedef struct AbraTuple {
  Obj _header;
  uint32_t size;
  AbraValue* items;
} AbraTuple;

AbraValue alloc_tuple(AbraValue* values, size_t size) {
  AbraTuple* tuple = GC_MALLOC(sizeof(AbraTuple));

  tuple->_header.type = OBJ_TUPLE;
  tuple->size = size;
  tuple->items = values;

  return ((AbraValue){.type = ABRA_TYPE_OBJ, .as = {.obj = ((Obj*)tuple)}});
}

bool std_tuple__eq(Obj* o1, Obj* o2) {
  AbraTuple* self = (AbraTuple*)o1;
  AbraTuple* other = (AbraTuple*)o2;
  if (self->size != other->size) return false;

  for (int i = 0; i < self->size; ++i) {
    if (!std__eq(self->items[i], other->items[i])) return false;
  }
  return true;
}

char const* std_tuple__to_string(Obj* obj) {
  AbraTuple* self = (AbraTuple*)obj;
  size_t s;
  char* str = (char*) array_to_string(self->items, self->size, &s);
  str[0] = '(';
  str[s - 1] = ')';
  return str;
}

size_t std_tuple__hash(Obj* obj) {
  AbraTuple* tuple = (AbraTuple*)obj;

  // Adapted from djb2 hashing algorithm
  size_t hash = 4253;
  for (int i = 0; i < tuple->size; ++i) {
    hash = ((hash << 5) + hash) ^ std__hash(tuple->items[i]);
  }
  return hash;
}

AbraValue std_tuple__index(Obj* obj, int64_t index) {
    AbraTuple* self = (AbraTuple*)obj;
    return self->items[index];
}

typedef struct AbraMap {
    Obj _header;
    hashmap_t hash;
} AbraMap;

AbraValue alloc_map() {
    AbraMap* map = GC_MALLOC(sizeof(AbraMap));

    map->_header.type = OBJ_MAP;
    map->hash = new_hashmap(&std__hash, &std__eq);

    return ((AbraValue){.type = ABRA_TYPE_OBJ, .as = {.obj = ((Obj*)map)}});
}

void std_map__insert(Obj* o, AbraValue key, AbraValue value) {
    AbraMap* self = (AbraMap*)o;
    hashmap_insert(&self->hash, key, value);
}

bool std_map__eq(Obj* o1, Obj* o2) {
    AbraMap* self = (AbraMap*)o1;
    AbraMap* other = (AbraMap*)o2;
    if (self->hash.size != other->hash.size) return false;

    AbraValue* keys = hashmap_keys(&self->hash);
    for (int i = 0; i < self->hash.size; ++i) {
        AbraValue key = keys[i];
        AbraValue self_val = hashmap_get(&self->hash, key);
        AbraValue other_val = hashmap_get(&other->hash, key);
        if (!std__eq(self_val, other_val)) return false;
    }
    return true;
}

char const* std_map__to_string(Obj* obj) {
    AbraMap* self = (AbraMap*)obj;

    if (self->hash.size == 0) return "{}";

    char* str = GC_MALLOC(sizeof(char) * 1000);
    memcpy(str, "{ ", 2);
    char* ptr = str + 2;

    size_t cur_item = 0;
    for (int i = 0; i < self->hash.capacity; ++i) {
        if (self->hash.buckets[i] == NULL) continue;

        hash_entry_t* node;
        for (node = self->hash.buckets[i]; node; node = node->next) {
            const char* key_str = std__to_string(node->key);
            size_t key_len = strlen(key_str);
            memcpy(ptr, key_str, key_len);
            ptr += key_len;

            memcpy(ptr, ": ", 2);
            ptr += 2;

            const char* val_str = std__to_string(node->value);
            size_t val_len = strlen(val_str);
            memcpy(ptr, val_str, val_len);
            ptr += val_len;

            if (cur_item < self->hash.size - 1) {
                memcpy(ptr, ", ", 2);
                ptr += 2;
            }

            cur_item++;
        }
    }

    memcpy(ptr, " }", 2);
    return str;
}

size_t std_map__hash(Obj* obj) {
    AbraMap* self = (AbraMap*)obj;

    // Adapted from djb2 hashing algorithm
    size_t hash = 4253;
    AbraValue* keys = hashmap_keys(&self->hash);
    for (int i = 0; i < self->hash.size; ++i) {
        AbraValue key = keys[i];
        AbraValue val = hashmap_get(&self->hash, key);
        hash = ((hash << 5) + hash) ^ std__hash(key);
        hash = ((hash << 5) + hash) ^ std__hash(val);
    }
    return hash;
}

AbraValue std_map__index(Obj* obj, AbraValue key) {
    AbraMap* self = (AbraMap*)obj;
    return hashmap_get(&self->hash, key);
}

struct abra_closure_env_t;
typedef AbraValue (*abra_closure_fn_t)(struct abra_closure_env_t*);
typedef struct abra_closure_env_t {
    abra_closure_fn_t __fn;
} abra_closure_env_t;

typedef struct AbraClosure {
    Obj _header;
    char const* name;
    abra_closure_env_t* env;
} AbraClosure;

AbraValue alloc_closure(char* name, abra_closure_env_t* env) {
    AbraClosure* closure = GC_MALLOC(sizeof(AbraClosure));

    closure->_header.type = OBJ_CLOSURE;
    closure->name = strdup(name);
    closure->env = env;

    return ((AbraValue){.type = ABRA_TYPE_OBJ, .as = {.obj = ((Obj*)closure)}});
}

AbraValue call_closure(AbraValue v) {
    AbraClosure* fn = (AbraClosure*) AS_OBJ(v);
    return fn->env->__fn(fn->env);
}

//AbraValue call(AbraValue v) {
//    // TODO abstract over AbraClosure and future AbraFn type?
//}

#define OBJ_LIMIT 100

typedef bool (*EqFn)(Obj*, Obj*);
static EqFn eq_fns[OBJ_LIMIT];

bool std__eq(AbraValue v1, AbraValue v2) {
  if (v1.type == ABRA_TYPE_INT && v2.type == ABRA_TYPE_FLOAT)
    return ((double)v1.as.abra_int) == v2.as.abra_float;
  if (v1.type == ABRA_TYPE_FLOAT && v2.type == ABRA_TYPE_INT)
    return v1.as.abra_float == ((double)v2.as.abra_int);

  if (v1.type != v2.type) return false;
  switch (v1.type) {
    case ABRA_TYPE_NONE:
      return true;
    case ABRA_TYPE_INT:
      return v1.as.abra_int == v2.as.abra_int;
    case ABRA_TYPE_FLOAT:
      return v1.as.abra_float == v2.as.abra_float;
    case ABRA_TYPE_BOOL:
      return v1.as.abra_bool == v2.as.abra_bool;
    case ABRA_TYPE_OBJ: {
      Obj* o1 = v1.as.obj;
      Obj* o2 = v2.as.obj;
      if (o1->type != o2->type) return false;
      return eq_fns[o1->type](o1, o2);
    }
  }

  return true;
}

typedef char const* (*ToStringFn)(Obj*);
static ToStringFn to_string_fns[OBJ_LIMIT];

char const* std__to_string(AbraValue val) {
  switch (val.type) {
    case ABRA_TYPE_NONE:
      return "None";
    case ABRA_TYPE_INT: {
      int64_t i = val.as.abra_int;
      int len = snprintf(NULL, 0, "%" PRId64, i);
      char* str = GC_MALLOC(len + 1);
      snprintf(str, len + 1, "%" PRId64, i);
      return str;
    }
    case ABRA_TYPE_FLOAT: {
      double d = val.as.abra_float;
      int len = snprintf(NULL, 0, "%f", d);
      char* str = GC_MALLOC(len + 1);
      snprintf(str, len + 1, "%f", d);
      // Trim trailing zeroes
      for (int i = len - 1; i >= 1; --i) {
        if (str[i] == '0' && str[i - 1] != '.') {
          str[i] = 0;
        } else {
          break;
        }
      }
      return str;
    }
    case ABRA_TYPE_BOOL:
      return val.as.abra_bool ? "true" : "false";
    case ABRA_TYPE_OBJ: {
      Obj* o = val.as.obj;
      return to_string_fns[o->type](o);
    } break;
    default:
      UNREACHABLE // All the primitive types have been handled
  }
}

typedef size_t (*HashFn)(Obj*);
static HashFn hash_fns[OBJ_LIMIT];

size_t std__hash(AbraValue val) {
  switch (val.type) {
    case ABRA_TYPE_NONE: return 0;
    case ABRA_TYPE_INT: return val.as.abra_int * 719;
    case ABRA_TYPE_FLOAT: return (size_t) (val.as.abra_float * 1000000000000 * 839);
    case ABRA_TYPE_BOOL: return val.as.abra_bool ? 42643801 : 43112609;
    case ABRA_TYPE_OBJ: {
      Obj* o = val.as.obj;
      return hash_fns[o->type](o);
    }
  }
}

void std__println(AbraValue val) {
  if (IS_NONE(val)) {
    printf("\n");
    return;
  }

  AbraArray* varargs = (AbraArray*)val.as.obj;
  for (int i = 0; i < varargs->size; ++i) {
    printf("%s", std__to_string(varargs->items[i]));
    if (i < varargs->size - 1) {
      printf(" ");
    }
  }
  printf("\n");
}

void abra_init() {
  GC_INIT();

  eq_fns[OBJ_STR] = &std_string__eq;
  eq_fns[OBJ_ARRAY] = &std_array__eq;
  eq_fns[OBJ_TUPLE] = &std_tuple__eq;
  eq_fns[OBJ_MAP] = &std_map__eq;

  to_string_fns[OBJ_STR] = &std_string__to_string;
  to_string_fns[OBJ_ARRAY] = &std_array__to_string;
  to_string_fns[OBJ_TUPLE] = &std_tuple__to_string;
  to_string_fns[OBJ_MAP] = &std_map__to_string;

  hash_fns[OBJ_STR] = &std_string__hash;
  hash_fns[OBJ_ARRAY] = &std_array__hash;
  hash_fns[OBJ_TUPLE] = &std_tuple__hash;
  hash_fns[OBJ_MAP] = &std_map__hash;
}

#endif
