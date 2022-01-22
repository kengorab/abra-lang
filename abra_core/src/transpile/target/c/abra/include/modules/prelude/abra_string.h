#ifndef __ABRA_STRING_H
#define __ABRA_STRING_H

#include "ctype.h"
#include "string.h"
#include "errno.h"
#include "../../abra_value.h"
#include "../../abra_module.h"
#include "abra_array.h"
#include "abra_tuple.h"

AbraValue ABRA_METHOD_NAME(std, String, concat)(void* _env, AbraValue _self, AbraValue _str, AbraValue _others);
AbraValue std_string__concat(AbraValue lhs, AbraValue rhs) {
    // If the lhs value is a string, we can just use the concat method impl, with the lhs as `self`.
    // If the lhs value is _not_ a string then the rhs must be (otherwise, this would not have been
    // a string concat operation). So convert the lhs value to an AbraString (wasteful) and use that
    // as the `self` value for the concat method impl.
    if (!(lhs.type == ABRA_TYPE_OBJ && AS_OBJ(lhs)->type == OBJ_STR)) {
        char* str = (char*) std__to_string(lhs);
        size_t str_size = strlen(str);
        lhs = alloc_string(str, str_size);
    }
    return ABRA_METHOD_NAME(std, String, concat)(NULL, lhs, rhs, ABRA_NONE);
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

  if (start >= end) {
    return alloc_string("", 0);
  }

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

// ************************************
// * String fields & methods
// ************************************

// length: Int
AbraValue ABRA_FIELD_NAME(std, String, length)(AbraValue _self) {
  AbraString* self = (AbraString*)AS_OBJ(_self);
  return NEW_INT(self->size);
}

// toString(): String
AbraValue ABRA_METHOD_NAME(std, String, toString)(void* _env, AbraValue _self) {
  char* str = (char*) std_string__to_string(AS_OBJ(_self));
  AbraValue ret = alloc_string(str, strlen(str));
  free(str);
  return ret;
}

// toLower(): String
AbraValue ABRA_METHOD_NAME(std, String, toLower)(void* _env, AbraValue _self) {
  AbraString* self = (AbraString*)AS_OBJ(_self);
  char* str = strdup(self->data);
  for (int i = 0; i < self->size; ++i) {
    if (str[i] >= 'A' && str[i] <= 'Z') {
      str[i] = str[i] + 32;
    }
  }
  return alloc_string(str, self->size);
}

// toUpper(): String
AbraValue ABRA_METHOD_NAME(std, String, toUpper)(void* _env, AbraValue _self) {
  AbraString* self = (AbraString*)AS_OBJ(_self);
  char* str = strdup(self->data);
  for (int i = 0; i < self->size; ++i) {
    if (str[i] >= 'a' && str[i] <= 'z') {
      str[i] = str[i] - 32;
    }
  }
  return alloc_string(str, self->size);
}

// padLeft(totalSize: Int, padding: String = ""): String
AbraValue ABRA_METHOD_NAME(std, String, padLeft)(void* _env, AbraValue _self, AbraValue _total_size, AbraValue _padding) {
  AbraString* self = (AbraString*)AS_OBJ(_self);
  int64_t total_size = AS_INT(_total_size);
  if (total_size <= 0 || self->size >= total_size) {
    return alloc_string(self->data, self->size);
  }

  char const* padding = " ";
  size_t padding_size = 1;
  if (!IS_NONE(_padding)) {
    AbraString* padding_str = (AbraString*) AS_OBJ(_padding);
    padding = strdup(padding_str->data);
    padding_size = padding_str->size;
  }

  size_t num_repeats = ceil(((total_size) - self->size) / (double) padding_size);
  char* new_str = malloc(sizeof(char) * total_size);
  for (int i = 0; i < num_repeats; ++i) {
    memcpy(new_str + (i * padding_size), padding, padding_size);
  }
  memcpy(new_str + (total_size - self->size), self->data, self->size);

  AbraValue ret = alloc_string(new_str, total_size);
  free(new_str);
  return ret;
}

// trim(): String
AbraValue ABRA_METHOD_NAME(std, String, trim)(void* _env, AbraValue _self) {
  AbraString* self = (AbraString*)AS_OBJ(_self);
  char* new_str = strdup(self->data);
  size_t size = self->size;

  for (int i = size - 1; i >= 0; --i) {
    if (isspace(new_str[i])) {
      new_str[i] = 0;
      size--;
    } else {
      break;
    }
  }
  size_t start_offset = 0;
  for (int i = 0; i < size; ++i) {
    if (isspace(new_str[i])) {
      start_offset++;
    } else {
      break;
    }
  }

  AbraValue ret = alloc_string(new_str + start_offset, size - start_offset);
  free(new_str);
  return ret;
}

// trimStart(pattern: String = ""): String
AbraValue ABRA_METHOD_NAME(std, String, trimStart)(void* _env, AbraValue _self, AbraValue _pattern) {
  AbraString* self = (AbraString*)AS_OBJ(_self);

  char const* pattern = " ";
  size_t pattern_size = 1;
  if (!IS_NONE(_pattern)) {
    AbraString* pattern_str = (AbraString*) AS_OBJ(_pattern);
    pattern = pattern_str->data;
    pattern_size = pattern_str->size;
  }
  if (self->size < pattern_size) {
    return alloc_string(self->data, self->size);
  }

  size_t start_offset = 0;
  for (int i = 0; i < self->size; i += pattern_size) {
    if (strncmp(self->data + i, pattern, pattern_size) != 0) break;
    start_offset += pattern_size;
  }

  return alloc_string(self->data + start_offset, self->size - start_offset);
}

// trimEnd(pattern: String = ""): String
AbraValue ABRA_METHOD_NAME(std, String, trimEnd)(void* _env, AbraValue _self, AbraValue _pattern) {
  AbraString* self = (AbraString*)AS_OBJ(_self);

  char const* pattern = " ";
  size_t pattern_size = 1;
  if (!IS_NONE(_pattern)) {
    AbraString* pattern_str = (AbraString*) AS_OBJ(_pattern);
    pattern = pattern_str->data;
    pattern_size = pattern_str->size;
  }
  if (self->size < pattern_size) {
    return alloc_string(self->data, self->size);
  }

  size_t size = self->size;
  char* new_str = strdup(self->data);
  for (int i = self->size - pattern_size; i >= 0; i -= pattern_size) {
    if (strncmp(new_str + i, pattern, pattern_size) != 0) break;
    new_str[i] = 0;
    size -= pattern_size;
  }

  AbraValue ret = alloc_string(new_str, size);
  free(new_str);
  return ret;
}

AbraValue split_string_by(char* self, size_t self_size, char* splitter, size_t splitter_size) {
  if (self_size < splitter_size) {
    AbraValue* items = GC_MALLOC(sizeof(AbraValue) * 1);
    items[0] = alloc_string(self, self_size);
    return alloc_array(items, 1);
  }

  size_t items_max = 4;
  AbraValue* items = GC_MALLOC(sizeof(AbraValue) * items_max);

  // Scan through string, testing for splitter pattern at each character. If match,
  // store preceding slice into items array, extending array length if necessary.
  size_t idx = 0;
  size_t slice_start = 0;
  size_t cursor = 0;
  for (; cursor < self_size; ++cursor) {
    if (strncmp(self + cursor, splitter, splitter_size) == 0) {
      size_t slice_size = cursor - slice_start;

      // If splitter is "", skip leading zero-length slice
      if (splitter_size == 0 && cursor == 0) continue;

      char* slice = malloc(sizeof(char) * slice_size);
      memcpy(slice, self + slice_start, slice_size);

      if (idx >= items_max) {
        items_max *= 2;
        items = GC_REALLOC(items, sizeof(AbraValue) * items_max);
      }

      items[idx] = alloc_string(slice, slice_size);
      free(slice);

      idx++;
      cursor += splitter_size;
      slice_start = cursor;
    }
  }

  // If we cannot fit one more item in the array, extend it by 1 to fit the final segment.
  // There is always at least 1 segment returned from this function.
  if (idx >= items_max) {
    items_max += 1;
    items = GC_REALLOC(items, sizeof(AbraValue) * items_max);
  }
  size_t slice_size = cursor - slice_start;
  char* slice = malloc(sizeof(char) * slice_size);
  memcpy(slice, self + slice_start, slice_size);
  items[idx] = alloc_string(slice, slice_size);
  free(slice);

  return alloc_array(items, idx + 1);
}

// split(splitter: String): String[]
AbraValue ABRA_METHOD_NAME(std, String, split)(void* _env, AbraValue _self, AbraValue _splitter) {
  AbraString* self = (AbraString*)AS_OBJ(_self);
  AbraString* splitter = (AbraString*)AS_OBJ(_splitter);

  return split_string_by(self->data, self->size, splitter->data, splitter->size);
}

// splitAt(index: Int): (String, String)
AbraValue ABRA_METHOD_NAME(std, String, splitAt)(void* _env, AbraValue _self, AbraValue _index) {
  AbraString* self = (AbraString*)AS_OBJ(_self);
  int64_t index = AS_INT(_index);

  AbraValue* tuple_items = GC_MALLOC(sizeof(AbraValue) * 2);
  if (index >= self->size) {
    tuple_items[0] = alloc_string(self->data, self->size);
    tuple_items[1] = alloc_string("", 0);
  } else if (index < (-1 * (int64_t)self->size)) {
    tuple_items[0] = alloc_string("", 0);
    tuple_items[1] = alloc_string(self->data, self->size);
  } else {
    size_t split_idx = (self->size + index) % self->size;

    char* l = strdup(self->data);
    l[split_idx] = 0;
    tuple_items[0] = alloc_string(l, split_idx);

    char* r = self->data + split_idx;
    tuple_items[1] = alloc_string(r, self->size - split_idx);
  }

  return alloc_tuple(tuple_items, 2);
}

// lines(): String[]
AbraValue ABRA_METHOD_NAME(std, String, lines)(void* _env, AbraValue _self) {
  AbraString* self = (AbraString*)AS_OBJ(_self);
  return split_string_by(self->data, self->size, "\n", 1);
}

// chars(): String[]
AbraValue ABRA_METHOD_NAME(std, String, chars)(void* _env, AbraValue _self) {
  AbraString* self = (AbraString*)AS_OBJ(_self);
  return split_string_by(self->data, self->size, "", 0);
}

// parseInt(radix?: Int): Int?
AbraValue ABRA_METHOD_NAME(std, String, parseInt)(void* _env, AbraValue _self, AbraValue _radix) {
  AbraString* self = (AbraString*)AS_OBJ(_self);
  int radix = 10;
  if (!IS_NONE(_radix)) {
      radix = (int)AS_INT(_radix);
  }

  int64_t val = strtoll(self->data, NULL, radix);
  if (val == 0) {
    for (int i = 0; i < self->size; ++i) {
      char c = self->data[i];
      if (c == '0') break;
      if (!isdigit(c)) return ABRA_NONE;
    }
  }
  return NEW_INT(val);
}

// parseFloat(): Float?
AbraValue ABRA_METHOD_NAME(std, String, parseFloat)(void* _env, AbraValue _self) {
  AbraString* self = (AbraString*)AS_OBJ(_self);

  double val = strtod(self->data, NULL);
  if (val == 0) {
    for (int i = 0; i < self->size; ++i) {
      char c = self->data[i];
      if (c == '0' || c == '.') break;
      if (!isdigit(c)) return ABRA_NONE;
    }
  }
  return NEW_FLOAT(val);
}

// concat(str: Any, *others: Any[]): String
AbraValue ABRA_METHOD_NAME(std, String, concat)(void* _env, AbraValue _self, AbraValue _str, AbraValue _others) {
  AbraString* self = (AbraString*)AS_OBJ(_self);

  char const* str = std__to_string(_str);
  size_t str_size = strlen(str);

  AbraArray* others = (AbraArray*)AS_OBJ(_others);
  size_t others_size = IS_NONE(_others) ? 0 : others->size;

  size_t len = self->size + str_size;
  char* concat = malloc(sizeof(char) * len);
  memcpy(concat, self->data, self->size);
  memcpy(concat + self->size, str, str_size);

  size_t offset = len;
  for (int i = 0; i < others_size; ++i) {
    char const* s = std__to_string(others->items[i]);
    size_t s_len = strlen(s);
    len += s_len;
    concat = realloc(concat, sizeof(char) * len);
    memcpy(concat + offset, s, s_len);
    offset += s_len;
  }

  AbraValue ret = alloc_string(concat, len);
  free(concat);
  return ret;
}

// replaceAll(pattern: String, replacement: String): String
AbraValue ABRA_METHOD_NAME(std, String, replaceAll)(void* _env, AbraValue _self, AbraValue _pattern, AbraValue _replacement) {
  AbraString* self = (AbraString*)AS_OBJ(_self);
  AbraString* pattern = (AbraString*)AS_OBJ(_pattern);
  AbraString* replacement = (AbraString*)AS_OBJ(_replacement);

  size_t max_len = ceil(self->size / (double)pattern->size) * replacement->size;
  char* new_str = malloc(sizeof(char) * max_len);
  size_t cursor = 0;
  for (int i = 0; i < self->size; ++i) {
    if (strncmp(self->data + i, pattern->data, pattern->size) == 0) {
      memcpy(new_str + cursor, replacement->data, replacement->size);
      cursor += replacement->size;
    } else {
      memcpy(new_str + cursor, self->data + i, 1);
      cursor += 1;
    }
  }

  AbraValue ret = alloc_string(new_str, cursor);
  free(new_str);
  return ret;
}

#endif
