#ifndef __ABRA_UTILS_H
#define __ABRA_UTILS_H

#include "abra_value.h"

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

#endif
