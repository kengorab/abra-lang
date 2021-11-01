#ifndef __ABRA_MAP_H
#define __ABRA_MAP_H

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

#endif
