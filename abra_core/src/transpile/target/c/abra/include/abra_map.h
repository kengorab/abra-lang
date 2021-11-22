#ifndef __ABRA_MAP_H
#define __ABRA_MAP_H

#include "abra_value.h"
#include "abra_array.h"
#include "abra_set.h"

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

AbraValue std_map__index_assign(Obj* obj, AbraValue key, AbraValue item) {
    AbraMap* self = (AbraMap*)obj;
    hashmap_insert(&self->hash, key, item);
    return item;
}

// ************************************
// * Map fields & methods
// ************************************

// size: Int
AbraValue std_map__field_size(AbraValue _self) {
    AbraMap* self = (AbraMap*)AS_OBJ(_self);
    return NEW_INT(self->hash.size);
}

// static fromPairs<T1, T2>(pairs: (T1, T2)[]): Map<T1, T2>
AbraValue std_map__static_method_fromPairs(void* _env, AbraValue _pairs) {
    AbraArray* pairs = (AbraArray*)AS_OBJ(_pairs);
    AbraValue map = alloc_map();
    for (int i = 0; i < pairs->size; ++i) {
        AbraValue pair = pairs->items[i];
        AbraTuple* tuple = (AbraTuple*)AS_OBJ(pair);
        std_map__insert(AS_OBJ(map), tuple->items[0], tuple->items[1]);
    }

    return map;
}

// isEmpty(): Bool
AbraValue std_map__method_isEmpty(void* _env, AbraValue _self) {
    AbraMap* self = (AbraMap*)AS_OBJ(_self);
    return self->hash.size == 0 ? ABRA_TRUE : ABRA_FALSE;
}

// enumerate(): (K, V)[]
AbraValue std_map__method_enumerate(void* _env, AbraValue _self) {
    AbraMap* self = (AbraMap*)AS_OBJ(_self);

    size_t size = self->hash.size;
    AbraValue* items = GC_MALLOC(sizeof(AbraValue) * size);
    AbraValue* map_keys = hashmap_keys(&self->hash);
    for (int i = 0; i < size; ++i) {
        AbraValue key = map_keys[i];
        AbraValue val = hashmap_get(&self->hash, key);

        AbraValue* tuple_items = GC_MALLOC(sizeof(AbraValue) * 2);
        tuple_items[0] = key;
        tuple_items[1] = val;
        items[i] = alloc_tuple(tuple_items, 2);
    }
    return alloc_array(items, size);
}

// keys(): Set<K>
AbraValue std_map__method_keys(void* _env, AbraValue _self) {
    AbraMap* self = (AbraMap*)AS_OBJ(_self);

    size_t size = self->hash.size;
    AbraValue set = alloc_set();
    AbraValue* map_keys = hashmap_keys(&self->hash);
    for (int i = 0; i < size; ++i) {
        std_set__insert(AS_OBJ(set), map_keys[i]);
    }
    return set;
}

// values(): Set<V>
AbraValue std_map__method_values(void* _env, AbraValue _self) {
    AbraMap* self = (AbraMap*)AS_OBJ(_self);

    size_t size = self->hash.size;
    AbraValue set = alloc_set();
    AbraValue* map_keys = hashmap_keys(&self->hash);
    for (int i = 0; i < size; ++i) {
        AbraValue val = hashmap_get(&self->hash, map_keys[i]);
        std_set__insert(AS_OBJ(set), val);
    }
    return set;
}

// entries(): Set<(K, V)>
AbraValue std_map__method_entries(void* _env, AbraValue _self) {
    AbraMap* self = (AbraMap*)AS_OBJ(_self);

    size_t size = self->hash.size;
    AbraValue set = alloc_set();
    AbraValue* map_keys = hashmap_keys(&self->hash);
    for (int i = 0; i < size; ++i) {
        AbraValue key = map_keys[i];
        AbraValue val = hashmap_get(&self->hash, key);

        AbraValue* tuple_items = GC_MALLOC(sizeof(AbraValue) * 2);
        tuple_items[0] = key;
        tuple_items[1] = val;
        std_set__insert(AS_OBJ(set), alloc_tuple(tuple_items, 2));
    }
    return set;
}

// containsKey(key: K): Bool
AbraValue std_map__method_containsKey(void* _env, AbraValue _self, AbraValue key) {
    AbraMap* self = (AbraMap*)AS_OBJ(_self);
    return IS_NONE(hashmap_get(&self->hash, key)) ? ABRA_FALSE : ABRA_TRUE;
}

// mapValues<U>(fn: (K, V) => U): Map<K, U>
AbraValue std_map__method_mapValues(void* _env, AbraValue _self, AbraValue _fn) {
    AbraMap* self = (AbraMap*)AS_OBJ(_self);
    AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);

    AbraValue new_map = alloc_map();
    AbraValue* keys = hashmap_keys(&self->hash);
    size_t size = self->hash.size;
    for (int i = 0; i < size; ++i) {
        AbraValue key = keys[i];
        AbraValue old_val = hashmap_get(&self->hash, key);
        AbraValue val = call_fn_2((callable_ctx__2_t*)fn->ctx, key, old_val);
        std_map__insert(AS_OBJ(new_map), key, val);
    }
    return new_map;
}

// getOrDefault(key: K, default: V): V
AbraValue std_map__method_getOrDefault(void* _env, AbraValue _self, AbraValue key, AbraValue _default) {
    AbraMap* self = (AbraMap*)AS_OBJ(_self);

    AbraValue val = hashmap_get(&self->hash, key);
    return IS_NONE(val) ? _default : val;
}

// getOrElse(key: K, fn: () => V): V
AbraValue std_map__method_getOrElse(void* _env, AbraValue _self, AbraValue key, AbraValue _fn) {
    AbraMap* self = (AbraMap*)AS_OBJ(_self);
    AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);

    AbraValue val = hashmap_get(&self->hash, key);
    if (IS_NONE(val)) {
        return call_fn_0((callable_ctx__0_t*)fn->ctx);
    }
    return val;
}

// update(key: K, fn: (V) => V)
AbraValue std_map__method_update(void* _env, AbraValue _self, AbraValue key, AbraValue _fn) {
    AbraMap* self = (AbraMap*)AS_OBJ(_self);
    AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);

    AbraValue val = hashmap_get(&self->hash, key);
    if (IS_NONE(val)) return ABRA_NONE;

    AbraValue new_val = call_fn_1((callable_ctx__1_t*)fn->ctx, val);
    std_map__insert(AS_OBJ(_self), key, new_val);

    return ABRA_NONE;
}

// remove(key: K): V?
AbraValue std_map__method_remove(void* _env, AbraValue _self, AbraValue key) {
    AbraMap* self = (AbraMap*)AS_OBJ(_self);
    return hashmap_remove(&self->hash, key);
}

#endif
