#ifndef __ABRA_FUNCTION_H
#define __ABRA_FUNCTION_H

typedef struct AbraFunction {
  Obj _header;
  char const* name;
  char const* c_name;
  void* ctx; // This will be some flavor of callable_ctx__<arity>_t (see callable.h)
} AbraFunction;

AbraValue alloc_function(char* name, char* c_name, void* ctx) {
  AbraFunction* fn = GC_MALLOC(sizeof(AbraFunction));

  fn->_header.type = OBJ_FUNCTION;
  fn->name = strdup(name);
  fn->c_name = strdup(c_name);
  fn->ctx = ctx;

  return ((AbraValue){.type = ABRA_TYPE_OBJ, .as = {.obj = ((Obj*)fn)}});
}

bool std_function__eq(Obj* o1, Obj* o2) {
  AbraFunction* self = (AbraFunction*) o1;
  AbraFunction* other = (AbraFunction*) o2;

  // TODO: comparing envs as well?
  return strcmp(self->c_name, other->c_name) == 0;
}

char const* std_function__to_string(Obj* obj) {
  AbraFunction* self = (AbraFunction*) obj;

  size_t name_len = strlen(self->name);
  char* str = GC_MALLOC(sizeof(char) * 8 + name_len); // <func {name}>
  memcpy(str, "<func ", 6);
  memcpy(str + 6, self->name, name_len);
  memcpy(str + 6 + name_len, ">", 1);
  return str;
}

size_t std_function__hash(Obj* obj) {
  AbraFunction* self = (AbraFunction*) obj;

  size_t name_len = strlen(self->name);

  // Adapted from djb2 hashing algorithm
  size_t hash = 5381;
  for (size_t i = 0; i < name_len; ++i) {
    hash = ((hash << 5) + hash) ^ self->name[i];
  }
  return hash;
}

#endif
