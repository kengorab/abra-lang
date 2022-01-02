#ifndef __ABRA_MODULE_H
#define __ABRA_MODULE_H

#include "abra_value.h"

#define ABRA_FN(mod, name) \
  AbraValue mod##__##name##_val; \
  AbraValue mod##__##name

#define FN_SETUP(name, arity, func) \
  do {                             \
      callable_ctx__##arity##_t* func##_env_ctx = GC_MALLOC(sizeof(callable_ctx__##arity##_t)); \
      func##_env_ctx->fn = &func;                                       \
      func##_env_ctx->env = NULL;                                               \
      func##_val = alloc_function(#name, #func, (void*) func##_env_ctx); \
  } while (0)

#define ABRA_MODULE(mod) void init_module_##mod()

#endif
