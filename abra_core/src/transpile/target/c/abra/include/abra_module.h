#ifndef __ABRA_MODULE_H
#define __ABRA_MODULE_H

#include "abra_value.h"
#include "macro_utils.h"

/**
 * Here be dragons: This file contains a lot of macro dark magic in order
 * to make the process of defining native types very easy. Be extremely
 * careful when modifying.
 *
 * Each macro is fairly well-documented, but nearly all of them rely heavily
 * on macros in `macro_utils.h`, which are some insanely brilliant (but very
 * complex) utility macros. Doing all of this stuff at compile-time is pretty
 * involved, but ultimately streamlines some repetitive and highly error-prone
 * processes.
 */

/**
 * Define the signature for a function in an Abra module, including the
 * _val top-level variable which represents the allocated function value.
 * If the function accepts no arguments, the `_0` macro can be used instead.
 * @see FN_SETUP
 */
#define _METHOD_ARG(field) AbraValue field
#define ABRA_DEFINE_FN(mod, name, ...) \
  AbraValue mod##__##name##_val; \
  AbraValue mod##__##name(void* _env, MAP_LIST(_METHOD_ARG, __VA_ARGS__))
#define ABRA_DEFINE_FN_0(mod, name) \
  AbraValue mod##__##name##_val; \
  AbraValue mod##__##name(void* _env)

/**
 * Perform setup for the function, allocating a function object and its
 * environment, and assigning it to the _val top-level variable.
 * @see ABRA_DEFINE_FN
 */
#define FN_SETUP(name, arity, func) \
  do {                             \
    callable_ctx__##arity##_t* func##_env_ctx = GC_MALLOC(sizeof(callable_ctx__##arity##_t)); \
    func##_env_ctx->fn = &func;                                       \
    func##_env_ctx->env = NULL;                                               \
    func##_val = alloc_function(#name, #func, (void*) func##_env_ctx); \
  } while (0)

/**
 * Assign __type_id value for the given type, using the auto-incrementing
 * __next_type_id value. Also, insert the eq/to_string/hash functions into
 * the arrays at the appropriate index as determined by this __type_id.
 */
#define TYPE_SETUP(mod, name) \
  do {\
    mod##__##name##__type_id = __next_type_id++; \
    eq_fns[mod##__##name##__type_id] = &mod##__##name##__eq; \
    to_string_fns[mod##__##name##__type_id] = &mod##__##name##__to_string; \
    hash_fns[mod##__##name##__type_id] = &mod##__##name##__hash; \
  } while (0)

#define ABRA_MODULE(mod) void init_module_##mod()

/**
 * Generate the struct typedef for the type.
 */
#define _GEN_FIELD_DEF(field) AbraValue field;
#define GEN_TYPEDEF(mod, type, ...) \
  typedef struct mod##__##type { \
    Obj _header;             \
    MAP(_GEN_FIELD_DEF, __VA_ARGS__)                         \
  } mod##__##type; \
  size_t mod##__##type##__type_id;

/**
 * Generate the `new` function to initialize a new instance
 * of the given type.
 */
#define _GEN_ARG(field) AbraValue field
#define _GEN_ASSN_LINE(field) self->field = field;
#define GEN_METHOD_INIT(mod, type_name, ...) \
  AbraValue mod##__##type_name##__new( \
    MAP_LIST(_GEN_ARG, year, month, day, hour, minute, second) \
  ) { \
    mod##__##type_name* self = GC_MALLOC(sizeof(mod##__##type_name)); \
    self->_header.type = OBJ_INSTANCE; \
    self->_header.type_id = mod##__##type_name##__type_id; \
    MAP(_GEN_ASSN_LINE, __VA_ARGS__) \
    return (AbraValue) {.type = ABRA_TYPE_OBJ, .as = {.obj = ((Obj*) self)}}; \
  }

/**
 * Generate the `eq` method for the given type, comparing each
 * field of `self` with the corresponding field of `other`, using
 * the `std__eq` function, which references `eq_fns`.
 * @see TYPE_SETUP
 */
#define _GEN_EQ_LINE(field) if (!std__eq(self->field, other->field)) return false;
#define GEN_METHOD_EQ(mod, type_name, ...) \
  bool mod##__##type_name##__eq(Obj* _self, Obj* _other) { \
    mod##__##type_name* self = (mod##__##type_name*) _self; \
    mod##__##type_name* other = (mod##__##type_name*) _other; \
    MAP(_GEN_EQ_LINE, __VA_ARGS__) \
    return true; \
  }

/**
 * Generate the `to_string` method for the given type, of the form
 *   `TypeName(field1: value1, ...)`
 * The generated function will call `std__to_string` for each field,
 * which references `to_string_fns`.
 * @see TYPE_SETUP
 */
#define _GEN_TOSTRING_LINES(field) \
  char const* field##_str = std__to_string(self->field); \
  size_t field##_str_len = strlen(field##_str);
#define _GEN_BASE_STR(field) #field ": %s, "
#define _FORMAT_STR(type, ...) #type "(" MAP(_GEN_BASE_STR, __VA_ARGS__)
#define _FORMAT_STR_SIZE(type, ...) sizeof(_FORMAT_STR(type, __VA_ARGS__)) - (2 * GET_ARG_COUNT(__VA_ARGS__)) - 1
#define _PLUS_LEN(field) + field##_str_len
#define _STRING_LEN(type, ...) sizeof(char) * (_FORMAT_STR_SIZE(type, __VA_ARGS__)) MAP(_PLUS_LEN, __VA_ARGS__) - 1
#define _ADD_STR(field) field##_str
#define _DO_SPRINTF(...) sprintf(str, format_str, MAP_LIST(_ADD_STR, __VA_ARGS__))
#define GEN_METHOD_TOSTRING(mod, type, ...) \
  char const* mod##__##type##__to_string(Obj* obj) { \
    mod##__##type* self = (mod##__##type*) obj; \
    MAP(_GEN_TOSTRING_LINES, __VA_ARGS__) \
    char* format_str = _FORMAT_STR(type, __VA_ARGS__); \
    size_t str_len = _STRING_LEN(type, __VA_ARGS__); \
    char* str = malloc(str_len); \
    _DO_SPRINTF(__VA_ARGS__); \
    str[str_len - 1] = ')'; \
    str[str_len] = 0; \
    return str; \
  }

/**
 * Generate the `hash` method for the given type. The generated method
 * will call `std__hash` on each field and assemble the hash value based
 * on those values. The `std__hash` function references `hash_fns`.
 * @see TYPE_SETUP
 */
#define _GEN_HASH_LINE(field) hash = 31 * hash + std__hash(self->field);
#define GEN_METHOD_HASH(mod, type, ...) \
  size_t mod##__##type##__hash(Obj* _self) { \
    mod##__##type* self = (mod##__##type*) _self; \
    size_t hash = 1; \
    MAP(_GEN_HASH_LINE, __VA_ARGS__) \
    return hash; \
  }

/**
 * Generates all the code required to have a natively-defined Abra type. This
 * includes the struct typedef, and the implementations of eq/to_string/hash.
 * A type defined in this way will need to be setup in the module's initialization
 * function.
 * @see TYPE_SETUP
 */
#define ABRA_DEFINE_TYPE(mod, type, ...) \
  GEN_TYPEDEF(mod, type, __VA_ARGS__) \
  GEN_METHOD_INIT(mod, type, __VA_ARGS__) \
  GEN_METHOD_EQ(mod, type, __VA_ARGS__) \
  GEN_METHOD_TOSTRING(mod, type, __VA_ARGS__) \
  GEN_METHOD_HASH(mod, type, __VA_ARGS__)

/**
 * Define the signature for a method for a given type in an Abra module.
 * This also casts the receiver value from a generic object into an instance
 * of the receiver's type.
 * If the method accepts no arguments aside from the receiver, the `_0` macro
 * can be used.
 */
#define ABRA_DEFINE_METHOD(mod, type, name, self, ...) \
  AbraValue mod##__##type##__method_##name(void* _env, AbraValue _##self, MAP_LIST(_METHOD_ARG, __VA_ARGS__)) { \
    mod##__##type* self = (mod##__##type*) AS_OBJ(_##self);
#define ABRA_DEFINE_METHOD_0(mod, type, name, self) \
  AbraValue mod##__##type##__method_##name(void* _env, AbraValue _##self) { \
    mod##__##type* self = (mod##__##type*) AS_OBJ(_##self);


#endif
