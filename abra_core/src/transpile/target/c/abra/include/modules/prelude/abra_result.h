#ifndef __ABRA_RESULT_H
#define __ABRA_RESULT_H

#include "../../abra_module.h"

/*
enum Result<V, E> {
  Ok(value: T)
  Err(error: E)
}
*/
ABRA_INIT_ENUM(std, Result, Ok, Err)
ABRA_INIT_ENUM_VARIANT(std, Result, Ok, value)
ABRA_INIT_ENUM_VARIANT(std, Result, Err, error)
ABRA_DEFINE_ENUM(std, Result, Ok, Err)
ABRA_DEFINE_ENUM_VARIANT(std, Result, Ok, value)
ABRA_DEFINE_ENUM_VARIANT(std, Result, Err, error)

// map<T>(fn: (V) => T): Result<T, E>
AbraValue ABRA_METHOD_NAME(std, Result, map)(void* _env, AbraValue _self, AbraValue _fn) {
    std__Result* self = (std__Result*)AS_OBJ(_self);
    if (self->_header.enum_variant_idx == std__Result_Variant_Err_idx) return _self;

    AbraFunction* fn = (AbraFunction*)AS_OBJ(_fn);
    AbraValue value = self->Ok.value;
    AbraValue new_value = call_fn_1((callable_ctx__1_t*)fn->ctx, value);
    return std__Result__new_Ok(NULL, new_value);
}

#endif
