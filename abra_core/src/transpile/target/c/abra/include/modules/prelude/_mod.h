#ifndef __ABRA_MODULE_PRELUDE_H
#define __ABRA_MODULE_PRELUDE_H

#include "../../abra_module.h"
#include "abra_int.h"
#include "abra_float.h"
#include "abra_string.h"
#include "abra_array.h"
#include "abra_tuple.h"
#include "abra_map.h"
#include "abra_set.h"
#include "abra_function.h"
#include "abra_result.h"

ABRA_MODULE(prelude) {
    // Bind eq functions for primitive types
    eq_fns[OBJ_STR] = &std_string__eq;
    eq_fns[OBJ_ARRAY] = &std_array__eq;
    eq_fns[OBJ_TUPLE] = &std_tuple__eq;
    eq_fns[OBJ_MAP] = &std_map__eq;
    eq_fns[OBJ_SET] = &std_set__eq;
    eq_fns[OBJ_FUNCTION] = &std_function__eq;

    // Bind toString functions for primitive types
    to_string_fns[OBJ_STR] = &std_string__to_string;
    to_string_fns[OBJ_ARRAY] = &std_array__to_string;
    to_string_fns[OBJ_TUPLE] = &std_tuple__to_string;
    to_string_fns[OBJ_MAP] = &std_map__to_string;
    to_string_fns[OBJ_SET] = &std_set__to_string;
    to_string_fns[OBJ_FUNCTION] = &std_function__to_string;

    // Bind hash functions for primitive types
    hash_fns[OBJ_STR] = &std_string__hash;
    hash_fns[OBJ_ARRAY] = &std_array__hash;
    hash_fns[OBJ_TUPLE] = &std_tuple__hash;
    hash_fns[OBJ_MAP] = &std_map__hash;
    hash_fns[OBJ_SET] = &std_set__hash;
    hash_fns[OBJ_FUNCTION] = &std_function__hash;

    // Set up Result enum
    TYPE_SETUP(std, Result);
    ENUM_VARIANT_SETUP(std, Result, Ok, 1)
    ENUM_VARIANT_SETUP(std, Result, Err, 1)
}

#endif
