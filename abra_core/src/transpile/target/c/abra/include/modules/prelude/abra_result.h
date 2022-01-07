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

#endif
