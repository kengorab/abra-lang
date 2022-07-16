#ifndef __ABRA_NAN_H
#define __ABRA_NAN_H

// ------------------------ NAN TAGGING ------------------------

typedef uint64_t value_t;

const uint64_t MASK_NAN =            (uint64_t)0x7ffc000000000000;
const uint64_t MASK_INT = MASK_NAN | (uint64_t)0x0002000000000000;
const uint64_t MASK_OBJ = MASK_NAN | (uint64_t)0x8000000000000000;

const uint64_t VAL_NONE  = MASK_NAN | (uint64_t)0x0001000000000000;
const uint64_t VAL_FALSE = MASK_NAN | (uint64_t)0x0001000000000001;
const uint64_t VAL_TRUE  = MASK_NAN | (uint64_t)0x0001000000000002;

const uint64_t PAYLOAD_MASK_INT = (uint64_t)0x00000000ffffffff;
const uint64_t PAYLOAD_MASK_OBJ = (uint64_t)0x0000ffffffffffff;

#define AS_INT(val)    ((int32_t) (val & PAYLOAD_MASK_INT))
#define AS_DOUBLE(val) ((double) (value_t_to_double(val)))

#define FROM_BOOL(b) (b ? VAL_TRUE : VAL_FALSE)

#define IS_INT(val) ((val & MASK_INT) == MASK_INT)
#define IS_FLOAT(val) ((val & MASK_NAN) != MASK_NAN)
#define IS_OBJ(val) ((val & MASK_OBJ) == MASK_OBJ)

#define AS_OBJ(val, typ)  ((typ*)(val & PAYLOAD_MASK_OBJ))
#define TAG_OBJ(val)      (MASK_OBJ | (uint64_t)val)
#define TAG_INT(i)        (MASK_INT | (uint64_t)i)

typedef union {
  value_t raw;
  double d;
} value_t_transmute;

value_t double_to_value_t(double value) {
  value_t_transmute t = {.d = value};
  return t.raw;
}

double value_t_to_double(value_t value) {
  value_t_transmute t = {.raw = value};
  return t.d;
}

#endif
