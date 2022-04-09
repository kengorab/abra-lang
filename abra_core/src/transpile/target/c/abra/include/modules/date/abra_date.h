#ifndef __ABRA_DATE_DATE_H
#define __ABRA_DATE_DATE_H

#include "../../abra_module.h"

/*
type Date {
  year: Int
  month: Int
  day: Int
  hour: Int
  minute: Int
  second: Int
}
*/
ABRA_DEFINE_TYPE(
  // module
  date,
  // type
  Date,
  // fields
  year, month, day, hour, minute, second
)

// addDays(amount: Int): Date
ABRA_DEFINE_METHOD(date, Date, addDays, self, _amount) //{
  int64_t amount = AS_INT(_amount);

  return date__Date__new(
    self->year,
    self->month,
    NEW_INT(AS_INT(self->day) + amount),
    self->hour,
    self->minute,
    self->second
  );
}

#endif