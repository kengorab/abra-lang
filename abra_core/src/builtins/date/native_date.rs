use abra_native::{AbraType, abra_methods};
use crate::vm::value::Value;
use std::fmt::Debug;
use std::hash::Hash;
use crate::builtins::arguments::Arguments;

#[derive(AbraType, Debug, Clone, Eq, Hash, PartialEq)]
#[abra_type(module = "date", signature = "Date")]
pub struct NativeDate {
    #[abra_field(name = "year", field_type = "Int")]
    year: i64,

    #[abra_field(name = "month", field_type = "Int")]
    month: i64,

    #[abra_field(name = "day", field_type = "Int")]
    day: i64,

    #[abra_field(name = "hour", field_type = "Int", has_default = true)]
    hour: i64,

    #[abra_field(name = "minute", field_type = "Int", has_default = true)]
    minute: i64,

    #[abra_field(name = "second", field_type = "Int", has_default = true)]
    second: i64,
}

#[abra_methods]
impl NativeDate {
    #[abra_constructor]
    pub(crate) fn new(args: Vec<Value>) -> Self {
        let mut args = Arguments::new("Date", 6, args);

        Self {
            year: args.next_int(),
            month: args.next_int(),
            day: args.next_int(),
            hour: args.next_int_or_default(0),
            minute: args.next_int_or_default(0),
            second: args.next_int_or_default(0),
        }
    }

    #[abra_getter(field = "year")]
    fn get_year(&self) -> Value {
        Value::Int(self.year)
    }

    #[abra_setter(field = "year")]
    fn set_year(&mut self, value: Value) {
        self.year = *value.as_int();
    }

    #[abra_getter(field = "month")]
    fn get_month(&self) -> Value {
        Value::Int(self.month)
    }

    #[abra_setter(field = "month")]
    fn set_month(&mut self, value: Value) {
        self.month = *value.as_int();
    }

    #[abra_getter(field = "day")]
    fn get_day(&self) -> Value {
        Value::Int(self.day)
    }

    #[abra_setter(field = "day")]
    fn set_day(&mut self, value: Value) {
        self.day = *value.as_int();
    }

    #[abra_getter(field = "hour")]
    fn get_hour(&self) -> Value {
        Value::Int(self.hour)
    }

    #[abra_setter(field = "hour")]
    fn set_hour(&mut self, value: Value) {
        self.hour = *value.as_int();
    }

    #[abra_getter(field = "minute")]
    fn get_minute(&self) -> Value {
        Value::Int(self.minute)
    }

    #[abra_setter(field = "minute")]
    fn set_minute(&mut self, value: Value) {
        self.minute = *value.as_int();
    }

    #[abra_getter(field = "second")]
    fn get_second(&self) -> Value {
        Value::Int(self.second)
    }

    #[abra_setter(field = "second")]
    fn set_second(&mut self, value: Value) {
        self.second = *value.as_int();
    }

    #[abra_static_method(signature = "now(): Date")]
    fn now() -> Self {
        Self { year: 0, month: 0, day: 0, hour: 0, minute: 0, second: 0 }
    }

    #[abra_method(signature = "addDays(amount: Int): Date")]
    fn add_days(&self, mut args: Arguments) -> Self {
        let amount = args.next_int();
        Self {
            day: self.day + amount,
            ..self.clone()
        }
    }
}

#[cfg(test)]
mod test {
    use crate::builtins::test_utils::{interpret, new_string_obj};

    #[test]
    fn test_date_construct() {
        let result = interpret(r#"
          import Date from date
          Date(year: 2021, month: 3, day: 9).toString()
        "#);
        let expected = new_string_obj("Date(year: 2021, month: 3, day: 9, hour: 0, minute: 0, second: 0)");
        assert_eq!(expected, result);
    }

    #[test]
    fn test_date_static_now() {
        let result = interpret(r#"
          import Date from date
          Date.now().toString()
        "#);
        let expected = new_string_obj("Date(year: 0, month: 0, day: 0, hour: 0, minute: 0, second: 0)");
        assert_eq!(expected, result);
    }

    #[test]
    fn test_date_add_days() {
        let result = interpret(r#"
          import Date from date
          val now = Date.now()
          now.addDays(1).addDays(1).toString()
        "#);
        let expected = new_string_obj("Date(year: 0, month: 0, day: 2, hour: 0, minute: 0, second: 0)");
        assert_eq!(expected, result);
    }
}
