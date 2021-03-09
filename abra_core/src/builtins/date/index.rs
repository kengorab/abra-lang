use crate::builtins::native_module_builder::{ModuleSpec, ModuleSpecBuilder};
use crate::builtins::date::native_date::NativeDate;

pub fn load_module() -> ModuleSpec {
    ModuleSpecBuilder::new("date")
        .add_type_impl::<NativeDate>()
        .build()
}

#[cfg(test)]
mod test {
    use crate::builtins::test_utils::interpret_get_result;

    #[test]
    fn test_importing_module() {
        let result = interpret_get_result("import Date from date");
        assert!(result.is_ok());
    }
}
