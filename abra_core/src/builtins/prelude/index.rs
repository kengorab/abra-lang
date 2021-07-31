use abra_native::abra_function;
use crate::typechecker::types::Type;
use crate::builtins::prelude::{NativeInt, NativeSet, NativeFloat, NativeString, NativeArray, NativeMap, Process};
use crate::builtins::common::to_string;
use crate::builtins::native_module_builder::{ModuleSpec, TypeSpec, ModuleSpecBuilder};
use crate::vm::value::Value;
use crate::builtins::arguments::Arguments;
use crate::vm::vm::VM;
use itertools::Itertools;

#[abra_function(signature = "println(*items: Any[])")]
fn println(args: Arguments, vm: &mut VM) {
    let vals = args.varargs();
    let string = vals.into_iter()
        .map(|val| to_string(&val, vm))
        .join(" ");
    (vm.ctx.print)(&format!("{}\n", string));
}

#[abra_function(signature = "print(*items: Any[])")]
fn print(args: Arguments, vm: &mut VM) {
    let vals = args.varargs();
    let string = vals.into_iter()
        .map(|val| to_string(&val, vm))
        .join(" ");
    (vm.ctx.print)(&format!("{}", string));
}

#[abra_function(signature = "range(from: Int, to: Int, increment?: Int): Int[]")]
fn range(mut args: Arguments) -> Value {
    let mut from = args.next_int();
    let to = args.next_int();
    let increment = args.next_int_or_default(1);

    let size = (to - from).abs() / increment;
    let mut values = Vec::with_capacity(size as usize);

    while from < to {
        values.push(Value::Int(from));
        from += increment;
    }

    Value::new_array_obj(values)
}

#[cfg(test)]
pub static PRELUDE_PRINTLN_INDEX: usize = 0;
#[cfg(test)]
pub static PRELUDE_RANGE_INDEX: usize = 2;
#[cfg(test)]
pub static PRELUDE_STRING_INDEX: usize = 8;
#[cfg(test)]
pub static NUM_PRELUDE_BINDINGS: usize = 15;

pub static PRELUDE_PROCESS_INDEX: usize = 4;

pub fn load_module() -> ModuleSpec {
    ModuleSpecBuilder::new("prelude")
        .add_function(println__gen_spec)
        .add_function(print__gen_spec)
        .add_function(range__gen_spec)
        .add_binding("None", Type::Option(Box::new(Type::Placeholder)), Value::Nil)
        .add_binding("process", Type::Reference("prelude/Process".to_string(), vec![]), Value::Nil)
        .add_type(
            TypeSpec::builder("Int", Type::Int)
                .with_native_value::<NativeInt>()
        )
        .add_type(
            TypeSpec::builder("Float", Type::Float)
                .with_native_value::<NativeFloat>()
        )
        .add_type(TypeSpec::builder("Bool", Type::Bool))
        .add_type(
            TypeSpec::builder("String", Type::String)
                .with_native_value::<NativeString>()
        )
        .add_type(TypeSpec::builder("Unit", Type::Unit))
        .add_type(TypeSpec::builder("Any", Type::Any))
        .add_type(
            TypeSpec::builder("Array", Type::Array(Box::new(Type::Generic("T".to_string()))))
                .with_typeref()
                .with_native_value::<NativeArray>()
        )
        .add_type(
            TypeSpec::builder("Map", Type::Map(Box::new(Type::Generic("K".to_string())), Box::new(Type::Generic("V".to_string()))))
                .with_typeref()
                .with_native_value::<NativeMap>()
        )
        .add_type(
            TypeSpec::builder("Set", Type::Set(Box::new(Type::Generic("T".to_string()))))
                .with_typeref()
                .with_native_value::<NativeSet>()
        )
        .add_type_impl::<Process>()
        .build()
}

#[cfg(test)]
mod test {
    use crate::builtins::test_utils::{interpret, interpret_get_result};
    use crate::vm::value::Value;

    #[test]
    fn test_importing_module_explicitly_fails() {
        let imports = &["println", "print", "range", "Int", "Float", "Bool", "String", "Unit", "Any", "Array", "Map", "Set", "Process", "process"];
        for import in imports {
            let result = interpret_get_result(format!("import {} from prelude", import));
            assert!(result.is_err());
        }
    }

    #[test]
    fn test_range() {
        let result = interpret("range(0, 4)");
        let expected = array![Value::Int(0), Value::Int(1), Value::Int(2), Value::Int(3)];
        assert_eq!(expected, result);

        let result = interpret("range(0, -4)");
        let expected = array![];
        assert_eq!(expected, result);

        let result = interpret("range(0, 10, 2)");
        let expected = array![Value::Int(0), Value::Int(2), Value::Int(4), Value::Int(6), Value::Int(8)];
        assert_eq!(expected, result);

        let result = interpret("range(1, 10, 3)");
        let expected = array![Value::Int(1), Value::Int(4), Value::Int(7)];
        assert_eq!(expected, result);
    }

    // TODO: Convert VMContext to a trait to allow for mocked-out testing?
    // #[test]
    // fn test_println() {
    //     let mut printed = None;
    //     let ctx = VMContext {
    //         print: |val| printed = Some(val)
    //     };
    //     let result = interpret_get_result_with_vm_ctx("println(\"hello world\")", ctx);
    //
    //     assert!(result.is_ok());
    //     assert_eq!(Some("hello world"), printed);
    // }
}
