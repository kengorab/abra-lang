use abra_native::abra_function;
use crate::typechecker::types::Type;
use crate::builtins::native::{NativeInt, NativeSet, NativeDate, to_string, NativeFloat, NativeString, NativeArray, NativeMap};
use crate::builtins::native_module_builder::{ModuleSpec, TypeSpec, ModuleSpecBuilder};
use crate::vm::value::Value;
use crate::builtins::arguments::Arguments;
use crate::vm::vm::VM;

#[abra_function(signature = "println(*items: Any[])")]
fn println(args: Arguments, vm: &mut VM) {
    let print_fn = vm.ctx.print;

    let vals = args.varargs();
    let num_vals = vals.len();
    for (idx, val) in vals.into_iter().enumerate() {
        let sp = if idx == num_vals - 1 { "" } else { " " };
        print_fn(&format!("{}{}", to_string(&val, vm), sp));
    }

    print_fn("\n");
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

#[abra_function(signature = "readFile(path: String): String?")]
fn read_file(mut args: Arguments) -> Value {
    let file_name = args.next_string();
    match std::fs::read_to_string(file_name) {
        Ok(contents) => Value::new_string_obj(contents),
        Err(_) => Value::Nil
    }
}

#[cfg(test)]
pub static PRELUDE_PRINTLN_INDEX: u8 = 0;
#[cfg(test)]
pub static PRELUDE_STRING_INDEX: u8 = 7;

pub fn load_module() -> ModuleSpec {
    ModuleSpecBuilder::new("prelude")
        .add_function(println__gen_spec)
        .add_function(range__gen_spec)
        .add_function(read_file__gen_spec)
        .add_binding("None", Type::Option(Box::new(Type::Placeholder)), Value::Nil)
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
        .add_type_impl::<NativeDate>()
        .build()
}
