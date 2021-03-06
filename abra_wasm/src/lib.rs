extern crate serde;
extern crate serde_derive;
extern crate serde_json;
extern crate js_sys;
extern crate wasm_bindgen;

mod js_value;

use crate::js_value::error::JsWrappedError;
use serde::ser::{Serializer, SerializeSeq};
use serde::Serialize;
use wasm_bindgen::prelude::*;
use abra_core::{Error, typecheck, compile, compile_and_disassemble};
use abra_core::vm::value::{Value, FnValue, ClosureValue, NativeFn, TypeValue, EnumValue, NativeInstanceObj};
use abra_core::vm::vm::{VMContext, VM};
use abra_core::common::display_error::DisplayError;
use abra_core::parser::ast::ModuleId;
use abra_core::module_loader::{ModuleReader, ModuleLoader};
use abra_core::builtins::common::to_string;

pub struct RunResultValue(Option<Value>);

impl Serialize for RunResultValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        if self.0.is_none() {
            return serializer.serialize_none();
        }

        match &self.0.as_ref().unwrap() {
            Value::Nil => serializer.serialize_none(),
            Value::Int(val) => serializer.serialize_i64(*val),
            Value::Float(val) => serializer.serialize_f64(*val),
            Value::Bool(val) => serializer.serialize_bool(*val),
            Value::Str(val) => serializer.serialize_str(val),
            Value::StringObj(o) => {
                serializer.serialize_str(&*o.borrow()._inner)
            }
            Value::ArrayObj(o) => {
                let arr = &*o.borrow();
                let array = &arr._inner;
                let mut arr = serializer.serialize_seq(Some((*array).len()))?;
                array.iter().for_each(|val| {
                    arr.serialize_element(&RunResultValue(Some((*val).clone()))).unwrap();
                });
                arr.end()
            }
            Value::TupleObj(o) => {
                let tup = &*o.borrow();
                let mut arr = serializer.serialize_seq(Some((*tup).len()))?;
                tup.iter().for_each(|val| {
                    arr.serialize_element(&RunResultValue(Some((*val).clone()))).unwrap();
                });
                arr.end()
            }
            Value::SetObj(o) => {
                let set = &*o.borrow();
                let items = &set._inner;
                let mut set = serializer.serialize_seq(Some((*items).len()))?;
                items.iter().for_each(|val| {
                    set.serialize_element(&RunResultValue(Some((*val).clone()))).unwrap();
                });
                set.end()
            }
            Value::MapObj(o) => {
                let map = &*o.borrow();
                let map = &map._inner;
                let mut obj = serializer.serialize_map(Some((*map).len()))?;
                map.into_iter().for_each(|(key, val)| {
                    obj.serialize_entry(&RunResultValue(Some(key.clone())), &RunResultValue(Some(val.clone()))).unwrap();
                });
                obj.end()
            }
            Value::InstanceObj(o) => {
                let inst = &*o.borrow();

                let fields = &inst.fields;
                let mut arr = serializer.serialize_seq(Some(fields.len()))?;
                fields.into_iter().for_each(|val| {
                    arr.serialize_element(&RunResultValue(Some((*val).clone()))).unwrap();
                });
                arr.end()
            }
            Value::NativeInstanceObj(o) => {
                let NativeInstanceObj { inst, .. } = &*o.borrow();

                let fields = &inst.get_field_values();
                let mut arr = serializer.serialize_seq(Some(fields.len()))?;
                for field_value in inst.get_field_values() {
                    arr.serialize_element(&RunResultValue(Some(field_value)))?;
                }

                arr.end()
            }
            Value::EnumVariantObj(o) => serializer.serialize_str(&*o.borrow().name),
            Value::Fn(FnValue { name, .. }) => serializer.serialize_str(name),
            Value::Closure(ClosureValue { name, .. }) => serializer.serialize_str(name),
            Value::NativeFn(NativeFn { name, .. }) => serializer.serialize_str(name),
            Value::Type(TypeValue { name, .. }) => serializer.serialize_str(name),
            Value::Enum(EnumValue { name, .. }) => serializer.serialize_str(name),
        }
    }
}

pub struct RunResult(Result<Option<(Value, String)>, Error>, String);

impl Serialize for RunResult {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        let mut obj = serializer.serialize_map(Some(2))?;

        match &self.0 {
            Ok(value) => {
                obj.serialize_entry("success", &true)?;
                match value {
                    None => {}
                    Some((value, to_str_value)) => {
                        obj.serialize_entry("data", &RunResultValue(Some((*value).clone())))?;
                        obj.serialize_entry("dataToString", to_str_value)?;
                    }
                }
            }
            Err(error) => {
                obj.serialize_entry("success", &false)?;
                obj.serialize_entry("error", &JsWrappedError(&error, &self.1))?;
                obj.serialize_entry("errorMessage", &error.get_message(&self.1))?;
            }
        };

        obj.end()
    }
}

pub struct TypecheckedResult(Result<(), Error>, String);

impl Serialize for TypecheckedResult {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        match &self.0 {
            Ok(_) => {
                let mut obj = serializer.serialize_map(Some(1))?;
                obj.serialize_entry("success", &true)?;
                obj.end()
            }
            Err(error) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("success", &false)?;
                obj.serialize_entry("error", &JsWrappedError(error, &self.1))?;
                obj.serialize_entry("errorMessage", &error.get_message(&self.1))?;
                obj.end()
            }
        }
    }
}

pub struct DisassembleResult(Result<String, Error>, String);

impl Serialize for DisassembleResult {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        match &self.0 {
            Ok(result) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("success", &true)?;
                obj.serialize_entry("disassembled", result)?;
                obj.end()
            }
            Err(error) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("success", &false)?;
                obj.serialize_entry("error", &JsWrappedError(error, &self.1))?;
                obj.serialize_entry("errorMessage", &error.get_message(&self.1))?;
                obj.end()
            }
        }
    }
}

struct UnimplementedModuleReader;

impl ModuleReader for UnimplementedModuleReader {
    fn read_module(&mut self, module_id: &ModuleId) -> Option<String> {
        let msg = format!("Could not load module '{}'; please provide a valid ModuleReader instance", module_id.get_name());
        wasm_bindgen::throw_str(msg.as_str());
    }
}

#[wasm_bindgen]
extern "C" {
    pub type AbraContext;

    #[wasm_bindgen(method, js_name = println)]
    pub fn println(this: &AbraContext, input: &str);

    pub type JsModuleReader;

    #[wasm_bindgen(method, js_name = readModule)]
    pub fn _read_module(this: &JsModuleReader, module_name: &str) -> Option<String>;

    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn console_log(s: &str);
}

impl ModuleReader for JsModuleReader {
    fn read_module(&mut self, module_id: &ModuleId) -> Option<String> {
        let module_name = module_id.get_name();
        self._read_module(&module_name)
    }
}

#[wasm_bindgen(js_name = disassemble)]
pub fn disassemble(input: &str) -> JsValue {
    let module_reader = UnimplementedModuleReader;
    let module_id = ModuleId::from_name(".repl");
    let result = compile_and_disassemble(module_id, &input.to_string(), module_reader);
    let disassemble_result = DisassembleResult(result, input.to_string());
    JsValue::from_serde(&disassemble_result)
        .unwrap_or(JsValue::NULL)
}

#[wasm_bindgen(js_name = disassembleModule)]
pub fn disassemble_module(module_name: &str, module_reader: JsModuleReader) -> JsValue {
    let input = match module_reader._read_module(module_name) {
        None => wasm_bindgen::throw_str(format!("Could not load module '{}'", module_name).as_str()),
        Some(input) => input
    };

    let module_id = ModuleId::from_name(module_name);
    let result = compile_and_disassemble(module_id, &input.to_string(), module_reader);
    let disassemble_result = DisassembleResult(result, input.to_string());
    JsValue::from_serde(&disassemble_result)
        .unwrap_or(JsValue::NULL)
}

#[wasm_bindgen(js_name = typecheck)]
pub fn typecheck_input(input: &str) -> JsValue {
    let module_reader = UnimplementedModuleReader;
    let module_id = ModuleId::from_name(".repl");
    let mut module_loader = ModuleLoader::new(module_reader);
    let result = typecheck(module_id, &input.to_string(), &mut module_loader).map(|_| ());
    let typecheck_result = TypecheckedResult(result, input.to_string());
    JsValue::from_serde(&typecheck_result)
        .unwrap_or(JsValue::NULL)
}

#[wasm_bindgen(js_name = typecheckModule)]
pub fn typecheck_module(module_name: &str, module_reader: JsModuleReader) -> JsValue {
    let input = match module_reader._read_module(module_name) {
        None => wasm_bindgen::throw_str(format!("Could not load module '{}'", module_name).as_str()),
        Some(input) => input
    };

    let module_id = ModuleId::from_name(module_name);
    let mut module_loader = ModuleLoader::new(module_reader);
    let result = typecheck(module_id, &input.to_string(), &mut module_loader).map(|_| ());
    let typecheck_result = TypecheckedResult(result, input.to_string());
    JsValue::from_serde(&typecheck_result)
        .unwrap_or(JsValue::NULL)
}

fn compile_and_run<R>(module_name: &str, input: String, ctx: VMContext, module_reader: R) -> Result<Option<(Value, String)>, Error>
    where R: ModuleReader
{
    let module_id = ModuleId::from_name(module_name);
    let modules = compile(module_id, &input, module_reader)?;
    let mut vm = VM::new(ctx);
    let mut res = None;

    for module in modules {
        match vm.run(module) {
            Ok(Some(v)) => res = Some(v),
            Ok(None) => res = None,
            Err(e) => return Err(Error::InterpretError(e)),
        }
    }
    let res = res.map(|r| {
        let as_str = to_string(&r, &mut vm);
        (r, as_str)
    });
    Ok(res)
}

#[wasm_bindgen(js_name = run)]
pub fn run(input: &str, js_bridge: Option<AbraContext>) -> JsValue {
    let ctx = VMContext {
        print: match js_bridge {
            Some(ctx) => Box::new(move |input| {
                let input = if input.ends_with('\n') {
                    &input[0..(input.len() - 1)]
                } else {
                    input
                };
                ctx.println(input)
            }),
            None => Box::new(|input| console_log(input))
        }
    };

    let module_reader = UnimplementedModuleReader;
    let result = compile_and_run(".repl", input.to_string(), ctx, module_reader);
    let run_result = RunResult(result, input.to_string().clone());
    JsValue::from_serde(&run_result)
        .unwrap_or(JsValue::NULL)
}

#[wasm_bindgen(js_name = runModule)]
pub fn run_module(module_name: &str, module_reader: JsModuleReader, js_bridge: Option<AbraContext>) -> JsValue {
    let ctx = VMContext {
        print: match js_bridge {
            Some(ctx) => Box::new(move |input| {
                let input = if input.ends_with('\n') {
                    &input[0..(input.len() - 1)]
                } else {
                    input
                };
                ctx.println(input)
            }),
            None => Box::new(|input| console_log(input))
        }
    };

    let input = match module_reader._read_module(module_name) {
        None => wasm_bindgen::throw_str(format!("Could not load module '{}'", module_name).as_str()),
        Some(input) => input
    };

    let result = compile_and_run(module_name, input.to_string(), ctx, module_reader);
    let run_result = RunResult(result, input.to_string().clone());
    JsValue::from_serde(&run_result)
        .unwrap_or(JsValue::NULL)
}
