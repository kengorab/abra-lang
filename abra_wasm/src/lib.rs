extern crate serde;
extern crate serde_derive;
extern crate serde_json;
extern crate wasm_bindgen;
extern crate wasm_bindgen_futures;
extern crate js_sys;
extern crate futures;

mod js_value;

use crate::js_value::module::JsModule;
use crate::js_value::error::JsWrappedError;
use futures::Future;
use serde::ser::{Serializer, SerializeSeq};
use serde::Serialize;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::future_to_promise;

use abra_core::builtins::native_fns::NativeFn;
use abra_core::{Error, typecheck, compile, compile_and_run, compile_and_disassemble};
use abra_core::vm::value::{Obj, Value, FnValue, ClosureValue, TypeValue};
use abra_core::vm::vm::VMContext;
use abra_core::vm::compiler::Module;

pub struct RunResult(Value);

impl Serialize for RunResult {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        match self {
            RunResult(Value::Nil) => serializer.serialize_none(),
            RunResult(Value::Int(val)) => serializer.serialize_i64(*val),
            RunResult(Value::Float(val)) => serializer.serialize_f64(*val),
            RunResult(Value::Bool(val)) => serializer.serialize_bool(*val),
            RunResult(Value::Str(val)) => serializer.serialize_str(val),
            RunResult(Value::Obj(obj)) => match &*obj.borrow() {
                Obj::StringObj(value) => serializer.serialize_str(value),
                Obj::ArrayObj(value) => {
                    let mut arr = serializer.serialize_seq(Some((*value).len()))?;
                    value.into_iter().for_each(|val| {
                        arr.serialize_element(&RunResult((*val).clone())).unwrap();
                    });
                    arr.end()
                }
                Obj::MapObj(value) => {
                    let mut obj = serializer.serialize_map(Some((*value).len()))?;
                    value.into_iter().for_each(|(key, val)| {
                        obj.serialize_entry(key, &RunResult(val.clone())).unwrap();
                    });
                    obj.end()
                }
                Obj::InstanceObj(inst) => {
                    let fields = &inst.fields;
                    let mut arr = serializer.serialize_seq(Some(fields.len()))?;
                    fields.into_iter().for_each(|val| {
                        arr.serialize_element(&RunResult((*val).clone())).unwrap();
                    });
                    arr.end()
                }
            }
            RunResult(Value::Fn(FnValue { name, .. })) => serializer.serialize_str(name),
            RunResult(Value::Closure(ClosureValue { name, .. })) => serializer.serialize_str(name),
            RunResult(Value::NativeFn(NativeFn { name, .. })) => serializer.serialize_str(name),
            RunResult(Value::Type(TypeValue { name, .. })) => serializer.serialize_str(name)
        }
    }
}

pub struct CompileResult(Result<Module, Error>);

impl Serialize for CompileResult {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        match &self.0 {
            Ok(module) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("success", &true)?;
                obj.serialize_entry("module", &JsModule(module))?;
                obj.end()
            }
            Err(error) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("success", &false)?;
                obj.serialize_entry("error", &JsWrappedError(error))?;
                obj.end()
            }
        }
    }
}

pub struct TypecheckedResult(Result<(), Error>);

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
                obj.serialize_entry("error", &JsWrappedError(error))?;
                obj.end()
            }
        }
    }
}

pub struct DisassembleResult(Result<String, Error>);

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
                obj.serialize_entry("error", &JsWrappedError(error))?;
                obj.end()
            }
        }
    }
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen(js_name = disassemble)]
pub fn disassemble(input: &str) -> JsValue {
    let result = compile_and_disassemble(input.to_string());
    let disassemble_result = DisassembleResult(result);
    JsValue::from_serde(&disassemble_result)
        .unwrap_or(JsValue::NULL)
}

#[wasm_bindgen(js_name = typecheck)]
pub fn typecheck_input(input: &str) -> JsValue {
    let result = typecheck(input.to_string())
        .map(|_| ());
    let typecheck_result = TypecheckedResult(result);
    JsValue::from_serde(&typecheck_result)
        .unwrap_or(JsValue::NULL)
}

#[wasm_bindgen(js_name = compile)]
pub fn parse_typecheck_and_compile(input: &str) -> JsValue {
    let result = compile(input.to_string())
        .map(|(module, _)| module);
    let compile_result = CompileResult(result);
    JsValue::from_serde(&compile_result)
        .unwrap_or(JsValue::NULL)
}

#[wasm_bindgen(js_name = runSync)]
pub fn run(input: &str) -> JsValue {
    let ctx = VMContext {
        print: |input| log(input)
    };

    let result = match compile_and_run(input.to_string(), ctx) {
        Ok(Some(value)) => JsValue::from_serde(&RunResult(value)),
        Ok(None) => Ok(JsValue::UNDEFINED),
        Err(error) => JsValue::from_serde(&JsWrappedError(&error))
    };
    result.unwrap_or(JsValue::from("Could not convert result to JSON"))
}

#[wasm_bindgen(js_name = runAsync)]
pub fn run_async(input: &str) -> js_sys::Promise {
    let ctx = VMContext {
        print: |input| log(input)
    };

    let future = futures::future::ok(input.to_string())
        .and_then(move |input| {
            match compile_and_run(input, ctx) {
                Ok(Some(value)) => JsValue::from_serde(&RunResult(value)),
                Ok(None) => Ok(JsValue::UNDEFINED),
                Err(error) => JsValue::from_serde(&JsWrappedError(&error))
            }
        })
        .map_err(|_| {
            JsValue::from("Could not convert result to JSON")
        });
    future_to_promise(future)
}
