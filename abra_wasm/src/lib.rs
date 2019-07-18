extern crate serde;
extern crate serde_derive;
extern crate serde_json;
extern crate wasm_bindgen;

mod js_value;

use serde::ser::{Serializer, SerializeSeq};
use serde::Serialize;
use wasm_bindgen::prelude::*;

use abra_core::{Error, compile, compile_and_run};
use abra_core::vm::value::{Obj, Value};
use abra_core::vm::chunk::CompiledModule;
use crate::js_value::compiled_module::JsCompiledModule;
use crate::js_value::error::JsWrappedError;

pub struct RunResult(Value);

impl Serialize for RunResult {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match self {
            RunResult(Value::Nil) => serializer.serialize_none(),
            RunResult(Value::Int(val)) => serializer.serialize_i64(*val),
            RunResult(Value::Float(val)) => serializer.serialize_f64(*val),
            RunResult(Value::Bool(val)) => serializer.serialize_bool(*val),
            RunResult(Value::Obj(obj)) => match obj {
                Obj::StringObj { value } => serializer.serialize_str(&*value),
                Obj::ArrayObj { value } => {
                    let mut arr = serializer.serialize_seq(Some((*value).len()))?;
                    value.into_iter().for_each(|val| {
                        arr.serialize_element(&RunResult((**val).clone())).unwrap();
                    });
                    arr.end()
                }
                Obj::OptionObj { value } => match value {
                    None => serializer.serialize_none(),
                    Some(value) => serializer.serialize_some(&RunResult(*value.clone()))
                }
            }
            RunResult(Value::Fn(fn_name)) => serializer.serialize_str(fn_name)
        }
    }
}

pub struct CompileResult<'a>(Result<CompiledModule<'a>, Error>);

impl<'a> Serialize for CompileResult<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        match &self.0 {
            Ok(module) => {
                let mut obj = serializer.serialize_map(Some(1))?;
                obj.serialize_entry("success", &true)?;
                obj.serialize_entry("compiledModule", &JsCompiledModule(module))?;
                obj.end()
            }
            Err(error) => {
                let mut obj = serializer.serialize_map(Some(1))?;
                obj.serialize_entry("success", &false)?;
                obj.serialize_entry("error", &JsWrappedError(error))?;
                obj.end()
            }
        }
    }
}

#[wasm_bindgen(js_name = compile)]
pub fn parse_typecheck_and_compile(input: &str) -> JsValue {
    let result = compile(input.to_string());
    let compile_result = CompileResult(result);
    JsValue::from_serde(&compile_result)
        .unwrap_or(JsValue::NULL)
}

#[wasm_bindgen(js_name = run)]
pub fn run(input: &str) -> JsValue {
    let result = match compile_and_run(input.to_string()) {
        Ok(Some(value)) => JsValue::from_serde(&RunResult(value)),
        Ok(None) => Ok(JsValue::UNDEFINED),
        Err(error) => JsValue::from_serde(&JsWrappedError(&error))
    };
    result.unwrap_or(JsValue::from("Could not convert result to JSON"))
}
