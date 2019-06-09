extern crate serde;
extern crate serde_derive;
extern crate serde_json;
extern crate wasm_bindgen;

use serde::ser::{Serializer, SerializeSeq};
use serde::Serialize;
use wasm_bindgen::prelude::*;

use abra_core::compile_and_run;
use abra_core::vm::value::{Obj, Value};

pub struct Res(Value);

impl Serialize for Res {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match self {
            Res(Value::Nil) => serializer.serialize_none(),
            Res(Value::Int(val)) => serializer.serialize_i64(*val),
            Res(Value::Float(val)) => serializer.serialize_f64(*val),
            Res(Value::Bool(val)) => serializer.serialize_bool(*val),
            Res(Value::Obj(obj)) => match obj {
                Obj::StringObj { value } => serializer.serialize_str(&*value),
                Obj::ArrayObj { value } => {
                    let mut arr = serializer.serialize_seq(Some((*value).len()))?;
                    value.into_iter().for_each(|val| {
                        arr.serialize_element(&Res((**val).clone())).unwrap();
                    });
                    arr.end()
                }
                Obj::OptionObj { value } => match value {
                    None => serializer.serialize_none(),
                    Some(value) => serializer.serialize_some(&Res(*value.clone()))
                }
            }
            Res(Value::Fn(fn_name)) => serializer.serialize_str(fn_name)
        }
    }
}

#[wasm_bindgen(js_name = run)]
pub fn run(input: &str) -> JsValue {
    compile_and_run(input.to_string())
        .map(|value| JsValue::from_serde(&Res(value)).unwrap_or(JsValue::NULL))
        .unwrap_or(JsValue::UNDEFINED)
}
