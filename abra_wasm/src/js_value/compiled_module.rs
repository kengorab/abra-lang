use abra_core::vm::chunk::CompiledModule;
use serde::{Serialize, Serializer};
use std::collections::HashMap;
use crate::js_value::chunk::JsChunk;
use crate::js_value::value::JsWrappedValue;

pub struct JsCompiledModule<'a>(pub &'a CompiledModule<'a>);

impl<'a> Serialize for JsCompiledModule<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        let CompiledModule { name, chunks, constants/*, bindings*/ } = self.0;

        let mut obj = serializer.serialize_map(Some(4))?;
        obj.serialize_entry("name", name)?;

        let chunks: HashMap<String, JsChunk> = chunks.iter()
            .map(|(k, v)| (k.clone(), JsChunk(v)))
            .collect();
        obj.serialize_entry("chunks", &chunks)?;

        let constants: Vec<JsWrappedValue> = constants.iter()
            .map(|v| JsWrappedValue(v))
            .collect();
        obj.serialize_entry("constants", &constants)?;

        obj.end()
    }
}
