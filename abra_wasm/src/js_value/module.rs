use crate::js_value::value::JsWrappedValue;
use serde::{Serialize, Serializer};
use abra_core::vm::compiler::Module;

pub struct JsModule<'a>(pub &'a Module);

impl<'a> Serialize for JsModule<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        let Module { name, constants, code } = self.0;

        let mut obj = serializer.serialize_map(Some(3))?;
        obj.serialize_entry("name", name)?;

        let mut bytecode = Vec::<(String, Option<Vec<u32>>)>::new();
        let mut code = code.iter();
        while let Some(op) = code.next() {
            let dis = op.dis();
            let mut parts = dis.split(" ");
            let op_repr = parts.next().expect("There should always be a name");
            let imms = parts
                .map(|p| p.parse::<u32>().expect("All immediate values should be numbers"))
                .collect::<Vec<_>>();
            let imms = if imms.is_empty() { None } else { Some(imms) };

            bytecode.push((op_repr.to_string(), imms));
        }
        obj.serialize_entry("code", &bytecode)?;

        let constants: Vec<JsWrappedValue> = constants.iter()
            .map(|v| JsWrappedValue(v))
            .collect();
        obj.serialize_entry("constants", &constants)?;

        obj.end()
    }
}
