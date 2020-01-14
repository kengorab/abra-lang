use crate::js_value::value::JsWrappedValue;
use serde::{Serialize, Serializer};
use abra_core::vm::compiler::Module;
use abra_core::vm::opcode::Opcode;

pub struct JsModule<'a>(pub &'a Module);

impl<'a> Serialize for JsModule<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        let Module { constants, code } = self.0;

        let mut obj = serializer.serialize_map(Some(4))?;

        let mut bytecode = Vec::<(String, Option<u8>)>::new();
        let mut code = code.iter();
        while let Some(byte) = code.next() {
            let op = Opcode::from(byte);
            let imm = if op.expects_imm() {
                code.next().map(|b| b.clone())
            } else { None };
            bytecode.push((op.to_string(), imm));
        }
        obj.serialize_entry("code", &bytecode)?;

        let constants: Vec<JsWrappedValue> = constants.iter()
            .map(|v| JsWrappedValue(v))
            .collect();
        obj.serialize_entry("constants", &constants)?;

        obj.end()
    }
}
