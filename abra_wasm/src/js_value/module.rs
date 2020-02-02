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

        let mut bytecode = Vec::<(String, Option<Vec<u8>>)>::new();
        let mut code = code.iter();
        while let Some(byte) = code.next() {
            let op = Opcode::from(byte);
            let expected_imms = op.num_expected_imms();
            let imms = if expected_imms > 0 {
                let mut imms = vec![];
                for _ in 0..expected_imms {
                    let imm = code.next().map(|b| b.clone()).unwrap();
                    imms.push(imm)
                }
                Some(imms)
            } else { None };
            bytecode.push((op.to_string(), imms));
        }
        obj.serialize_entry("code", &bytecode)?;

        let constants: Vec<JsWrappedValue> = constants.iter()
            .map(|v| JsWrappedValue(v))
            .collect();
        obj.serialize_entry("constants", &constants)?;

        obj.end()
    }
}
