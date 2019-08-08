use abra_core::vm::chunk::Chunk;
use serde::{Serialize, Serializer};
use abra_core::vm::opcode::Opcode;

pub struct JsChunk<'a>(pub &'a Chunk);

impl<'a> Serialize for JsChunk<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        let Chunk { code, .. } = self.0;

        let mut obj = serializer.serialize_map(Some(2))?;

        let mut bytecode = Vec::<(String, Option<u8>)>::new();
        let mut code = code.iter();
        while let Some(byte) = code.next() {
            let op = Opcode::from(*byte);
            let imm = if op.expects_imm() {
                code.next().map(|b| b.clone())
            } else { None };
            bytecode.push((op.to_string(), imm));
        }

        obj.serialize_entry("code", &bytecode)?;
        obj.end()
    }
}