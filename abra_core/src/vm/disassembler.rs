use crate::vm::compiler::{Metadata, Module};
use crate::vm::opcode::Opcode;
use crate::vm::value::Value;
use std::collections::HashMap;

pub fn disassemble(module: Module, metadata: Metadata) -> String {
    let mut disassembler = Disassembler {
        current_load: 0,
        current_store: 0,
        module,
        metadata,
    };
    disassembler.disassemble()
}

struct Disassembler {
    current_load: usize,
    current_store: usize,
    module: Module,
    metadata: Metadata,
}

impl Disassembler {
    fn disassemble_bytecode(&mut self, name: String, code: Vec<u8>) -> Vec<String> {
        let mut labels: HashMap<usize, String> = HashMap::new();

        let mut slot_idx: i8 = -1;
        let mut code = code.iter();
        let mut disassembled = Vec::new();
        while let Some(byte) = code.next() {
            let slot_idx_orig = slot_idx;
            slot_idx += 1;
            let mut acc = Vec::new();

            let opcode = Opcode::from(byte);
            acc.push(opcode.to_string());

            let imm = if opcode.expects_imm() {
                slot_idx += 1;
                code.next().map(|imm| {
                    acc.push(format!(" {}", imm));
                    imm
                })
            } else { None };

            match opcode {
                Opcode::Constant => {
                    let imm = imm.expect("Constant requires an immediate");
                    let constant = self.module.constants.get(*imm as usize)
                        .expect("The constant at the index should exist");
                    acc.push(format!("\t; {}", constant))
                }
                Opcode::JumpIfF | Opcode::Jump => {
                    let imm = imm.expect("JumpIfF/Jump requires an immediate");
                    let label = format!("label_{}", labels.len());
                    labels.insert((slot_idx + 1 + (*imm as i8)) as usize, label.clone());
                    acc.push(format!("\t; {}", label))
                }
                Opcode::JumpB => {
                    let imm = imm.expect("JumpB requires an immediate");
                    let label = format!("label_{}", labels.len());
                    labels.insert((slot_idx + 1 - (*imm as i8)) as usize, label.clone());
                    acc.push(format!("\t; {}", label))
                }
                Opcode::LLoad | Opcode::LLoad0 | Opcode::LLoad1 | Opcode::LLoad2 | Opcode::LLoad3 | Opcode::LLoad4 => {
                    let ident = self.metadata.loads.get(self.current_load)
                        .expect(&format!("There should be a load in the metadata at index {}", self.current_load));
                    self.current_load += 1;

                    if !ident.is_empty() {
                        acc.push(format!("\t; {}", ident))
                    }
                }
                Opcode::LStore | Opcode::LStore0 | Opcode::LStore1 | Opcode::LStore2 | Opcode::LStore3 | Opcode::LStore4 => {
                    let ident = self.metadata.stores.get(self.current_store)
                        .expect(&format!("There should be a store in the metadata at index {}", self.current_store));
                    self.current_store += 1;
                    if !ident.is_empty() {
                        acc.push(format!("\t; {}", ident))
                    }
                }
                _ => {}
            }

            let line = acc.into_iter().collect::<String>();
            disassembled.push((line, slot_idx - slot_idx_orig));
        }

        let mut output = Vec::<String>::new();
        output.push(format!("\n{}:\n", name).to_string());

        let mut offset = 0;
        for (line, num_bytes) in disassembled.into_iter() {
            if let Some(label) = labels.get(&offset) {
                output.push(format!("{}:\n", label));
            }
            output.push(format!("  {}\n", line));
            offset += num_bytes as usize;
        }

        output
    }

    pub fn disassemble(&mut self) -> String {
        let mut output = Vec::<String>::new();

        let main_name = "entrypoint $main".to_string();

        let mut disassembled = self.disassemble_bytecode(main_name, self.module.code.clone());
        output.append(&mut disassembled);

        let constants = self.module.constants.clone();
        let iter = constants.iter().filter_map(|val| {
            match val {
                Value::Fn { name, code } => Some((format!("fn {}", name.clone()), code.clone())),
                _ => None,
            }
        });
        for (name, code) in iter {
            let mut disassembled = self.disassemble_bytecode(name, code);
            output.append(&mut disassembled);
        }

        output.into_iter().collect()
    }
}
