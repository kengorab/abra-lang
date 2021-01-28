use crate::vm::compiler::{Metadata, Module};
use crate::vm::opcode::Opcode;
use crate::vm::value::{Value, FnValue, TypeValue, EnumValue};
use std::collections::HashMap;

pub fn disassemble(module: Module, metadata: Metadata) -> String {
    let mut disassembler = Disassembler {
        current_load: 0,
        current_uv_load: 0,
        current_uv_store: 0,
        current_store: 0,
        current_field_get: 0,
        current_local_mark: 0,
        module,
        metadata,
    };
    disassembler.disassemble()
}

struct Disassembler {
    current_load: usize,
    current_uv_load: usize,
    current_uv_store: usize,
    current_store: usize,
    current_field_get: usize,
    current_local_mark: usize,
    module: Module,
    metadata: Metadata,
}

impl Disassembler {
    fn disassemble_bytecode(&mut self, name: String, code: Vec<Opcode>, is_native: bool) -> Vec<String> {
        let mut output = Vec::<String>::new();
        output.push(format!("\n{}:\n", name).to_string());

        if is_native {
            output.push("  <native>".to_string());
            return output;
        }

        let mut labels: HashMap<usize, String> = HashMap::new();

        let mut slot_idx: i16 = -1;
        let mut code = code.iter();
        let mut disassembled = Vec::new();
        while let Some(opcode) = code.next() {
            let slot_idx_orig = slot_idx;
            slot_idx += 1;
            let mut acc = Vec::new();

            acc.push(opcode.dis());

            match opcode {
                Opcode::Constant(imm) => {
                    let constant = self.module.constants.get(*imm)
                        .expect(format!("The constant at index {} should exist", imm).as_str());
                    acc.push(format!("\t; {}", constant))
                }
                Opcode::JumpIfF(offset) | Opcode::Jump(offset) => {
                    let label = format!("label_{}", labels.len());
                    labels.insert((slot_idx + 1 + (*offset as i16)) as usize, label.clone());
                    acc.push(format!("\t; {}", label))
                }
                Opcode::JumpB(offset) => {
                    let label = format!("label_{}", labels.len());
                    labels.insert((slot_idx + 1 - (*offset as i16)) as usize, label.clone());
                    acc.push(format!("\t; {}", label))
                }
                Opcode::LLoad(_) => {
                    let ident = self.metadata.loads.get(self.current_load)
                        .expect(&format!("There should be a load in the metadata at index {}", self.current_load));
                    self.current_load += 1;

                    if !ident.is_empty() {
                        acc.push(format!("\t; {}", ident))
                    }
                }
                Opcode::LStore(_) => {
                    let ident = self.metadata.stores.get(self.current_store)
                        .expect(&format!("There should be a store in the metadata at index {}", self.current_store));
                    self.current_store += 1;
                    if !ident.is_empty() {
                        acc.push(format!("\t; {}", ident))
                    }
                }
                Opcode::ULoad(_) => {
                    let ident = self.metadata.uv_loads.get(self.current_uv_load)
                        .expect(&format!("There should be an upvalue load in the metadata at index {}", self.current_uv_load));
                    self.current_uv_load += 1;

                    if !ident.is_empty() {
                        acc.push(format!("\t; {}", ident))
                    }
                }
                Opcode::UStore(_) => {
                    let ident = self.metadata.uv_stores.get(self.current_uv_load)
                        .expect(&format!("There should be an upvalue store in the metadata at index {}", self.current_uv_load));
                    self.current_uv_store += 1;

                    if !ident.is_empty() {
                        acc.push(format!("\t; {}", ident))
                    }
                }
                Opcode::GetField(_) => {
                    let ident = self.metadata.field_gets.get(self.current_field_get)
                        .expect(&format!("There should be a field_name in the metadata at index {}", self.current_field_get));
                    self.current_field_get += 1;
                    if !ident.is_empty() {
                        acc.push(format!("\t; {}", ident))
                    }
                }
                Opcode::MarkLocal(_) => {
                    let ident = self.metadata.local_marks.get(self.current_local_mark)
                        .expect(&format!("There should be a local_mark in the metadata at index {}", self.current_local_mark));
                    self.current_local_mark += 1;
                    if !ident.is_empty() {
                        acc.push(format!("\t; {}", ident))
                    }
                }
                _ => {}
            }

            let line = acc.into_iter().collect::<String>();
            disassembled.push((line, slot_idx - slot_idx_orig));
        }

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

        let mut disassembled = self.disassemble_bytecode(main_name, self.module.code.clone(), false);
        output.append(&mut disassembled);

        let constants = self.module.constants.clone();
        let iter = constants.iter().filter_map(|val| {
            match val {
                Value::Fn(FnValue { name, code, .. }) => {
                    let name = format!("fn {}", name.clone());
                    let values = (name, code.clone(), false);
                    Some(vec![values])
                }
                Value::Type(TypeValue { name, methods, static_fields, .. }) |
                Value::Enum(EnumValue { name, methods, static_fields, .. }) => {
                    let mut values = vec![];

                    for (fn_name, value) in methods {
                        let method_name = format!("fn {}#{}", name, fn_name);
                        match value {
                            Value::Fn(FnValue { code, .. }) => values.push((method_name, code.clone(), false)),
                            Value::NativeFn(_) => values.push((method_name, vec![], true)),
                            _ => unreachable!()
                        }
                    }
                    for (fn_name, value) in static_fields {
                        let static_method_name = format!("fn {}::{}", name, fn_name);
                        match value {
                            Value::Fn(FnValue { code, .. }) => values.push((static_method_name, code.clone(), false)),
                            Value::NativeFn(_) => values.push((static_method_name, vec![], true)),
                            _ => unreachable!()
                        }
                    }

                    Some(values)
                }
                _ => None,
            }
        }).flatten();
        for (name, code, is_native) in iter {
            let mut disassembled = self.disassemble_bytecode(name, code, is_native);
            output.append(&mut disassembled);
        }

        output.into_iter().collect()
    }
}
