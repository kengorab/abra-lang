use crate::vm::compiler::{Metadata, Module};
use crate::vm::opcode::Opcode;
use crate::vm::value::{Value, FnValue, TypeValue, EnumValue};
use std::collections::HashMap;

pub fn disassemble(modules: Vec<(Module, Metadata)>) -> String {
    let mut disassembler = Disassembler { modules, ..Disassembler::default() };
    disassembler.disassemble()
}

#[derive(Default)]
struct Disassembler {
    current_load: usize,
    current_uv_load: usize,
    current_uv_store: usize,
    current_store: usize,
    current_field_get: usize,
    current_local_mark: usize,
    modules: Vec<(Module, Metadata)>,
}

impl Disassembler {
    fn disassemble_bytecode(&mut self, name: String, code: &Vec<Opcode>, metadata: &Metadata, is_native: bool) -> Vec<String> {
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
                Opcode::Constant(mod_idx, imm) => {
                    let constant = self.modules[*mod_idx].0.constants.get(*imm)
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
                    let ident = metadata.loads.get(self.current_load)
                        .expect(&format!("There should be a load in the metadata at index {}", self.current_load));
                    self.current_load += 1;

                    if !ident.is_empty() {
                        acc.push(format!("\t; {}", ident))
                    }
                }
                Opcode::LStore(_) => {
                    let ident = metadata.stores.get(self.current_store)
                        .expect(&format!("There should be a store in the metadata at index {}", self.current_store));
                    self.current_store += 1;
                    if !ident.is_empty() {
                        acc.push(format!("\t; {}", ident))
                    }
                }
                Opcode::ULoad(_) => {
                    let ident = metadata.uv_loads.get(self.current_uv_load)
                        .expect(&format!("There should be an upvalue load in the metadata at index {}", self.current_uv_load));
                    self.current_uv_load += 1;

                    if !ident.is_empty() {
                        acc.push(format!("\t; {}", ident))
                    }
                }
                Opcode::UStore(_) => {
                    let ident = metadata.uv_stores.get(self.current_uv_load)
                        .expect(&format!("There should be an upvalue store in the metadata at index {}", self.current_uv_load));
                    self.current_uv_store += 1;

                    if !ident.is_empty() {
                        acc.push(format!("\t; {}", ident))
                    }
                }
                Opcode::GetField(_) => {
                    let ident = metadata.field_gets.get(self.current_field_get)
                        .expect(&format!("There should be a field_name in the metadata at index {}", self.current_field_get));
                    self.current_field_get += 1;
                    if !ident.is_empty() {
                        acc.push(format!("\t; {}", ident))
                    }
                }
                Opcode::MarkLocal(_) => {
                    let ident = metadata.local_marks.get(self.current_local_mark)
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

    pub fn disassemble_module(&mut self, module: &Module, metadata: &Metadata) -> String {
        let mut output = Vec::<String>::new();

        let header = format!("module {}", module.name);
        let mut disassembled = self.disassemble_bytecode(header, &module.code, &metadata, false);
        output.append(&mut disassembled);

        let iter = module.constants.iter().filter_map(|val| {
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
            let mut disassembled = self.disassemble_bytecode(name, &code, &metadata, is_native);
            output.append(&mut disassembled);
        }

        output.into_iter().collect()
    }

    pub fn disassemble(&mut self) -> String {
        let mut output = Vec::<String>::new();

        dbg!(&self.modules);
        for (module, metadata) in self.modules.clone() {
            if module.name == "prelude" {
                continue;
            }

            output.push(self.disassemble_module(&module, &metadata));
            output.push("\n".to_string());
        }

        output.into_iter().collect()
    }
}
