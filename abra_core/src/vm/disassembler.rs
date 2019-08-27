use crate::vm::chunk::{CompiledModule, Chunk};
use crate::vm::compiler::MAIN_CHUNK_NAME;
use crate::vm::opcode::Opcode;
use std::collections::HashMap;

fn disassemble_chunk(module: &CompiledModule, name: String, chunk: &Chunk) -> Vec<String> {
    let mut labels: HashMap<usize, String> = HashMap::new();

    let mut slot_idx: i8 = -1;
    let mut code = chunk.code.iter();
    let mut disassembled = Vec::new();
    while let Some(byte) = code.next() {
        let slot_idx_orig = slot_idx;
        slot_idx += 1;
        let mut acc = Vec::new();

        let opcode = Opcode::from(byte);
        acc.push(opcode.to_string());

        let imm = if opcode.expects_imm() {
            slot_idx += 1;
            match code.next() {
                Some(imm) => {
                    acc.push(format!(" {}", imm));
                    Some(imm)
                }
                None => None
            }
        } else { None };

        match opcode {
            Opcode::Constant => {
                let imm = imm.expect("Constant requires an immediate");
                let constant = module.constants.get(*imm as usize)
                    .expect("The constant at the index should exist");
                acc.push(format!("  ; {}", constant))
            }
            Opcode::JumpIfF | Opcode::Jump => {
                let imm = imm.expect("Constant requires an immediate");
                let label = format!("label_{}", labels.len());
                labels.insert((slot_idx + 1 + (*imm as i8)) as usize, label.clone());
                acc.push(format!("  ; {}", label))
            }
            Opcode::JumpB => {
                let imm = imm.expect("Constant requires an immediate");
                let label = format!("label_{}", labels.len());
                labels.insert((slot_idx + 1 - (*imm as i8)) as usize, label.clone());
                acc.push(format!("  ; {}", label))
            }
            _ => {}
        }

        let line = acc.into_iter().collect::<String>();
        disassembled.push((line, slot_idx - slot_idx_orig));
    }

    let mut output = Vec::<String>::new();
    output.push(format!("\nchunk_{}:\n", name).to_string());

    let mut offset = 0;
    for (line, num_bytes) in disassembled.into_iter() {
        if let Some(label) = labels.get(&offset) {
            output.push(format!("{}\n", label));
        }
        output.push(format!("  {}\n", line));
        offset += num_bytes as usize;
    }

    output
}

pub fn disassemble(module: &CompiledModule) -> String {
    let mut output = Vec::<String>::new();
    for (name, chunk) in module.chunks.iter() {
        if name != MAIN_CHUNK_NAME {
            let mut disassembled = disassemble_chunk(module, name.to_string(), chunk);
            output.append(&mut disassembled);
        }
    }

    let main_chunk = module.chunks.get(MAIN_CHUNK_NAME).unwrap();
    let mut disassembled = disassemble_chunk(module, MAIN_CHUNK_NAME.to_string(), main_chunk);
    output.append(&mut disassembled);

    output.into_iter().collect()
}