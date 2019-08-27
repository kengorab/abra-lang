use std::fmt::{Debug, Formatter, Error};
use std::collections::HashMap;
use crate::vm::opcode::Opcode;
use crate::vm::value::Value;

#[derive(PartialEq)]
pub struct Chunk {
    pub lines: Vec<usize>,
    pub code: Vec<u8>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk { lines: Vec::new(), code: Vec::new() }
    }

    fn add_line(&mut self, line_num: usize) {
        if !self.lines.is_empty() && self.lines.len() == line_num {
            self.lines[line_num - 1] += 1;
        } else {
            // Pad any intermediate empty lines with 0's
            if line_num > self.lines.len() + 1 {
                let len = self.lines.len();
                for _ in (len + 1)..line_num {
                    self.lines.push(0);
                }
            }

            self.lines.push(1);
        }
    }

    pub fn write(&mut self, byte: u8, line_num: usize) {
        self.add_line(line_num);
        self.code.push(byte);
    }
}

impl Debug for Chunk {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "Chunk(lines: {:?}, code: [", self.lines)?;

        let mut bytecode = self.code.iter().peekable();
        loop {
            match bytecode.next() {
                Some(&byte) => {
                    let opcode = Opcode::from(&byte);
                    write!(f, "{:?}", opcode)?;
                    if opcode.expects_imm() {
                        match bytecode.next() {
                            None => panic!("Byte expected after opcode"),
                            Some(&byte) => write!(f, ", {:?}", byte)?
                        };
                    }
                }
                None => break
            };

            if let Some(_) = bytecode.peek() {
                write!(f, ", ")?;
            }
        }

        write!(f, "])")
    }
}

#[derive(Debug, PartialEq)]
pub struct CompiledModule<'a> {
    pub name: &'a str,
    pub chunks: HashMap<String, Chunk>,
    pub constants: Vec<Value>,
}

impl<'a> CompiledModule<'a> {
    pub fn new(name: &'a str) -> Self {
        CompiledModule { name, chunks: HashMap::new(), constants: Vec::new() /*bindings: Vec::new()*/ }
    }

    pub fn get_chunk(&mut self, name: String) -> Option<&mut Chunk> {
        self.chunks.get_mut(&name)
    }

    pub fn add_chunk(&mut self, name: String, chunk: Chunk) {
        self.chunks.insert(name, chunk);
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.get_constant_index(&value)
            .unwrap_or_else(|| {
                self.constants.push(value);
                (self.constants.len() - 1) as u8
            })
    }

    pub fn get_constant_index(&self, value: &Value) -> Option<u8> {
        self.constants.iter()
            .position(|v| v == value)
            .map(|v| v as u8)
    }
}
