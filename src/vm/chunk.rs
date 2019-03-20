use std::fmt::{Debug, Formatter, Error};
use crate::vm::opcode::Opcode;

#[derive(Debug, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
}

pub struct Chunk {
    lines: Vec<usize>,
    code: Vec<u8>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk { lines: Vec::new(), code: Vec::new(), constants: Vec::new() }
    }

    fn add_line(&mut self, line_num: usize) {
        if !self.lines.is_empty() && self.lines.len() == line_num - 1 {
            self.lines[line_num - 1] += 1;
        } else {
            self.lines.push(1);
        }
    }

    pub fn write(&mut self, byte: u8, line_num: usize) {
        self.add_line(line_num);
        self.code.push(byte);
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        (self.constants.len() - 1) as u8
    }
}

impl Debug for Chunk {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "Chunk(code: [");

        let mut bytecode = self.code.iter().peekable();
        loop {
            match bytecode.next() {
                Some(&byte) => {
                    write!(f, "{:?}", Opcode::from(byte));
                    if byte == Opcode::Constant.into() {
                        match bytecode.next() {
                            None => panic!("Byte expected after Constant opcode!"),
                            Some(&byte) => write!(f, ", {:?}", byte)
                        }
                    } else { continue; }
                }
                None => break
            };
            write!(f, ", ");
        }

        write!(f, "], constants: {:?})", self.constants)
    }
}
