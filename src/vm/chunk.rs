use std::fmt::{Debug, Formatter, Error};
use std::collections::HashMap;
use crate::vm::opcode::Opcode;
use crate::vm::value::Value;

#[derive(PartialEq)]
pub struct Chunk {
    pub(crate) lines: Vec<usize>,
    pub(crate) code: Vec<u8>,
    pub(crate) constants: Vec<Value>,
    pub(crate) bindings: HashMap<String, usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk { lines: Vec::new(), code: Vec::new(), constants: Vec::new(), bindings: HashMap::new() }
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

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        (self.constants.len() - 1) as u8
    }
}

impl Debug for Chunk {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "Chunk(lines: {:?}, code: [", self.lines)?;

        let mut bytecode = self.code.iter().peekable();
        loop {
            match bytecode.next() {
                Some(&byte) => {
                    write!(f, "{:?}", Opcode::from(byte))?;
                    if byte == Opcode::Constant as u8 {
                        match bytecode.next() {
                            None => panic!("Byte expected after Constant opcode!"),
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

        write!(f, "], constants: {:?}, bindings: {:?})", self.constants, self.bindings)
    }
}
