use crate::vm::chunk::Chunk;
use crate::vm::opcode::Opcode;
use crate::vm::value::{Value, Obj};

#[derive(Debug)]
pub enum InterpretError {
    StackEmpty,
    ConstIdxOutOfBounds,
    EndOfBytes,
}

pub struct VM<'a> {
    ip: usize,
    chunk: &'a Chunk,
    stack: Vec<Value>,
}

impl<'a> VM<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        VM { ip: 0, chunk, stack: Vec::new() }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    fn pop_expect(&mut self) -> Result<Value, InterpretError> {
        self.stack.pop().ok_or(InterpretError::StackEmpty)
    }

    fn read_byte(&mut self) -> Option<u8> {
        if self.chunk.code.len() == self.ip {
            None
        } else {
            let instr: u8 = self.chunk.code[self.ip];
            self.ip += 1;
            Some(instr)
        }
    }

    fn read_instr(&mut self) -> Option<Opcode> {
        self.read_byte().map(|b| Opcode::from(b))
    }

    fn int_op<F>(&mut self, f: F) -> Result<(), InterpretError>
        where F: FnOnce(i64, i64) -> i64
    {
        let b = self.pop_expect()?;
        let a = self.pop_expect()?;

        match (a, b) {
            (Value::Int(a), Value::Int(b)) => {
                self.push(Value::Int(f(a, b)))
            }
            _ => unreachable!()
        };
        Ok(())
    }

    fn float_op<F>(&mut self, f: F) -> Result<(), InterpretError>
        where F: FnOnce(f64, f64) -> f64
    {
        let b = self.pop_expect()?;
        let a = self.pop_expect()?;

        match (a, b) {
            (Value::Float(a), Value::Float(b)) => {
                self.push(Value::Float(f(a, b)))
            }
            _ => unreachable!()
        };
        Ok(())
    }

    pub fn run(&mut self) -> Result<Option<Value>, InterpretError> {
        loop {
            let instr = self.read_instr()
                .ok_or(InterpretError::EndOfBytes)?;

            match instr {
                Opcode::Constant => {
                    let const_idx = self.read_byte().ok_or(InterpretError::EndOfBytes)? as usize;
                    let val = self.chunk.constants.get(const_idx)
                        .ok_or(InterpretError::ConstIdxOutOfBounds)?
                        .clone();
                    self.push(val)
                }
                Opcode::IAdd => self.int_op(|a, b| a + b)?,
                Opcode::ISub => self.int_op(|a, b| a - b)?,
                Opcode::IMul => self.int_op(|a, b| a * b)?,
                Opcode::IDiv => self.int_op(|a, b| a / b)?,
                Opcode::FAdd => self.float_op(|a, b| a + b)?,
                Opcode::FSub => self.float_op(|a, b| a - b)?,
                Opcode::FMul => self.float_op(|a, b| a * b)?,
                Opcode::FDiv => self.float_op(|a, b| a / b)?,
                Opcode::I2F => {
                    let val = self.pop_expect()?;
                    let val = match val {
                        Value::Int(v) => Value::Float(v as f64),
                        _ => unreachable!()
                    };
                    self.push(val)
                }
                Opcode::F2I => {
                    let val = self.pop_expect()?;
                    let val = match val {
                        Value::Float(v) => Value::Int(v as i64),
                        _ => unreachable!()
                    };
                    self.push(val)
                }
                Opcode::StrConcat => {
                    let b = self.pop_expect()?;
                    let a = self.pop_expect()?;

                    let a = a.to_string();
                    let b = b.to_string();
                    let concat = a + &b;
                    self.push(Value::Obj(Obj::StringObj { value: Box::new(concat) }))
                }
                Opcode::T => self.push(Value::Bool(true)),
                Opcode::F => self.push(Value::Bool(false)),
                Opcode::And | Opcode::Or => {
                    if let Value::Bool(b) = self.pop_expect()? {
                        if let Value::Bool(a) = self.pop_expect()? {
                            let res = if let Opcode::And = instr {
                                a && b
                            } else {
                                a || b
                            };
                            self.push(Value::Bool(res));
                        }
                    }
                }
                Opcode::Negate => {
                    let val = self.pop_expect()?;
                    let val = match val {
                        Value::Int(v) => Value::Int(-v),
                        Value::Float(v) => Value::Float(-v),
                        _ => unreachable!()
                    };
                    self.push(val)
                }
                Opcode::Return => break Ok(self.pop()),
            }
        }
    }
}