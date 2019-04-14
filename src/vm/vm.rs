use crate::vm::chunk::Chunk;
use crate::vm::opcode::Opcode;
use crate::vm::value::{Value, Obj};
use std::cmp::Ordering;
use std::collections::vec_deque::VecDeque;

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
    vars: Vec<Value>,
}

impl<'a> VM<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        VM { ip: 0, chunk, stack: Vec::new(), vars: Vec::with_capacity(chunk.bindings.len()) }
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

    fn comp_values(&mut self, opcode: Opcode) -> Result<(), InterpretError> {
        let b = self.pop_expect()?;
        let a = self.pop_expect()?;

        // Rust can't natively compare floats and ints, so we provide that logic here via
        // partial_cmp, deferring to normal implementation of PartialOrd for non-float comparison.
        let ord = match (a, b) {
            (Value::Int(a), Value::Float(b)) => (a as f64).partial_cmp(&b),
            (Value::Float(a), Value::Float(b)) => a.partial_cmp(&b),
            (Value::Float(a), Value::Int(b)) => a.partial_cmp(&(b as f64)),
            (a @ _, b @ _) => a.partial_cmp(&b),
        };

        // If the partial_cmp call above returns None, treat that as an equivalence value of false
        let res = match ord {
            None => false,
            Some(ord) =>
                match opcode {
                    Opcode::LT => ord == Ordering::Less,
                    Opcode::LTE => ord != Ordering::Greater,
                    Opcode::GT => ord == Ordering::Greater,
                    Opcode::GTE => ord != Ordering::Less,
                    Opcode::Eq => ord == Ordering::Equal,
                    Opcode::Neq => ord != Ordering::Equal,
                    _ => unreachable!()
                }
        };
        self.push(Value::Bool(res));

        Ok(())
    }

    fn store(&mut self, var_idx: usize) -> Result<(), InterpretError> {
        let val = self.pop_expect()?;
        Ok(self.vars.insert(var_idx, val))
    }

    fn load(&mut self, var_idx: usize) -> Result<(), InterpretError> {
        if let Some(val) = self.vars.get(var_idx) {
            self.push(val.clone());
        } else {
            unreachable!()
        }
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
                Opcode::IConst0 => self.push(Value::Int(0)),
                Opcode::IConst1 => self.push(Value::Int(1)),
                Opcode::IConst2 => self.push(Value::Int(2)),
                Opcode::IConst3 => self.push(Value::Int(3)),
                Opcode::IConst4 => self.push(Value::Int(4)),
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
                Opcode::Invert => {
                    let val = self.pop_expect()?;
                    let val = match val {
                        Value::Int(v) => Value::Int(-v),
                        Value::Float(v) => Value::Float(-v),
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
                        } else {
                            unreachable!()
                        }
                    } else {
                        unreachable!()
                    }
                }
                Opcode::Negate => {
                    if let Value::Bool(val) = self.pop_expect()? {
                        self.push(Value::Bool(!val));
                    } else {
                        unreachable!()
                    }
                }
                Opcode::LT => self.comp_values(Opcode::LT)?,
                Opcode::LTE => self.comp_values(Opcode::LTE)?,
                Opcode::GT => self.comp_values(Opcode::GT)?,
                Opcode::GTE => self.comp_values(Opcode::GTE)?,
                Opcode::Neq => self.comp_values(Opcode::Neq)?,
                Opcode::Eq => self.comp_values(Opcode::Eq)?,
                Opcode::MkArr => {
                    if let Value::Int(mut size) = self.pop_expect()? {
                        // Array items are on the stack in reverse order, pop them off in reverse
                        let mut arr_items = VecDeque::<Box<Value>>::with_capacity(size as usize);
                        while size > 0 {
                            size -= 1;
                            arr_items.push_front(Box::new(self.pop_expect()?));
                        }
                        self.push(Value::Obj(Obj::ArrayObj { value: arr_items.into() }));
                    } else {
                        unreachable!()
                    }
                }
                Opcode::Store0 => self.store(0)?,
                Opcode::Store1 => self.store(1)?,
                Opcode::Store2 => self.store(2)?,
                Opcode::Store3 => self.store(3)?,
                Opcode::Store4 => self.store(4)?,
                Opcode::Store => {
                    if let Value::Int(var_idx) = self.pop_expect()? {
                        self.store(var_idx as usize)?;
                    } else {
                        unreachable!()
                    }
                }
                Opcode::Load0 => self.load(0)?,
                Opcode::Load1 => self.load(1)?,
                Opcode::Load2 => self.load(2)?,
                Opcode::Load3 => self.load(3)?,
                Opcode::Load4 => self.load(4)?,
                Opcode::Load => {
                    if let Value::Int(var_idx) = self.pop_expect()? {
                        self.load(var_idx as usize)?;
                    } else {
                        unreachable!()
                    }
                }
                Opcode::Return => break Ok(self.pop()),
            }
        }
    }
}