use crate::builtins::native_fns::{NATIVE_FNS_MAP, NativeFn};
use crate::vm::opcode::Opcode;
use crate::vm::value::{Value, Obj};
use crate::vm::compiler::Module;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::vec_deque::VecDeque;
use crate::builtins::native_types::{NativeString, NativeType, NativeArray};

// Helper macros
macro_rules! pop_expect_string {
    ($self: expr) => (
        match $self.pop_expect()? {
            Value::Obj(Obj::StringObj { value }) => Ok(*value),
            v @ _ => Err(InterpretError::TypeError("String".to_string(), v.to_string()))
        }
    );
}

macro_rules! pop_expect_int {
    ($self: expr) => (
        match $self.pop_expect()? {
            Value::Int(value) => Ok(value),
            v @ _ => Err(InterpretError::TypeError("Int".to_string(), v.to_string()))
        }
    );
}

macro_rules! pop_expect_float {
    ($self: expr) => (
        match $self.pop_expect()? {
            Value::Float(value) => Ok(value),
            v @ _ => Err(InterpretError::TypeError("Float".to_string(), v.to_string()))
        }
    );
}

macro_rules! pop_expect_bool {
    ($self: expr) => (
        match $self.pop_expect()? {
            Value::Bool(value) => Ok(value),
            v @ _ => Err(InterpretError::TypeError("Bool".to_string(), v.to_string()))
        }
    );
}

// Helper to get the current frame, without having to worry about lifetimes
macro_rules! current_frame {
    ($self: expr) => { $self.call_stack.last_mut().expect("There needs to be at least 1 active call stack member") };
}

#[derive(Debug)]
pub enum InterpretError {
    StackEmpty,
    ConstIdxOutOfBounds,
    EndOfBytes,
    TypeError(/* expected: */ String, /* actual:*/ String),
    StackOverflow,
}

struct CallFrame {
    ip: usize,
    code: Vec<u8>,
    stack_offset: usize,
    name: String,
}

#[derive(Clone)]
pub struct VMContext {
    pub print: fn(&str) -> ()
}

impl VMContext {
    pub fn default() -> Self {
        VMContext {
            print: VMContext::print
        }
    }

    pub fn print(input: &str) {
        print!("{}\n", input)
    }
}

pub struct VM {
    ctx: VMContext,
    constants: Vec<Value>,
    call_stack: Vec<CallFrame>,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
}

const STACK_LIMIT: usize = 64;

impl VM {
    pub fn new(module: Module, ctx: VMContext) -> Self {
        let name = "$main".to_string();
        let Module { code, constants } = module;
        let root_frame = CallFrame { ip: 0, code, stack_offset: 0, name };
        VM {
            ctx,
            constants,
            call_stack: vec![root_frame],
            stack: Vec::new(),
            globals: HashMap::new(),
        }
    }

    fn stack_insert_at(&mut self, index: usize, value: Value) {
        match self.stack.get_mut(index) {
            Some(slot) => *slot = value,
            None => {
                let frame = self.call_stack.last().unwrap();
                let chunk_name = &frame.name;
                let offset = frame.ip;
                panic!("Runtime error [{}+{}]:\n  No stack slot available at index {}", chunk_name, offset, index)
            }
        }
    }

    fn stack_get(&mut self, index: usize) -> Value {
        self.stack.get(index)
            .map(|value| value.clone())
            .unwrap_or(Value::Nil)
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

    fn pop_expect_n(&mut self, num: usize) -> Result<(), InterpretError> {
        if self.stack.len() < num {
            Err(InterpretError::StackEmpty)
        } else {
            let new_size = self.stack.len() - num;
            self.stack.truncate(new_size);
            Ok(())
        }
    }

    fn read_byte(&mut self) -> Option<u8> {
        let frame: &mut CallFrame = current_frame!(self);
        if frame.code.len() == frame.ip {
            None
        } else {
            let instr = frame.code[frame.ip];
            frame.ip += 1;
            Some(instr)
        }
    }

    fn read_byte_expect(&mut self) -> Result<usize, InterpretError> {
        self.read_byte()
            .map(|b| b as usize)
            .ok_or(InterpretError::EndOfBytes)
    }

    fn read_instr(&mut self) -> Option<Opcode> {
        self.read_byte().map(|b| Opcode::from(&b))
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

    fn store(&mut self, stack_slot: usize) -> Result<(), InterpretError> {
        let CallFrame { stack_offset, .. } = current_frame!(self);
        let stack_slot = stack_slot + *stack_offset;
        let value = self.pop_expect()?;
        self.stack_insert_at(stack_slot, value); // TODO: Raise InterpretError when OOB stack_slot
        Ok(())
    }

    fn load(&mut self, stack_slot: usize) -> Result<(), InterpretError> {
        let CallFrame { stack_offset, .. } = current_frame!(self);
        let stack_slot = stack_slot + *stack_offset;
        let value = self.stack_get(stack_slot);
        Ok(self.push(value))
    }

    pub fn run(&mut self) -> Result<Option<Value>, InterpretError> {
        loop {
            let instr = self.read_instr()
                .ok_or(InterpretError::EndOfBytes)?;

            match instr {
                Opcode::Constant => {
                    let const_idx = self.read_byte_expect()?;
                    let val = self.constants.get(const_idx)
                        .ok_or(InterpretError::ConstIdxOutOfBounds)?
                        .clone();
                    self.push(val)
                }
                Opcode::Nil => self.push(Value::Nil),
                Opcode::IConst0 => self.push(Value::Int(0)),
                Opcode::IConst1 => self.push(Value::Int(1)),
                Opcode::IConst2 => self.push(Value::Int(2)),
                Opcode::IConst3 => self.push(Value::Int(3)),
                Opcode::IConst4 => self.push(Value::Int(4)),
                Opcode::IAdd => self.int_op(|a, b| a + b)?,
                Opcode::ISub => self.int_op(|a, b| a - b)?,
                Opcode::IMul => self.int_op(|a, b| a * b)?,
                Opcode::IDiv => self.int_op(|a, b| a / b)?,
                Opcode::IMod => self.int_op(|a, b| a % b)?,
                Opcode::FAdd => self.float_op(|a, b| a + b)?,
                Opcode::FSub => self.float_op(|a, b| a - b)?,
                Opcode::FMul => self.float_op(|a, b| a * b)?,
                Opcode::FDiv => self.float_op(|a, b| a / b)?,
                Opcode::FMod => self.float_op(|a, b| a % b)?,
                Opcode::I2F => {
                    let val = pop_expect_int!(self)?;
                    self.push(Value::Float(val as f64))
                }
                Opcode::F2I => {
                    let val = pop_expect_float!(self)?;
                    self.push(Value::Int(val as i64))
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
                    // TODO: Short-circuiting
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
                    let val = pop_expect_bool!(self)?;
                    self.push(Value::Bool(!val));
                }
                Opcode::Coalesce => { // TODO: Rewrite this using jumps when they're implemented!
                    let fallback = self.pop_expect()?;

                    if let Value::Obj(Obj::OptionObj { value }) = self.pop_expect()? {
                        match value {
                            Some(value) => self.push(*value),
                            None => self.push(fallback)
                        }
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
                Opcode::OptMk => {
                    let value = match self.pop_expect()? {
                        Value::Nil => None,
                        v @ _ => Some(Box::new(v))
                    };
                    self.push(Value::Obj(Obj::OptionObj { value }))
                }
                Opcode::MapMk => {
                    let size = self.read_byte_expect()?;
                    let mut items = HashMap::with_capacity(size);
                    for _ in 0..size {
                        let value = self.pop_expect()?;
                        let key = pop_expect_string!(self)?;
                        items.insert(key, value);
                    }
                    self.push(Value::Obj(Obj::MapObj { value: items }));
                }
                Opcode::MapLoad => {
                    let key = pop_expect_string!(self)?;
                    let val = match self.pop_expect()? {
                        Value::Obj(Obj::MapObj { value }) => {
                            match value.get(&key) {
                                Some(val) => val.clone(),
                                None => Value::Nil
                            }
                        }
                        value @ Value::Obj(Obj::StringObj { .. }) => NativeString::get_field_value(&value, key),
                        value @ Value::Obj(Obj::ArrayObj { .. }) => NativeArray::get_field_value(&value, key),
                        _ => unreachable!()
                    };
                    self.push(val)
                }
                Opcode::ArrMk => {
                    let size = self.read_byte_expect()?;
                    let mut arr_items = VecDeque::<Box<Value>>::with_capacity(size as usize);
                    for _ in 0..size {
                        arr_items.push_front(Box::new(self.pop_expect()?));
                    }
                    self.push(Value::Obj(Obj::ArrayObj { value: arr_items.into() }));
                }
                Opcode::ArrLoad => {
                    let idx = pop_expect_int!(self)?;
                    let value = match self.pop_expect()? {
                        Value::Obj(Obj::StringObj { value }) => {
                            let len = value.len() as i64;
                            let idx = if idx < 0 { idx + len } else { idx };

                            match (*value).chars().nth(idx as usize) {
                                Some(ch) => Value::Obj(Obj::StringObj { value: Box::new(ch.to_string()) }),
                                None => Value::Nil
                            }
                        }
                        Value::Obj(Obj::ArrayObj { value }) => {
                            let len = value.len() as i64;
                            if idx < -len || idx >= len {
                                Value::Nil//None
                            } else {
                                let idx = if idx < 0 { idx + len } else { idx };
                                *value[idx as usize].clone()
                            }
                        }
                        _ => unreachable!()
                    };
                    self.push(value);
                }
                Opcode::ArrSlc => {
                    #[inline]
                    fn get_range_endpoints(len: usize, start: i64, end: Value) -> (usize, usize) {
                        let len = len as i64;
                        let start = if start < 0 { start + len } else { start };
                        let end = match end {
                            Value::Int(end) => end,
                            Value::Nil => len,
                            _ => unreachable!()
                        };
                        let end = if end < 0 { end + len } else { end };
                        (start as usize, end as usize - start as usize)
                    }

                    let end = self.pop_expect()?;
                    let start = pop_expect_int!(self)?;

                    let value = match self.pop_expect()? {
                        Value::Obj(Obj::StringObj { value }) => {
                            let (start, len) = get_range_endpoints(value.len(), start, end);
                            let value = (*value).chars().skip(start).take(len).collect::<String>();
                            Value::Obj(Obj::StringObj { value: Box::new(value) })
                        }
                        Value::Obj(Obj::ArrayObj { value }) => {
                            let (start, len) = get_range_endpoints(value.len(), start, end);
                            let value = value.into_iter().skip(start).take(len).collect::<Vec<_>>();
                            Value::Obj(Obj::ArrayObj { value })
                        }
                        _ => unreachable!()
                    };
                    self.push(value);
                }
                Opcode::GStore => {
                    let global_name: String = pop_expect_string!(self)?;
                    let value = self.pop_expect()?;
                    self.globals.insert(global_name, value);
                }
                Opcode::LStore0 => self.store(0)?,
                Opcode::LStore1 => self.store(1)?,
                Opcode::LStore2 => self.store(2)?,
                Opcode::LStore3 => self.store(3)?,
                Opcode::LStore4 => self.store(4)?,
                Opcode::LStore => {
                    let stack_slot = self.read_byte_expect()?;
                    self.store(stack_slot)?
                }
                Opcode::GLoad => {
                    let global_name: String = pop_expect_string!(self)?;
                    let value = self.globals.get(&global_name)
                        .unwrap_or(&Value::Nil)
                        .clone();
                    self.push(value);
                }
                Opcode::LLoad0 => self.load(0)?,
                Opcode::LLoad1 => self.load(1)?,
                Opcode::LLoad2 => self.load(2)?,
                Opcode::LLoad3 => self.load(3)?,
                Opcode::LLoad4 => self.load(4)?,
                Opcode::LLoad => {
                    let stack_slot = self.read_byte_expect()?;
                    self.load(stack_slot)?
                }
                Opcode::Jump => {
                    let jump_offset = self.read_byte_expect()?;

                    let frame: &mut CallFrame = current_frame!(self);
                    frame.ip += jump_offset;
                }
                Opcode::JumpIfF => {
                    let jump_offset = self.read_byte_expect()?;
                    let cond = pop_expect_bool!(self)?;
                    if !cond {
                        let frame: &mut CallFrame = current_frame!(self);
                        frame.ip += jump_offset;
                    }
                }
                Opcode::JumpB => {
                    let jump_offset = self.read_byte_expect()?;
                    let frame: &mut CallFrame = current_frame!(self);
                    frame.ip -= jump_offset;
                }
                Opcode::Invoke => {
                    let target = self.pop_expect()?;
                    let mut arity = self.read_byte_expect()?;
                    let has_return = self.read_byte_expect()? == 1;

                    match target {
                        Value::Obj(Obj::StringObj { value }) => {
                            if !value.starts_with("~") {
                                return Err(InterpretError::TypeError("Function".to_string(), "String".to_string()));
                            }

                            let func_name = value.replace("~", "");
                            if let Some(native_fn) = NATIVE_FNS_MAP.get(&func_name) {
                                let native_fn: &NativeFn = native_fn;

                                let len = self.stack.len();
                                let args = self.stack.split_off(len - arity);
                                if has_return { self.stack.pop(); } // <-- Pop off nil (<ret> placeholder) value
                                if let Some(value) = native_fn.invoke(self.ctx.clone(), args) {
                                    self.push(value);
                                }
                                continue;
                            } else {
                                unreachable!() // A native fn that exists in bytecode but not at runtime is "impossible"
                            }
                        }
                        Value::Fn { name, code } => {
                            if has_return { arity += 1 }
                            let frame = CallFrame {
                                ip: 0,
                                code,
                                stack_offset: self.stack.len() - arity,
                                name,
                            };
                            if self.call_stack.len() + 1 >= STACK_LIMIT {
                                break Err(InterpretError::StackOverflow);
                            } else {
                                self.call_stack.push(frame);
                            }
                        }
                        v @ _ => {
                            return Err(InterpretError::TypeError("Function".to_string(), v.to_string()));
                        }
                    }
                }
                Opcode::Pop => {
                    self.pop_expect()?;
                }
                Opcode::PopN => {
                    let num_to_pop = self.read_byte_expect()?;
                    self.pop_expect_n(num_to_pop)?;
                }
                Opcode::Return => {
                    let is_main_frame = self.call_stack.len() == 1;

                    if is_main_frame {
                        let top = self.pop();
                        break Ok(top);
                    } else {
                        // Pop off current frame, so the next loop will resume with the previous frame
                        self.call_stack.pop();
                    }
                }
            }
        }
    }
}