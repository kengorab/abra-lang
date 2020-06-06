use crate::builtins::native_types::{NativeString, NativeType, NativeArray};
use crate::vm::compiler::{Module, UpvalueCaptureKind};
use crate::vm::opcode::Opcode;
use crate::vm::value::{Value, Obj, FnValue, ClosureValue, TypeValue, InstanceObj};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::vec_deque::VecDeque;
use std::cell::RefCell;
use std::sync::Arc;

// Helper macros
macro_rules! pop_expect_string {
    ($self: expr) => (
        match $self.pop_expect()? {
            Value::Str(value) => Ok(value),
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

macro_rules! pop_expect_obj {
    ($self: expr) => (
        match $self.pop_expect()? {
            Value::Obj(value) => Ok(value),
            v @ _ => Err(InterpretError::TypeError("Obj".to_string(), v.to_string()))
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

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Upvalue {
    pub slot_idx: usize,
    pub is_closed: bool,
    pub val: Option<Value>,
}

struct CallFrame {
    ip: usize,
    code: Vec<u8>,
    stack_offset: usize,
    name: String,
    upvalues: Vec<Arc<RefCell<Upvalue>>>,
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
    open_upvalues: HashMap<usize, Arc<RefCell<Upvalue>>>,
}

const STACK_LIMIT: usize = 64;

impl VM {
    pub fn new(module: Module, ctx: VMContext) -> Self {
        let name = "$main".to_string();
        let Module { code, constants } = module;
        let root_frame = CallFrame { ip: 0, code, stack_offset: 0, name, upvalues: vec![] };

        VM {
            ctx,
            constants,
            call_stack: vec![root_frame],
            stack: Vec::new(),
            globals: HashMap::new(),
            open_upvalues: HashMap::new(),
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

    fn store_local(&mut self, stack_slot: usize) -> Result<(), InterpretError> {
        let CallFrame { stack_offset, .. } = current_frame!(self);
        let stack_slot = stack_slot + *stack_offset;
        let value = self.pop_expect()?;
        self.stack_insert_at(stack_slot, value); // TODO: Raise InterpretError when OOB stack_slot
        Ok(())
    }

    fn store_upvalue(&mut self, upvalue_idx: usize) -> Result<(), InterpretError> {
        let frame = current_frame!(self);
        let uv = frame.upvalues[upvalue_idx].clone();
        let mut uv = (*uv).borrow_mut();

        if uv.is_closed {
            let value = self.pop_expect()?;
            uv.val = Some(value);
        } else {
            self.store_local(uv.slot_idx)?;
        }
        Ok(())
    }

    fn load_local(&mut self, stack_slot: usize) -> Result<(), InterpretError> {
        let CallFrame { stack_offset, .. } = current_frame!(self);
        let stack_slot = stack_slot + *stack_offset;
        let value = self.stack_get(stack_slot);
        Ok(self.push(value))
    }

    fn load_upvalue(&mut self, upvalue_idx: usize) -> Result<(), InterpretError> {
        let frame = current_frame!(self);
        let uv = frame.upvalues[upvalue_idx].clone();
        let uv = uv.borrow();

        if uv.is_closed {
            // TODO: Note, this won't work for mutable upvalues, ie. `myArray.push(1)`
            let val = uv.val.as_ref()
                .expect("A closed upvalue should have a val")
                .clone();
            self.push(val);
        } else {
            self.load_local(uv.slot_idx)?;
        }
        Ok(())
    }

    fn close_upvalues_from_idx(&mut self, stack_slot: usize) -> Result<(), InterpretError> {
        let max_slot_idx = self.open_upvalues.keys().max();
        if max_slot_idx.is_none() { return Ok(()); }

        for idx in stack_slot..=*max_slot_idx.unwrap() {
            match self.open_upvalues.remove(&idx) {
                None => continue,
                Some(uv) => {
                    let mut uv = (*uv).borrow_mut();
                    uv.is_closed = true;
                    let value = self.stack_get(uv.slot_idx);
                    uv.val = Some(value);
                }
            }
        }
        Ok(())
    }

    fn close_upvalues_for_frame(&mut self) -> Result<(), InterpretError> {
        let stack_offset = current_frame!(self).stack_offset.clone();
        self.close_upvalues_from_idx(stack_offset)
    }

    fn close_upvalue(&mut self) -> Result<(), InterpretError> {
        let cur_stack_slot = self.stack.len() - 1;
        self.close_upvalues_from_idx(cur_stack_slot)
    }

    #[inline]
    fn make_closure(&mut self) -> Result<(), InterpretError> {
        let function = self.pop_expect()?;
        let (name, code, upvalues, receiver) = match function {
            Value::Fn(FnValue { name, code, upvalues, receiver }) => Ok((name, code, upvalues, receiver)),
            v @ _ => Err(InterpretError::TypeError("Function".to_string(), v.to_string())),
        }?;

        let captures = upvalues.iter().map(|uv| {
            match uv.capture_kind {
                UpvalueCaptureKind::Local { local_idx } => {
                    let frame = current_frame!(self);
                    let idx = frame.stack_offset + local_idx;

                    match self.open_upvalues.get(&idx) {
                        None => {
                            let uv = Upvalue { slot_idx: idx, is_closed: false, val: None };
                            let uv = Arc::new(RefCell::new(uv));
                            self.open_upvalues.insert(idx, uv.clone());
                            uv
                        }
                        Some(uv) => uv.clone(),
                    }
                }
                UpvalueCaptureKind::Upvalue { upvalue_idx } => {
                    let frame = current_frame!(self);
                    frame.upvalues[upvalue_idx].clone()
                }
            }
        });
        // Because upvalues are discovered in static order (the order in which they're encountered
        // in the code), but closed in _stack-pop_ order (aka, reversed), this needs to be reversed
        // in order for the upvalue_idx's to line up properly.
        let captures = captures.rev().collect::<Vec<_>>();

        self.push(Value::Closure(ClosureValue { name, code, captures, receiver }));
        Ok(())
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
                    self.push(Value::new_string_obj(concat))
                }
                Opcode::T => self.push(Value::Bool(true)),
                Opcode::F => self.push(Value::Bool(false)),
                Opcode::Negate => {
                    let val = pop_expect_bool!(self)?;
                    self.push(Value::Bool(!val));
                }
                Opcode::LT => self.comp_values(Opcode::LT)?,
                Opcode::LTE => self.comp_values(Opcode::LTE)?,
                Opcode::GT => self.comp_values(Opcode::GT)?,
                Opcode::GTE => self.comp_values(Opcode::GTE)?,
                Opcode::Neq => self.comp_values(Opcode::Neq)?,
                Opcode::Eq => self.comp_values(Opcode::Eq)?,
                Opcode::New => {
                    let size = self.read_byte_expect()?;
                    let mut fields = Vec::with_capacity(size);
                    for _ in 0..size {
                        let field_value = self.pop_expect()?;
                        fields.push(field_value);
                    }

                    let type_value = self.pop_expect()?;

                    let inst = Value::new_instance_obj(type_value, fields);
                    self.push(inst);
                }
                Opcode::Init => {
                    let instance_value = pop_expect_obj!(self)?;
                    let type_value = match self.pop_expect()? {
                        Value::Type(type_value) => type_value,
                        _ => unreachable!()
                    };

                    for (_, mut method_value) in type_value.methods {
                        method_value.receiver = Some(instance_value.clone());
                        match *instance_value.borrow_mut() {
                            Obj::InstanceObj(ref mut obj) => obj.fields.push(Value::Fn(method_value)),
                            _ => unreachable!()
                        }
                    }

                    let initialized_inst = Value::Obj(instance_value);
                    self.push(initialized_inst);
                }
                Opcode::GetField => {
                    let inst = self.pop_expect()?;
                    let field_idx = self.read_byte_expect()?;

                    let value = match inst {
                        Value::Obj(obj) => match &*obj.borrow() {
                            Obj::InstanceObj(inst) => inst.fields[field_idx].clone(),
                            Obj::StringObj { .. } => NativeString::get_field_value(&obj, field_idx),
                            Obj::ArrayObj { .. } => NativeArray::get_field_value(&obj, field_idx),
                            _ => unreachable!()
                        }
                        Value::Type(TypeValue { static_fields, .. }) => {
                            let (_, field_value) = static_fields[field_idx].clone();
                            Value::Fn(field_value)
                        }
                        _ => unreachable!()
                    };
                    self.push(value);
                }
                Opcode::SetField => {
                    let field_idx = self.read_byte_expect()?;
                    let inst = pop_expect_obj!(self)?;
                    let value = self.pop_expect()?;

                    match *inst.borrow_mut() {
                        Obj::InstanceObj(InstanceObj { ref mut fields, .. }) => {
                            fields[field_idx] = value.clone();
                        }
                        _ => unimplemented!()
                    };
                    self.push(value);
                }
                Opcode::MapMk => {
                    let size = self.read_byte_expect()?;
                    let mut items = HashMap::with_capacity(size);
                    for _ in 0..size {
                        let value = self.pop_expect()?;
                        let key = pop_expect_string!(self)?;
                        items.insert(key, value);
                    }
                    self.push(Value::new_map_obj(items));
                }
                Opcode::MapLoad => {
                    let key = pop_expect_obj!(self)?;
                    let key = match &*key.borrow() {
                        Obj::StringObj(key) => key.clone(),
                        _ => unreachable!()
                    };
                    let obj = pop_expect_obj!(self)?;
                    let val = match &*obj.borrow() {
                        Obj::MapObj(value) => match value.get(&key) {
                            Some(val) => val.clone(),
                            None => Value::Nil
                        }
                        _ => unreachable!()
                    };
                    self.push(val)
                }
                Opcode::MapStore => {
                    let value = self.pop_expect()?;
                    let idx = pop_expect_obj!(self)?;
                    let idx = match &*idx.borrow() {
                        Obj::StringObj(idx) => idx.clone(),
                        _ => unreachable!()
                    };

                    let obj = pop_expect_obj!(self)?;
                    match *obj.borrow_mut() {
                        Obj::MapObj(ref mut values) => values.insert(idx, value),
                        _ => unreachable!()
                    };
                    self.push(Value::Obj(obj));
                }
                Opcode::ArrMk => {
                    let size = self.read_byte_expect()?;
                    let mut arr_items = VecDeque::<Value>::with_capacity(size as usize);
                    for _ in 0..size {
                        arr_items.push_front(self.pop_expect()?);
                    }
                    self.push(Value::new_array_obj(arr_items.into()));
                }
                Opcode::ArrLoad => {
                    let idx = pop_expect_int!(self)?;
                    let obj = pop_expect_obj!(self)?;
                    let value = match &*obj.borrow() {
                        Obj::StringObj(value) => {
                            let len = value.len() as i64;
                            let idx = if idx < 0 { idx + len } else { idx };

                            match (*value).chars().nth(idx as usize) {
                                Some(ch) => Value::new_string_obj(ch.to_string()),
                                None => Value::Nil
                            }
                        }
                        Obj::ArrayObj(value) => {
                            let len = value.len() as i64;
                            if idx < -len || idx >= len {
                                Value::Nil
                            } else {
                                let idx = if idx < 0 { idx + len } else { idx };
                                value[idx as usize].clone()
                            }
                        }
                        _ => unreachable!()
                    };
                    self.push(value);
                }
                Opcode::ArrStore => {
                    let value = self.pop_expect()?;
                    let idx = pop_expect_int!(self)? as usize;
                    let obj = pop_expect_obj!(self)?;
                    match *obj.borrow_mut() {
                        Obj::ArrayObj(ref mut values) => {
                            if values.len() < idx {
                                let mut padding = std::iter::repeat(Value::Nil)
                                    .take(idx - values.len())
                                    .collect::<Vec<Value>>();
                                values.append(&mut padding);
                                values.push(value);
                            } else if values.len() == idx {
                                values.push(value);
                            } else {
                                values[idx] = value;
                            }
                        }
                        _ => unreachable!()
                    }
                    self.push(Value::Obj(obj));
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

                    let obj = pop_expect_obj!(self)?;
                    let value = match &*obj.borrow() {
                        Obj::StringObj(value) => {
                            let (start, len) = get_range_endpoints(value.len(), start, end);
                            let value = (*value).chars().skip(start).take(len).collect::<String>();
                            Value::new_string_obj(value)
                        }
                        Obj::ArrayObj(value) => {
                            let (start, len) = get_range_endpoints(value.len(), start, end);
                            let values = value.iter().skip(start).take(len).map(|i| i.clone()).collect::<Vec<Value>>();
                            Value::new_array_obj(values)
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
                Opcode::LStore0 => self.store_local(0)?,
                Opcode::LStore1 => self.store_local(1)?,
                Opcode::LStore2 => self.store_local(2)?,
                Opcode::LStore3 => self.store_local(3)?,
                Opcode::LStore4 => self.store_local(4)?,
                Opcode::LStore => {
                    let stack_slot = self.read_byte_expect()?;
                    self.store_local(stack_slot)?
                }
                Opcode::UStore0 => self.store_upvalue(0)?,
                Opcode::UStore1 => self.store_upvalue(1)?,
                Opcode::UStore2 => self.store_upvalue(2)?,
                Opcode::UStore3 => self.store_upvalue(3)?,
                Opcode::UStore4 => self.store_upvalue(4)?,
                Opcode::UStore => {
                    let upvalue_idx = self.read_byte_expect()?;
                    self.store_upvalue(upvalue_idx)?;
                }
                Opcode::GLoad => {
                    let global_name: String = pop_expect_string!(self)?;
                    let value = self.globals.get(&global_name)
                        .unwrap_or(&Value::Nil)
                        .clone();
                    self.push(value);
                }
                Opcode::LLoad0 => self.load_local(0)?,
                Opcode::LLoad1 => self.load_local(1)?,
                Opcode::LLoad2 => self.load_local(2)?,
                Opcode::LLoad3 => self.load_local(3)?,
                Opcode::LLoad4 => self.load_local(4)?,
                Opcode::LLoad => {
                    let stack_slot = self.read_byte_expect()?;
                    self.load_local(stack_slot)?
                }
                Opcode::ULoad0 => self.load_upvalue(0)?,
                Opcode::ULoad1 => self.load_upvalue(1)?,
                Opcode::ULoad2 => self.load_upvalue(2)?,
                Opcode::ULoad3 => self.load_upvalue(3)?,
                Opcode::ULoad4 => self.load_upvalue(4)?,
                Opcode::ULoad => {
                    let upvalue_idx = self.read_byte_expect()?;
                    self.load_upvalue(upvalue_idx)?;
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

                    let (name, code, upvalues, receiver) = match target {
                        Value::NativeFn(native_fn) => {
                            let num_args = self.stack.len() - arity;
                            let args = self.stack.split_off(num_args);
                            if has_return { self.stack.pop(); } // <-- Pop off nil (<ret> placeholder) value
                            if let Some(value) = native_fn.invoke(&self.ctx, args) {
                                self.push(value);
                            }
                            continue;
                        }
                        Value::Fn(FnValue { name, code, receiver, .. }) => (name, code, vec![], receiver),
                        Value::Closure(ClosureValue { name, code, captures, receiver, .. }) => (name, code, captures, receiver),
                        v @ _ => {
                            return Err(InterpretError::TypeError("Function".to_string(), v.to_string()));
                        }
                    };

                    match receiver {
                        Some(receiver) => {
                            let mut args = self.stack.split_off(self.stack.len() - arity);
                            self.stack.push(Value::Obj(receiver));
                            self.stack.append(&mut args);
                            arity += 1;
                        }
                        None => {}
                    }
                    if has_return { arity += 1 }
                    let stack_offset = self.stack.len() - arity;

                    let frame = CallFrame { ip: 0, code, stack_offset, name, upvalues };
                    if self.call_stack.len() + 1 >= STACK_LIMIT {
                        break Err(InterpretError::StackOverflow);
                    }
                    self.call_stack.push(frame);
                }
                Opcode::ClosureMk => self.make_closure()?,
                Opcode::CloseUpvalue => self.close_upvalue()?,
                Opcode::CloseUpvalueAndPop => {
                    self.close_upvalue()?;
                    self.pop_expect()?;
                }
                Opcode::Pop => {
                    self.pop_expect()?;
                }
                Opcode::PopN => {
                    let num_to_pop = self.read_byte_expect()?;
                    self.pop_expect_n(num_to_pop)?;
                }
                Opcode::Dup => {
                    let value = self.stack.last().unwrap().clone();
                    self.stack.push(value);
                }
                Opcode::Return => {
                    let is_main_frame = self.call_stack.len() == 1;

                    if is_main_frame {
                        let top = self.pop();
                        break Ok(top.clone());
                    } else {
                        // Ensure that any upvalues that are created in the current frame are closed
                        self.close_upvalues_for_frame()?;

                        // Pop off current frame, so the next loop will resume with the previous frame
                        self.call_stack.pop();
                    }
                }
            }
        }
    }
}