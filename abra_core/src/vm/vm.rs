use crate::builtins::native_types::{NativeString, NativeType, NativeArray};
use crate::vm::compiler::{Module, UpvalueCaptureKind};
use crate::vm::opcode::Opcode;
use crate::vm::value::{Value, Obj, FnValue, ClosureValue, TypeValue, InstanceObj, EnumValue};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::vec_deque::VecDeque;
use std::cell::RefCell;
use std::sync::Arc;
use crate::builtins::native_fns::NativeFn;

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
    start_stack_idx: usize,
    name: String,
    upvalues: Vec<Arc<RefCell<Upvalue>>>,
    local_addrs: Vec<usize>,
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
    pub ctx: VMContext,
    constants: Vec<Value>,
    type_constant_indexes: HashMap<String, usize>,
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
        let root_frame = CallFrame { ip: 0, code, start_stack_idx: 0, name, upvalues: vec![], local_addrs: vec![] };

        let type_constant_indexes = constants.iter().enumerate()
            .filter_map(|(idx, a)|
                if let Value::Type(TypeValue { name, .. }) = a {
                    Some((name.clone(), idx))
                } else { None }
            )
            .collect();

        VM {
            ctx,
            constants,
            type_constant_indexes,
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
                if index == self.stack.len() {
                    self.stack.push(value)
                } else {
                    let frame = self.call_stack.last().unwrap();
                    let chunk_name = &frame.name;
                    let offset = frame.ip;
                    panic!("Runtime error [{}+{}]:\n  No stack slot available at index {}", chunk_name, offset, index)
                }
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
        let frame = current_frame!(self);
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

    fn load_constant(&mut self, const_idx: usize) -> Result<(), InterpretError> {
        let val = self.constants.get(const_idx)
            .ok_or(InterpretError::ConstIdxOutOfBounds)?
            .clone();
        self.push(val);
        Ok(())
    }

    fn stack_slot_for_local(&mut self, local_idx: usize) -> usize {
        let CallFrame { local_addrs, .. } = current_frame!(self);
        // This is worth documenting: if we're trying to resolve a local_idx and we haven't yet recorded
        // it in local_addrs, just point that local_idx to the top of the stack. This works for the only
        // case where it's relevant (default-valued arguments, in lambdas), but will probably ultimately
        // need to be refactored.
        if local_idx >= local_addrs.len() {
            local_addrs.push(self.stack.len() - 1);
        }
        local_addrs[local_idx]
    }

    fn load_local(&mut self, local_idx: usize) -> Result<(), InterpretError> {
        let stack_slot = self.stack_slot_for_local(local_idx);
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
            let value = self.stack_get(uv.slot_idx);
            self.push(value);
        }
        Ok(())
    }

    fn store_local(&mut self, local_idx: usize) -> Result<(), InterpretError> {
        let stack_slot = self.stack_slot_for_local(local_idx);
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
            let value = self.pop_expect()?;
            self.stack_insert_at(uv.slot_idx, value);
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
        let start_stack_idx = current_frame!(self).start_stack_idx;
        self.close_upvalues_from_idx(start_stack_idx)
    }

    fn close_upvalue(&mut self) -> Result<(), InterpretError> {
        let cur_stack_slot = self.stack.len() - 1;
        self.close_upvalues_from_idx(cur_stack_slot)
    }

    #[inline]
    fn make_closure(&mut self) -> Result<(), InterpretError> {
        let function = self.pop_expect()?;
        let (name, code, upvalues, receiver, has_return) = match function {
            Value::Fn(FnValue { name, code, upvalues, receiver, has_return }) => Ok((name, code, upvalues, receiver, has_return)),
            v @ _ => Err(InterpretError::TypeError("Function".to_string(), v.to_string())),
        }?;

        let captures = upvalues.iter().map(|uv| {
            match uv.capture_kind {
                UpvalueCaptureKind::Local { local_idx } => {
                    let stack_slot = self.stack_slot_for_local(local_idx);

                    match self.open_upvalues.get(&stack_slot) {
                        None => {
                            let uv = Upvalue { slot_idx: stack_slot, is_closed: false, val: None };
                            let uv = Arc::new(RefCell::new(uv));
                            self.open_upvalues.insert(stack_slot, uv.clone());
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

        self.push(Value::Closure(ClosureValue { name, code, captures, receiver, has_return }));
        Ok(())
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
            (Value::Type(TypeValue { name: name1, .. }), Value::Type(TypeValue { name: name2, .. })) => name1.partial_cmp(&name2),
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

    fn invoke_native_fn(&mut self, arity: usize, native_fn: &NativeFn) -> Result<Option<Value>, InterpretError> {
        let num_args = self.stack.len() - arity;
        let args = self.stack.split_off(num_args);
        if native_fn.has_return {
            self.stack.pop(); // <-- Pop off nil (<ret> placeholder) value
        }
        return Ok(native_fn.invoke(args, self));
    }

    fn start_call_frame(&mut self, arity: usize, func_value: Value) -> Result<(), InterpretError> {
        let mut arity = arity;
        let (name, code, upvalues, receiver, has_return) = match func_value {
            Value::Fn(FnValue { name, code, receiver, has_return, .. }) => (name, code, vec![], receiver, has_return),
            Value::Closure(ClosureValue { name, code, captures, receiver, has_return, .. }) => (name, code, captures, receiver, has_return),
            _ => unreachable!("Native functions should be handled separately")
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
        let start_stack_idx = self.stack.len() - arity;

        // Initialize the frame with local_addrs for the args already on the stack, since
        // the function body itself won't mark these as locals
        let mut local_addrs = Vec::new();
        for l in 0..arity {
            local_addrs.push(self.stack.len() - (arity - l));
        }

        let frame = CallFrame { ip: 0, code, start_stack_idx, name, upvalues, local_addrs };
        if self.call_stack.len() + 1 >= STACK_LIMIT {
            return Err(InterpretError::StackOverflow);
        }
        self.call_stack.push(frame);

        Ok(())
    }

    pub fn invoke_fn(&mut self, args: Vec<Value>, func: Value) -> Result<Option<Value>, InterpretError> {
        let arity = args.len();
        let has_return = match &func {
            Value::NativeFn(native_fn) => return self.invoke_native_fn(arity, native_fn),
            Value::Fn(FnValue { has_return, .. }) |
            Value::Closure(ClosureValue { has_return, .. }) => has_return.clone(),
            _ => unreachable!()
        };

        if has_return {
            self.push(Value::Nil);
        }

        let arity = args.len();
        for arg in args {
            self.push(arg);
        }

        self.start_call_frame(arity, func)?;

        let res = self.run_from_call_stack_depth(self.call_stack.len())?;
        if has_return {
            Ok(res)
        } else {
            if let Some(res) = res {
                self.push(res);
            }
            Ok(None)
        }
    }

    pub fn run(&mut self) -> Result<Option<Value>, InterpretError> {
        self.run_from_call_stack_depth(1)
    }

    /// Run the VM to execute bytecode, starting from `init_call_stack_depth`. When running the VM,
    /// via the `run` method, this should be 1; when executing a function's bytecode from a native
    /// context, the initial call stack depth needs to be provided so we know when to return the
    /// value to that native context. (See comments in the `Opcode::Return` block)
    fn run_from_call_stack_depth(&mut self, init_call_stack_depth: usize) -> Result<Option<Value>, InterpretError> {
        loop {
            let instr = self.read_instr().ok_or(InterpretError::EndOfBytes)?;

            match instr {
                Opcode::Constant => {
                    let const_idx = self.read_byte_expect()?;
                    self.load_constant(const_idx)?;
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
                            Obj::EnumVariantObj(inst) => inst.methods[field_idx].clone(),
                            Obj::StringObj { .. } => NativeString::get_field_value(&obj, field_idx),
                            Obj::ArrayObj { .. } => NativeArray::get_field_value(&obj, field_idx),
                            _ => unreachable!()
                        }
                        Value::Type(TypeValue { static_fields, .. }) => {
                            let (_, field_value) = static_fields[field_idx].clone();
                            Value::Fn(field_value)
                        }
                        Value::Enum(EnumValue { variants, static_fields, methods, .. }) => {
                            if field_idx >= variants.len() {
                                let (_, field_value) = static_fields[field_idx - variants.len()].clone();
                                Value::Fn(field_value)
                            } else {
                                let (_, variant_value) = variants[field_idx].clone();
                                let instance_value = if let Value::Obj(instance_value) = Value::new_enum_variant_obj(variant_value) {
                                    for (_, mut method_value) in methods {
                                        method_value.receiver = Some(instance_value.clone());
                                        match *instance_value.borrow_mut() {
                                            Obj::EnumVariantObj(ref mut obj) => obj.methods.push(Value::Fn(method_value)),
                                            _ => unreachable!()
                                        }
                                    }
                                    instance_value
                                } else { unreachable!() };
                                Value::Obj(instance_value)
                            }
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

                    let frame = current_frame!(self);
                    frame.ip += jump_offset;
                }
                Opcode::JumpIfF => {
                    let jump_offset = self.read_byte_expect()?;
                    let cond = pop_expect_bool!(self)?;
                    if !cond {
                        let frame = current_frame!(self);
                        frame.ip += jump_offset;
                    }
                }
                Opcode::JumpB => {
                    let jump_offset = self.read_byte_expect()?;
                    let frame = current_frame!(self);
                    frame.ip -= jump_offset;
                }
                Opcode::Invoke => {
                    let target = self.pop_expect()?;
                    let arity = self.read_byte_expect()?;

                    match &target {
                        Value::NativeFn(native_fn) => {
                            if let Some(value) = self.invoke_native_fn(arity, native_fn)? {
                                self.push(value);
                            }
                            continue;
                        }
                        Value::Obj(obj) => {
                            let mut obj = (*obj).borrow_mut();
                            match *obj {
                                Obj::EnumVariantObj(ref mut evv) => {
                                    let arity = evv.arity;
                                    let num_args = self.stack.len() - arity;
                                    let args = self.stack.split_off(num_args);
                                    self.stack.pop(); // <-- Pop off nil (<ret> placeholder) value

                                    evv.values = Some(args);
                                }
                                _ => unreachable!()
                            };
                        }
                        _ => {
                            self.start_call_frame(arity, target)?;
                            continue;
                        }
                    }

                    // The Value::Obj(Obj::EnumVariant) path above is the only way to reach here; since
                    // it mutates `target` as an Arc<RefCell<Obj>>, we can't push it until the match ends.
                    self.push(target);
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
                Opcode::MarkLocal => {
                    let local_idx = self.read_byte_expect()?;
                    let CallFrame { local_addrs, .. } = current_frame!(self);

                    let local_addr = self.stack.len() - 1;
                    if local_idx >= local_addrs.len() {
                        local_addrs.push(local_addr);
                    } else {
                        local_addrs[local_idx] = local_addr;
                    }
                }
                Opcode::Typeof => {
                    let v = self.pop_expect()?;
                    match v {
                        Value::Int(_) => self.load_constant(self.type_constant_indexes["Int"])?,
                        Value::Float(_) => self.load_constant(self.type_constant_indexes["Float"])?,
                        Value::Bool(_) => self.load_constant(self.type_constant_indexes["Bool"])?,
                        Value::Obj(obj) => match &*obj.borrow() {
                            Obj::StringObj(_) => self.load_constant(self.type_constant_indexes["String"])?,
                            Obj::InstanceObj(inst) => self.push(*inst.typ.clone()),
                            Obj::EnumVariantObj(_) |
                            Obj::ArrayObj(_) |
                            Obj::MapObj(_) => unimplemented!()
                        }
                        Value::Nil => self.push(Value::Nil),
                        Value::Fn(_) |
                        Value::Closure(_) |
                        Value::NativeFn(_) |
                        Value::Type(_) |
                        Value::Enum(_) => unimplemented!(),
                        Value::Str(_) => unreachable!("There should never be a value of type Str"),
                    }
                }
                Opcode::Return => {
                    let is_complete = self.call_stack.len() == init_call_stack_depth;

                    // If the call stack is complete (ie, we've returned back up to the stack depth
                    // when the run was issued), we want to pop the value off the stack and return it.
                    let top = if is_complete { self.pop() } else { None };

                    // Ensure that any upvalues that are created in the current frame are closed, and
                    // pop off current frame, so the next loop will resume with the previous frame. We
                    // want to do this regardless of whether we're complete. This is important because
                    // if the `run` function is called when invoking a fn from a native context, the
                    // completeness condition will be dependant on init_call_stack_depth > 1.
                    self.close_upvalues_for_frame()?;
                    self.call_stack.pop();

                    if is_complete { break Ok(top); }
                }
            }
        }
    }
}