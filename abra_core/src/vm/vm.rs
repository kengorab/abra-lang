use crate::builtins::common::to_string;
use crate::vm::compiler::{Module, UpvalueCaptureKind};
use crate::vm::opcode::Opcode;
use crate::vm::value::{Value, FnValue, ClosureValue, NativeFn, TypeValue, InstanceObj, EnumValue, EnumInstanceObj};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::vec_deque::VecDeque;
use std::cell::RefCell;
use std::sync::Arc;
use crate::builtins::native_value_trait::NativeValue;

// Helper macros
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

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Upvalue {
    pub slot_idx: usize,
    pub is_closed: bool,
    pub val: Option<Value>,
}

#[derive(Default)]
struct CallFrame {
    ip: usize,
    code: Vec<Opcode>,
    start_stack_idx: usize,
    name: String,
    upvalues: Vec<Arc<RefCell<Upvalue>>>,
    local_addrs: Vec<usize>,
}

pub struct VMContext {
    pub print: Box<dyn Fn(&str)>,
    pub prompt: Box<dyn Fn(&str) -> String>,
}

impl VMContext {
    pub fn default() -> Self {
        VMContext {
            print: Box::new(VMContext::print),
            prompt: Box::new(VMContext::prompt),
        }
    }

    pub fn print(input: &str) {
        print!("{}", input)
    }

    pub fn prompt(prompt: &str) -> String {
        use std::io::{stdin, stdout, Write};
        let mut s = String::new();
        VMContext::print(prompt);
        let _ = stdout().flush();
        stdin().read_line(&mut s).expect("Did not enter a correct string");
        if let Some('\n') = s.chars().next_back() { s.pop(); }
        if let Some('\r') = s.chars().next_back() { s.pop(); }
        s
    }
}

pub struct VM {
    pub ctx: VMContext,
    constants: Vec<Vec<Value>>,
    type_constant_indexes: HashMap<String, usize>,
    call_stack: Vec<CallFrame>,
    stack: Vec<Value>,
    globals: Vec<Value>,
    num_globals: usize,
    open_upvalues: HashMap<usize, Arc<RefCell<Upvalue>>>,
}

const STACK_LIMIT: usize = 1024;

impl VM {
    pub fn new(ctx: VMContext) -> Self {
        VM {
            ctx,
            constants: vec![],
            type_constant_indexes: HashMap::new(),
            call_stack: vec![],
            stack: Vec::new(),
            globals: vec![Value::Nil; 1024],
            num_globals: 0,
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

    #[inline]
    fn pop_expect_boolish(&mut self) -> Result<bool, InterpretError> {
        let val = self.stack.pop().ok_or(InterpretError::StackEmpty)?;
        match val {
            Value::Bool(b) => Ok(b),
            Value::Nil => Ok(false),
            _ => unreachable!("Cannot implicitly convert value to boolean")
        }
    }

    fn read_instr(&mut self) -> Option<Opcode> {
        let frame = current_frame!(self);
        if frame.code.len() == frame.ip {
            None
        } else {
            let instr = frame.code[frame.ip];
            frame.ip += 1;
            Some(instr)
        }
    }

    fn load_constant(&mut self, (module_idx, const_idx): (usize, usize)) -> Result<(), InterpretError> {
        let val = self.constants[module_idx].get(const_idx)
            .ok_or(InterpretError::ConstIdxOutOfBounds)?
            .clone();
        self.push(val);
        Ok(())
    }

    pub fn load_type(&self, type_id: usize) -> &TypeValue {
        if let Value::Type(tv) = &self.globals[type_id] {
            tv
        } else { panic!("Could not load type for id: {}", type_id) }
    }

    pub fn load_enum(&self, type_id: usize) -> &EnumValue {
        if let Value::Enum(ev) = &self.globals[type_id] {
            ev
        } else { panic!("Could not load enum for id: {}", type_id) }
    }

    fn load_type_methods(&self, type_id: usize) -> &Vec<(String, Value)> {
        match &self.globals[type_id] {
            Value::Type(tv) => &tv.methods,
            Value::Enum(ev) => &ev.methods,
            _ => unreachable!()
        }
    }

    pub fn type_id_for_name<S: AsRef<str>>(&self, name: S) -> usize {
        let name = name.as_ref();
        self.type_constant_indexes[name]
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
        self.stack_insert_at(stack_slot, value);
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
        let (name, code, upvalues, receiver) = match function {
            Value::Fn(FnValue { name, code, upvalues, receiver }) => Ok((name, code, upvalues, receiver)),
            v @ _ => Err(InterpretError::TypeError("Function".to_string(), to_string(&v, self))),
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

        self.push(Value::Closure(ClosureValue { name, code, captures, receiver }));
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
            (Value::Int(a), Value::Int(b)) => a.partial_cmp(&b),
            (Value::Int(a), Value::Float(b)) => (a as f64).partial_cmp(&b),
            (Value::Float(a), Value::Float(b)) => a.partial_cmp(&b),
            (Value::Float(a), Value::Int(b)) => a.partial_cmp(&(b as f64)),
            (Value::Type(TypeValue { name: name1, .. }), Value::Type(TypeValue { name: name2, .. })) => name1.partial_cmp(&name2),
            (Value::EnumInstanceObj(o), Value::Int(b)) => o.borrow().idx.partial_cmp(&(b as usize)),
            (Value::StringObj(s1), Value::StringObj(s2)) => {
                s1.borrow()._inner.partial_cmp(&s2.borrow()._inner)
            }
            (a, b) => {
                if a.eq(&b) { Some(Ordering::Equal) } else { Some(Ordering::Less) }
            }
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

    fn invoke_native_fn(&mut self, arity: usize, native_fn: &NativeFn) -> Result<Value, InterpretError> {
        let num_args = self.stack.len() - arity;
        let args = self.stack.split_off(num_args);
        return Ok(native_fn.invoke(args, self));
    }

    fn start_call_frame(&mut self, arity: usize, func_value: Value) -> Result<(), InterpretError> {
        let mut arity = arity;
        let (name, code, upvalues, receiver) = match func_value {
            Value::Fn(FnValue { name, code, receiver, .. }) => (name, code, vec![], receiver),
            Value::Closure(ClosureValue { name, code, captures, receiver, .. }) => (name, code, captures, receiver),
            _ => unreachable!("Native functions should be handled separately")
        };

        match receiver {
            Some(receiver) => {
                let mut args = self.stack.split_off(self.stack.len() - arity);
                self.stack.push(*receiver);
                self.stack.append(&mut args);
                arity += 1;
            }
            None => {}
        }
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

    pub fn invoke_fn(&mut self, args: Vec<Value>, func: Value) -> Result<Value, InterpretError> {
        let arity = args.len();
        if let Value::NativeFn(native_fn) = &func {
            return self.invoke_native_fn(arity, native_fn);
        }

        let arity = args.len();
        for arg in args {
            self.push(arg);
        }

        self.start_call_frame(arity, func)?;
        self.run_from_call_stack_depth(self.call_stack.len())
    }

    pub fn run(&mut self, module: Module) -> Result<Value, InterpretError> {
        let Module { name, is_native, num_globals, code, constants, .. } = module;

        if is_native {
            for v in &constants {
                match &v {
                    Value::Type(TypeValue { name, module_name, .. }) |
                    Value::Enum(EnumValue { name, module_name, .. }) => {
                        let fully_qualified_type_name = format!("{}/{}", module_name, name);
                        self.type_constant_indexes.insert(fully_qualified_type_name, self.num_globals);
                    }
                    _ => {}
                };
                self.globals[self.num_globals] = v.clone();
                self.num_globals += 1;
            }
        } else {
            self.num_globals += num_globals;
        }

        self.constants.push(constants);

        if code.is_empty() {
            return Ok(Value::Nil);
        }

        let frame = CallFrame { code, name, ..CallFrame::default() };
        self.call_stack.push(frame);

        self.run_from_call_stack_depth(1)
    }

    /// Run the VM to execute bytecode, starting from `init_call_stack_depth`. When running the VM,
    /// via the `run` method, this should be 1; when executing a function's bytecode from a native
    /// context, the initial call stack depth needs to be provided so we know when to return the
    /// value to that native context. (See comments in the `Opcode::Return` block)
    fn run_from_call_stack_depth(&mut self, init_call_stack_depth: usize) -> Result<Value, InterpretError> {
        loop {
            let instr = self.read_instr().ok_or(InterpretError::EndOfBytes)?;

            match instr {
                Opcode::Constant(mod_idx, const_idx) => self.load_constant((mod_idx, const_idx))?,
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
                Opcode::Pow => {
                    let b = self.pop_expect()?;
                    let a = self.pop_expect()?;

                    let (a, b) = match (a, b) {
                        (Value::Int(a), Value::Int(b)) => (a as f64, b as f64),
                        (Value::Float(a), Value::Int(b)) => (a, b as f64),
                        (Value::Int(a), Value::Float(b)) => (a as f64, b),
                        (Value::Float(a), Value::Float(b)) => (a, b),
                        _ => unreachable!()
                    };
                    self.push(Value::Float(a.powf(b)));
                }
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

                    let a = to_string(&a, self);
                    let b = to_string(&b, self);
                    let concat = a + &b;
                    self.push(Value::new_string_obj(concat))
                }
                Opcode::T => self.push(Value::Bool(true)),
                Opcode::F => self.push(Value::Bool(false)),
                Opcode::Negate => {
                    let val = self.pop_expect_boolish()?;
                    self.push(Value::Bool(!val));
                }
                Opcode::LT => self.comp_values(Opcode::LT)?,
                Opcode::LTE => self.comp_values(Opcode::LTE)?,
                Opcode::GT => self.comp_values(Opcode::GT)?,
                Opcode::GTE => self.comp_values(Opcode::GTE)?,
                Opcode::Neq => self.comp_values(Opcode::Neq)?,
                Opcode::Eq => self.comp_values(Opcode::Eq)?,
                Opcode::Xor => {
                    let b = self.pop_expect_boolish()?;
                    let a = self.pop_expect_boolish()?;
                    let val = if a && b { false } else { a || b };
                    self.push(Value::Bool(val));
                }
                Opcode::New(type_id, num_fields) => {
                    let mut fields = Vec::with_capacity(num_fields);
                    for _ in 0..num_fields {
                        let field_value = self.pop_expect()?;
                        fields.push(field_value);
                    }

                    let inst = match &self.globals[type_id] {
                        Value::Type(tv) => tv.construct(type_id, fields),
                        Value::Enum(_) => {
                            let idx = pop_expect_int!(self)? as usize;
                            let inst = EnumInstanceObj { idx, type_id, values: Some(fields) };
                            Value::new_enum_instance_obj(inst)
                        }
                        _ => unreachable!()
                    };

                    self.push(inst);
                }
                Opcode::GetField(idx) => {
                    let inst = self.pop_expect()?;

                    let value = match inst {
                        Value::ArrayObj(o) => (*o.borrow()).get_field_value(idx),
                        Value::StringObj(o) => (*o.borrow()).get_field_value(idx),
                        Value::SetObj(o) => (*o.borrow()).get_field_value(idx),
                        Value::MapObj(o) => (*o.borrow()).get_field_value(idx),
                        Value::InstanceObj(o) => {
                            let i = &*o.borrow();
                            i.fields[idx].clone()
                        }
                        Value::NativeInstanceObj(o) => {
                            let i = &*o.borrow();
                            i.inst.get_field_value(idx)
                        }
                        Value::EnumInstanceObj(o) => {
                            let inst = &*o.borrow();
                            if let Some(values) = &inst.values {
                                values[idx].clone()
                            } else { unreachable!() }
                        }
                        Value::Enum(EnumValue { variants, .. }) => {
                            let (_, variant_value) = variants[idx].clone();
                            variant_value
                        }
                        v => unreachable!("Value {} has no fields", v)
                    };
                    self.push(value);
                }
                Opcode::GetMethod(idx) => {
                    let inst = self.pop_expect()?;

                    let mut method = match &inst {
                        // Handle static methods first, since they don't need to be bound to any instance
                        Value::Type(TypeValue { static_fields, .. }) |
                        Value::Enum(EnumValue { static_fields, .. }) => {
                            let (_, field_value) = static_fields[idx].clone();
                            self.push(field_value);
                            continue;
                        }
                        // Otherwise, handle remaining value kinds
                        v => {
                            let type_id = match &v {
                                Value::Int(_) => self.type_constant_indexes["prelude/Int"],
                                Value::Float(_) => self.type_constant_indexes["prelude/Float"],
                                Value::ArrayObj(_) => self.type_constant_indexes["prelude/Array"],
                                Value::StringObj(_) => self.type_constant_indexes["prelude/String"],
                                Value::SetObj(_) => self.type_constant_indexes["prelude/Set"],
                                Value::MapObj(_) => self.type_constant_indexes["prelude/Map"],
                                Value::InstanceObj(o) => o.borrow().type_id,
                                Value::EnumInstanceObj(o) => o.borrow().type_id,
                                Value::NativeInstanceObj(o) => o.borrow().type_id,
                                _ => unreachable!("Remaining value kinds should have been handled above")
                            };
                            let (_, method) = self.load_type_methods(type_id)[idx].clone();
                            method
                        }
                    };
                    method.bind_fn_value(inst);

                    self.push(method);
                }
                Opcode::SetField(field_idx) => {
                    let target = self.pop_expect()?;
                    let value = self.pop_expect()?;

                    match target {
                        Value::InstanceObj(o) => {
                            let InstanceObj { ref mut fields, .. } = &mut *o.borrow_mut();
                            fields[field_idx] = value.clone();
                        }
                        Value::NativeInstanceObj(o) => {
                            let inst = &mut *o.borrow_mut();
                            inst.inst.set_field_value(field_idx, value.clone())
                        }
                        Value::EnumInstanceObj(o) => {
                            match &mut (&mut *o.borrow_mut()).values {
                                None => unreachable!(),
                                Some(values) => {
                                    values[field_idx] = value.clone();
                                }
                            }
                        }
                        _ => unimplemented!("No builtin types have mutable fields")
                    }

                    self.push(value);
                }
                Opcode::MapMk(size) => {
                    let mut items = Vec::with_capacity(size * 2);
                    for _ in 0..size {
                        let value = self.pop_expect()?;
                        let key = self.pop_expect()?;
                        items.push(key);
                        items.push(value);
                    }
                    self.push(Value::new_map_obj(items));
                }
                Opcode::MapLoad => {
                    let key = self.pop_expect()?;
                    let val = match self.pop_expect()? {
                        Value::MapObj(o) => {
                            let map = &*o.borrow();
                            match map._inner.get(&key) {
                                Some(value) => value.clone(),
                                None => Value::Nil
                            }
                        }
                        Value::Nil => Value::Nil,
                        _ => unreachable!()
                    };
                    self.push(val)
                }
                Opcode::MapStore => {
                    let value = self.pop_expect()?;
                    let key = self.pop_expect()?;

                    let map = self.pop_expect()?;
                    match &map {
                        Value::MapObj(o) => {
                            let map = &mut *o.borrow_mut();
                            map._inner.insert(key, value)
                        }
                        _ => unreachable!()
                    };
                    self.push(map);
                }
                Opcode::ArrMk(size) => {
                    let mut items = VecDeque::<Value>::with_capacity(size as usize);
                    for _ in 0..size {
                        items.push_front(self.pop_expect()?);
                    }
                    self.push(Value::new_array_obj(items.into()));
                }
                Opcode::TupleMk(size) => {
                    let mut items = VecDeque::<Value>::with_capacity(size as usize);
                    for _ in 0..size {
                        items.push_front(self.pop_expect()?);
                    }
                    self.push(Value::new_tuple_obj(items.into()));
                }
                Opcode::SetMk(size) => {
                    let mut arr_items = VecDeque::<Value>::with_capacity(size as usize);
                    for _ in 0..size {
                        arr_items.push_front(self.pop_expect()?);
                    }
                    let set_items = arr_items.into_iter().collect();
                    self.push(Value::new_set_obj(set_items));
                }
                Opcode::ArrLoad | Opcode::TupleLoad => {
                    let idx = pop_expect_int!(self)?;
                    let target = self.pop_expect()?;
                    let value = match &target {
                        Value::ArrayObj(o) => {
                            let values = &(*o.borrow())._inner;
                            let len = values.len() as i64;
                            if idx < -len || idx >= len {
                                Value::Nil
                            } else {
                                let idx = if idx < 0 { idx + len } else { idx };
                                values[idx as usize].clone()
                            }
                        }
                        Value::TupleObj(o) => {
                            let values = &*o.borrow();
                            let len = values.len() as i64;
                            if idx < -len || idx >= len {
                                Value::Nil
                            } else {
                                let idx = if idx < 0 { idx + len } else { idx };
                                values[idx as usize].clone()
                            }
                        }
                        Value::StringObj(o) => {
                            let string = &(*o.borrow())._inner;
                            let len = string.len() as i64;
                            let idx = if idx < 0 { idx + len } else { idx };

                            match (*string).chars().nth(idx as usize) {
                                Some(ch) => Value::new_string_obj(ch.to_string()),
                                None => Value::Nil
                            }
                        }
                        Value::Nil => Value::Nil,
                        _ => unreachable!()
                    };
                    self.push(value);
                }
                Opcode::ArrStore | Opcode::TupleStore => {
                    let value = self.pop_expect()?;
                    let idx = pop_expect_int!(self)?;

                    let target = self.pop_expect()?;
                    match &target {
                        Value::ArrayObj(o) => {
                            let values = &mut (*o.borrow_mut())._inner;
                            if idx < 0 {
                                if -(values.len() as i64) <= idx {
                                    let idx: usize = (idx + (values.len() as i64)) as usize;
                                    values[idx] = value;
                                }
                            } else {
                                let idx = idx as usize;
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
                        }
                        Value::TupleObj(o) => {
                            let idx = idx as usize;
                            let values = &mut *o.borrow_mut();
                            if values.len() < (idx as usize) {
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
                    self.push(target);
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

                    let target = self.pop_expect()?;
                    let value = match &target {
                        Value::ArrayObj(o) => {
                            let value = &(*o.borrow())._inner;
                            let (start, len) = get_range_endpoints(value.len(), start, end);
                            let slc = &value[start..(start + len)];
                            Value::new_array_obj(slc.to_vec())
                        }
                        Value::StringObj(o) => {
                            let string = &(*o.borrow())._inner;
                            let (start, len) = get_range_endpoints(string.len(), start, end);
                            let value = (*string).chars().skip(start).take(len).collect::<String>();
                            Value::new_string_obj(value)
                        }
                        _ => unreachable!()
                    };
                    self.push(value);
                }
                Opcode::GStore(global_idx) => {
                    let value = self.pop_expect()?;
                    self.globals[global_idx] = value;
                }
                Opcode::LStore(stack_slot) => self.store_local(stack_slot)?,
                Opcode::UStore(upvalue_idx) => self.store_upvalue(upvalue_idx)?,
                Opcode::GLoad(global_idx) => {
                    self.push(self.globals[global_idx].clone())
                }
                Opcode::LLoad(stack_slot) => self.load_local(stack_slot)?,
                Opcode::ULoad(upvalue_idx) => self.load_upvalue(upvalue_idx)?,
                Opcode::Jump(offset) => {
                    let frame = current_frame!(self);
                    frame.ip += offset;
                }
                Opcode::JumpIfF(offset) => {
                    let cond = self.pop_expect_boolish()?;
                    if !cond {
                        let frame = current_frame!(self);
                        frame.ip += offset;
                    }
                }
                Opcode::JumpB(offset) => {
                    let frame = current_frame!(self);
                    frame.ip -= offset;
                }
                Opcode::Invoke(arity) => {
                    let fn_idx = self.stack.len() - arity - 1;
                    let target = self.stack.remove(fn_idx);

                    if let Value::NativeFn(native_fn) = &target {
                        let value = self.invoke_native_fn(arity, native_fn)?;
                        self.push(value);
                    } else {
                        self.start_call_frame(arity, target)?;
                    }
                }
                Opcode::ClosureMk => self.make_closure()?,
                Opcode::CloseUpvalue => self.close_upvalue()?,
                Opcode::CloseUpvalueAndPop => {
                    self.close_upvalue()?;
                    self.pop_expect()?;
                }
                Opcode::Pop(num_to_pop) => self.pop_expect_n(num_to_pop)?,
                Opcode::Dup => {
                    let value = self.stack.last().unwrap().clone();
                    self.stack.push(value);
                }
                Opcode::MarkLocal(local_idx) => {
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
                        Value::Int(_) => self.load_constant((0, self.type_constant_indexes["prelude/Int"]))?,
                        Value::Float(_) => self.load_constant((0, self.type_constant_indexes["prelude/Float"]))?,
                        Value::Bool(_) => self.load_constant((0, self.type_constant_indexes["prelude/Bool"]))?,
                        Value::StringObj(_) => self.load_constant((0, self.type_constant_indexes["prelude/String"]))?,
                        Value::ArrayObj(_) => self.load_constant((0, self.type_constant_indexes["prelude/Array"]))?,
                        Value::SetObj(_) => self.load_constant((0, self.type_constant_indexes["prelude/Set"]))?,
                        Value::MapObj(_) => self.load_constant((0, self.type_constant_indexes["prelude/Map"]))?,
                        Value::InstanceObj(o) => {
                            let i = &*o.borrow();
                            let type_value = self.load_type(i.type_id).clone();
                            self.push(Value::Type(type_value))
                        }
                        Value::NativeInstanceObj(o) => {
                            let i = &*o.borrow();
                            let type_value = self.load_type(i.type_id).clone();
                            self.push(Value::Type(type_value))
                        }
                        Value::EnumInstanceObj(o) => {
                            let i = &*o.borrow();
                            let enum_value = self.load_enum(i.type_id).clone();
                            self.push(Value::Enum(enum_value))
                        }
                        Value::Nil => self.push(Value::Nil),
                        Value::TupleObj(_) |
                        Value::Fn(_) |
                        Value::Closure(_) |
                        Value::NativeFn(_) |
                        Value::Type(_) |
                        Value::Enum(_) |
                        Value::Module(_) => unimplemented!(),
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

                    if is_complete {
                        debug_assert!(if init_call_stack_depth == 1 { self.stack.is_empty() } else { true });

                        let top = top.unwrap_or(Value::Nil);
                        break Ok(top);
                    }
                }
            }
        }
    }
}