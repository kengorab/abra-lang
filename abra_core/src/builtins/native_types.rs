use crate::typechecker::types::Type;
use crate::vm::value::{Value, Obj};
use crate::builtins::gen_native_types::{NativeStringMethodsAndFields, NativeArrayMethodsAndFields, NativeFloatMethodsAndFields, NativeIntMethodsAndFields, NativeMapMethodsAndFields};
use crate::vm::vm::VM;
use std::collections::{HashSet, HashMap};

macro_rules! obj_as_string {
    ($obj:expr) => {
        if let Value::Obj(obj) = $obj.unwrap() {
            if let Obj::StringObj(value) = &*(obj.borrow()) {
                value.clone()
            } else { unreachable!() }
        } else { unreachable!() }
    };
}

pub trait NativeType {
    fn get_field_or_method(name: &str) -> Option<(usize, Type)>;
    fn get_field_value(obj: Box<Value>, field_idx: usize) -> Value;

    fn get_field_idx(field_name: &str) -> usize {
        match Self::get_field_or_method(field_name) {
            Some((idx, _)) => idx,
            None => unreachable!()
        }
    }
}

pub type NativeInt = crate::builtins::gen_native_types::NativeInt;

impl NativeIntMethodsAndFields for crate::builtins::gen_native_types::NativeInt {
    fn method_abs(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Int(i) = receiver.unwrap() {
            Some(Value::Int(i.abs()))
        } else { unimplemented!() }
    }

    fn method_as_base(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let base = args.into_iter().next().expect("Int::asBase requires 1 argument");
        let base = if let Value::Int(base) = base { base } else { unreachable!() };

        if let Value::Int(i) = receiver.unwrap() {
            if base <= 1 || base >= 37 || i <= 0 {
                return Some(Value::new_string_obj(i.to_string()));
            }

            let base = base as u32;
            let mut i = i as u32;
            let mut digits = Vec::new();
            while i > 0 {
                if let Some(ch) = std::char::from_digit(i % base, base) {
                    digits.push(ch);
                }

                i = i / base;
            }

            let str_val = digits.into_iter().rev().collect::<String>();
            Some(Value::new_string_obj(str_val))
        } else { unimplemented!() }
    }

    fn method_is_even(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Int(i) = receiver.unwrap() {
            Some(Value::Bool(i % 2 == 0))
        } else { unreachable!() }
    }

    fn method_is_odd(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Int(i) = receiver.unwrap() {
            Some(Value::Bool(i % 2 != 0))
        } else { unreachable!() }
    }
}

pub type NativeFloat = crate::builtins::gen_native_types::NativeFloat;

impl NativeFloatMethodsAndFields for crate::builtins::gen_native_types::NativeFloat {
    fn method_floor(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Float(f) = receiver.unwrap() {
            Some(Value::Int(f.floor() as i64))
        } else { unreachable!() }
    }

    fn method_ceil(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Float(f) = receiver.unwrap() {
            Some(Value::Int(f.ceil() as i64))
        } else { unreachable!() }
    }

    fn method_round(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Float(f) = receiver.unwrap() {
            Some(Value::Int(f.round() as i64))
        } else { unreachable!() }
    }

    fn method_with_precision(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let precision = args.into_iter().next().expect("Float::withPrecision requires 1 argument");
        let precision = if let Value::Int(precision) = precision { precision } else { unreachable!() };

        if precision < 0 {
            return receiver;
        } else if precision >= 10 {
            return receiver;
        }

        if let Value::Float(f) = receiver.unwrap() {
            let power = 10_i32.pow(precision as u32);
            let i = (f * (power as f64)).trunc();

            Some(Value::Float(i / (power as f64)))
        } else { unreachable!() }
    }

    fn method_abs(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Float(f) = receiver.unwrap() {
            Some(Value::Float(f.abs()))
        } else { unreachable!() }
    }
}

pub type NativeString = crate::builtins::gen_native_types::NativeString;

impl NativeStringMethodsAndFields for crate::builtins::gen_native_types::NativeString {
    fn field_length(obj: Box<Value>) -> Value {
        if let Value::Obj(obj) = *obj {
            match &*(obj.borrow()) {
                Obj::StringObj(value) => Value::Int(value.len() as i64),
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_to_lower(receiver: Option<Value>, _: Vec<Value>, _: &mut VM) -> Option<Value> {
        let receiver = obj_as_string!(receiver);
        Some(Value::new_string_obj(receiver.to_lowercase()))
    }

    fn method_to_upper(receiver: Option<Value>, _: Vec<Value>, _: &mut VM) -> Option<Value> {
        let receiver = obj_as_string!(receiver);
        Some(Value::new_string_obj(receiver.to_uppercase()))
    }

    fn method_pad_left(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let mut args = args.into_iter();

        let amount = args.next().expect("String::padLeft requires 2 arguments");
        let amount = if let Value::Int(amount) = amount { amount } else { unreachable!() };

        if amount <= 0 {
            return receiver;
        }

        let receiver = obj_as_string!(receiver);
        if receiver.len() >= (amount as usize) {
            return Some(Value::new_string_obj(receiver));
        }

        let padding = args.next().expect("String::padLeft requires 2 arguments");
        let padding = if let Value::Obj(obj) = padding {
            match &*(obj.borrow()) {
                Obj::StringObj(value) => value.clone(),
                _ => unreachable!()
            }
        } else { unreachable!() };

        let num_repeats = ((amount as usize) - receiver.len()) / padding.len();
        let padding = padding.repeat(num_repeats);

        Some(Value::new_string_obj(format!("{}{}", padding, receiver)))
    }

    fn method_trim(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let receiver = obj_as_string!(receiver);
        Some(Value::new_string_obj(receiver.trim().to_string()))
    }

    fn method_trim_start(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let trim_pattern = match args.into_iter().next() {
            Some(Value::Obj(obj)) => match &*obj.borrow() {
                Obj::StringObj(value) => Some(value.clone()),
                _ => unreachable!()
            },
            _ => None
        };

        let receiver = obj_as_string!(receiver);
        let new_val = if let Some(trim_pattern) = trim_pattern {
            receiver.trim_start_matches(&trim_pattern)
        } else {
            receiver.trim_start()
        };
        Some(Value::new_string_obj(new_val.to_string()))
    }

    fn method_trim_end(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let trim_pattern = match args.into_iter().next() {
            Some(Value::Obj(obj)) => match &*obj.borrow() {
                Obj::StringObj(value) => Some(value.clone()),
                _ => unreachable!()
            },
            _ => None
        };

        let receiver = obj_as_string!(receiver);

        let new_val = if let Some(trim_pattern) = trim_pattern {
            receiver.trim_end_matches(&trim_pattern)
        } else {
            receiver.trim_end()
        };
        Some(Value::new_string_obj(new_val.to_string()))
    }

    fn method_split(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let splitter = args.into_iter().next().expect("String::split requires 1 argument");
        let splitter = match splitter {
            Value::Obj(obj) => match &*obj.borrow() {
                Obj::StringObj(value) => value.clone(),
                _ => unreachable!()
            },
            _ => unreachable!()
        };

        let receiver = obj_as_string!(receiver);
        let items = if splitter.is_empty() {
            receiver.chars()
                .map(|s| Value::new_string_obj(s.to_string()))
                .collect()
        } else {
            receiver.split(&splitter)
                .map(|s| Value::new_string_obj(s.to_string()))
                .collect()
        };

        Some(Value::new_array_obj(items))
    }

    fn method_lines(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let receiver = obj_as_string!(receiver);

        let items = receiver.lines()
            .map(|s| Value::new_string_obj(s.to_string()))
            .collect();
        Some(Value::new_array_obj(items))
    }

    fn method_chars(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let receiver = obj_as_string!(receiver);
        let items = receiver.chars()
            .map(|s| Value::new_string_obj(s.to_string()))
            .collect();
        Some(Value::new_array_obj(items))
    }

    fn method_parse_int(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let radix = match args.into_iter().next() {
            Some(Value::Int(radix)) => radix,
            _ => 10
        };
        let receiver = obj_as_string!(receiver);
        match i64::from_str_radix(&receiver, radix as u32) {
            Ok(i) => Some(Value::Int(i)),
            Err(_) => Some(Value::Nil)
        }
    }

    fn method_parse_float(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let receiver = obj_as_string!(receiver);
        match receiver.parse::<f64>() {
            Ok(f) => Some(Value::Float(f)),
            Err(_) => Some(Value::Nil)
        }
    }
}

pub type NativeArray = crate::builtins::gen_native_types::NativeArray;

impl NativeArrayMethodsAndFields for crate::builtins::gen_native_types::NativeArray {
    fn field_length(obj: Box<Value>) -> Value {
        if let Value::Obj(obj) = *obj {
            match &*(obj.borrow()) {
                Obj::ArrayObj(value) => Value::Int(value.len() as i64),
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_push(receiver: Option<Value>, args: Vec<Value>, _: &mut VM) -> Option<Value> {
        let item = args.into_iter().next().expect("Array::push requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match *obj.borrow_mut() {
                Obj::ArrayObj(ref mut array) => array.push(item),
                _ => unreachable!()
            }
        } else { unreachable!() }
        None
    }

    fn method_concat(receiver: Option<Value>, args: Vec<Value>, _: &mut VM) -> Option<Value> {
        let arg = args.into_iter().next().expect("Array::concat requires 1 argument");
        let mut other_arr = match arg {
            Value::Obj(obj) => match &*obj.borrow() {
                Obj::ArrayObj(other_arr) => other_arr.clone(),
                _ => unreachable!()
            },
            _ => unreachable!()
        };

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut new_arr = Vec::with_capacity(array.len() + other_arr.len());
                    let mut old_arr = array.clone();
                    new_arr.append(&mut old_arr);
                    new_arr.append(&mut other_arr);
                    Some(Value::new_array_obj(new_arr))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_map(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match *obj.borrow() {
                Obj::ArrayObj(ref array) => {
                    let callback = args.into_iter().next().expect("Array::map requires 1 argument");

                    let mut new_array_items = Vec::new();

                    for value in array {
                        let args = vec![value.clone()];
                        let new_value = vm.invoke_fn(args, callback.clone())
                            .unwrap_or(Some(Value::Nil))
                            .unwrap_or(Value::Nil);
                        new_array_items.push(new_value);
                    }

                    Some(Value::new_array_obj(new_array_items))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_filter(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let callback = args.into_iter().next().expect("Array::filter requires 1 argument");

                    let mut new_array_items = Vec::new();

                    for value in array {
                        let args = vec![value.clone()];
                        let new_value = vm.invoke_fn(args, callback.clone())
                            .unwrap_or(Some(Value::Bool(false)))
                            .unwrap_or(Value::Bool(false));
                        if let Value::Bool(true) = new_value {
                            new_array_items.push(value.clone());
                        }
                    }

                    Some(Value::new_array_obj(new_array_items))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_reduce(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut args = args.into_iter();
                    let initial_value = args.next().expect("Array::reduce requires 2 arguments");
                    let callback = args.next().expect("Array::reduce requires 2 arguments");

                    let mut accumulator = initial_value;

                    for value in array {
                        let args = vec![accumulator, value.clone()];
                        let new_value = vm.invoke_fn(args, callback.clone())
                            .unwrap_or(Some(Value::Nil))
                            .unwrap_or(Value::Nil);
                        accumulator = new_value;
                    }

                    Some(accumulator)
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_join(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let joiner = args.into_iter().next().expect("Array::join requires 1 argument");
        let joiner = if let Value::Obj(obj) = joiner {
            match &(*obj.borrow()) {
                Obj::StringObj(s) => s.clone(),
                _ => unreachable!()
            }
        } else if let Value::Nil = joiner {
            "".to_string()
        } else { unreachable!() };

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let joined = array.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(joiner.as_str());
                    Some(Value::new_string_obj(joined))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_contains(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let item = args.into_iter().next().expect("Array::contains requires 1 argument");
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let v = array.contains(&item);
                    Some(Value::Bool(v))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_find(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Array::find requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut iter = array.iter();
                    let return_value = loop {
                        match iter.next() {
                            None => break Value::Nil,
                            Some(value) => {
                                let args = vec![value.clone()];
                                let ret_val = vm.invoke_fn(args, callback.clone())
                                    .unwrap_or(Some(Value::Nil))
                                    .unwrap_or(Value::Nil);
                                match ret_val {
                                    Value::Bool(false) | Value::Nil => continue,
                                    _ => break value.clone()
                                }
                            }
                        }
                    };
                    Some(return_value)
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_any(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Array::any requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut iter = array.iter();
                    let return_value = loop {
                        match iter.next() {
                            None => break Value::Bool(false),
                            Some(value) => {
                                let args = vec![value.clone()];
                                let ret_val = vm.invoke_fn(args, callback.clone())
                                    .unwrap_or(Some(Value::Nil))
                                    .unwrap_or(Value::Nil);
                                match ret_val {
                                    Value::Bool(false) | Value::Nil => continue,
                                    _ => break Value::Bool(true)
                                }
                            }
                        }
                    };
                    Some(return_value)
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_all(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Array::all requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut iter = array.iter();
                    let return_value = loop {
                        match iter.next() {
                            None => break Value::Bool(true),
                            Some(value) => {
                                let args = vec![value.clone()];
                                let ret_val = vm.invoke_fn(args, callback.clone())
                                    .unwrap_or(Some(Value::Nil))
                                    .unwrap_or(Value::Nil);
                                match ret_val {
                                    Value::Bool(false) | Value::Nil => break Value::Bool(false),
                                    _ => continue
                                }
                            }
                        }
                    };
                    Some(return_value)
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_none(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Array::none requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut iter = array.iter();
                    let return_value = loop {
                        match iter.next() {
                            None => break Value::Bool(true),
                            Some(value) => {
                                let args = vec![value.clone()];
                                let ret_val = vm.invoke_fn(args, callback.clone())
                                    .unwrap_or(Some(Value::Nil))
                                    .unwrap_or(Value::Nil);
                                match ret_val {
                                    Value::Bool(false) | Value::Nil => continue,
                                    _ => break Value::Bool(false)
                                }
                            }
                        }
                    };
                    Some(return_value)
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_sort_by(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let mut args = args.into_iter();

        let callback = args.next().expect("Array::sortBy requires 2 arguments");

        let reverse = args.next().expect("Array::sortBy requires 2 arguments");
        let reverse = if let Value::Bool(b) = reverse { b } else { false };

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow_mut()) {
                Obj::ArrayObj(array) => {
                    let mut sort_values = array.iter().enumerate().map(|(idx, item)| {
                        let args = vec![item.clone()];
                        let v = vm.invoke_fn(args, callback.clone())
                            .unwrap_or(Some(Value::Nil))
                            .unwrap_or(Value::Nil);
                        (v, idx)
                    }).collect::<Vec<_>>();
                    sort_values.sort_by(|v1, v2| {
                        match (&v1.0, &v2.0) {
                            (Value::Int(i1), Value::Int(i2)) => {
                                if reverse {
                                    i2.cmp(&i1)
                                } else {
                                    i1.cmp(&i2)
                                }
                            }
                            _ => unreachable!()
                        }
                    });
                    let items = sort_values.iter()
                        .map(|(_, idx)| array[*idx].clone())
                        .collect();
                    Some(Value::new_array_obj(items))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_dedupe(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut new_array_items = vec![];
                    let mut seen = HashSet::new();

                    for item in array {
                        if seen.contains(item) {
                            continue;
                        }
                        seen.insert(item);
                        new_array_items.push(item.clone())
                    }

                    Some(Value::new_array_obj(new_array_items))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_dedupe_by(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Array::dedupeBy requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut new_array_items = vec![];
                    let mut seen = HashSet::new();

                    for item in array {
                        let args = vec![item.clone()];
                        let v = vm.invoke_fn(args, callback.clone())
                            .unwrap_or(Some(Value::Nil))
                            .unwrap_or(Value::Nil);

                        if seen.contains(&v) {
                            continue;
                        }
                        seen.insert(v);
                        new_array_items.push(item.clone())
                    }

                    Some(Value::new_array_obj(new_array_items))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_partition(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Array::partition requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut map = HashMap::new();

                    for item in array {
                        let args = vec![item.clone()];
                        let v = vm.invoke_fn(args, callback.clone())
                            .unwrap_or(Some(Value::Nil))
                            .unwrap_or(Value::Nil);

                        map.entry(v).or_insert(vec![]).push(item.clone());
                    }

                    let map = map.into_iter()
                        .map(|(k, v)| (k, Value::new_array_obj(v)))
                        .collect();
                    Some(Value::new_map_obj(map))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_tally(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut map = HashMap::new();

                    for item in array {
                        *map.entry(item.clone()).or_insert(0) += 1;
                    }

                    let map = map.into_iter()
                        .map(|(k, v)| (k, Value::Int(v)))
                        .collect();
                    Some(Value::new_map_obj(map))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_tally_by(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Array::partition requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::ArrayObj(array) => {
                    let mut map = HashMap::new();

                    for item in array {
                        let args = vec![item.clone()];
                        let v = vm.invoke_fn(args, callback.clone())
                            .unwrap_or(Some(Value::Nil))
                            .unwrap_or(Value::Nil);

                        *map.entry(v).or_insert(0) += 1;
                    }

                    let map = map.into_iter()
                        .map(|(k, v)| (k, Value::Int(v)))
                        .collect();
                    Some(Value::new_map_obj(map))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }
}

pub type NativeMap = crate::builtins::gen_native_types::NativeMap;

impl NativeMapMethodsAndFields for NativeMap {
    fn field_size(obj: Box<Value>) -> Value {
        if let Value::Obj(obj) = *obj {
            match &*(obj.borrow()) {
                Obj::MapObj(value) => Value::Int(value.len() as i64),
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_keys(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::MapObj(map) => {
                    let keys = map.keys()
                        .map(|k| k.clone())
                        .collect::<Vec<_>>();
                    Some(Value::new_array_obj(keys))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_values(receiver: Option<Value>, _args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::MapObj(map) => {
                    let values = map.values()
                        .map(|v| v.clone())
                        .collect::<Vec<_>>();
                    Some(Value::new_array_obj(values))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_contains_key(receiver: Option<Value>, args: Vec<Value>, _vm: &mut VM) -> Option<Value> {
        let key = obj_as_string!(args.into_iter().next());

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::MapObj(map) => {
                    Some(Value::Bool(map.contains_key(&Value::new_string_obj(key))))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }

    fn method_map_values(receiver: Option<Value>, args: Vec<Value>, vm: &mut VM) -> Option<Value> {
        let callback = args.into_iter().next().expect("Map::mapValues requires 1 argument");

        if let Value::Obj(obj) = receiver.unwrap() {
            match &*(obj.borrow()) {
                Obj::MapObj(map) => {
                    let mut new_map = HashMap::new();

                    for (k, v) in map {
                        let args = vec![k.clone(), v.clone()];
                        let v = vm.invoke_fn(args, callback.clone())
                            .unwrap_or(Some(Value::Nil))
                            .unwrap_or(Value::Nil);

                        new_map.insert(k.clone(), v);
                    }

                    Some(Value::new_map_obj(new_map))
                }
                _ => unreachable!()
            }
        } else { unreachable!() }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::lexer::tokenize;
    use crate::parser::parser::parse;
    use crate::typechecker::typechecker::typecheck;
    use crate::vm::compiler::compile;
    use crate::vm::value::Value;
    use crate::vm::vm::{VM, VMContext};

    fn new_string_obj(string: &str) -> Value {
        Value::new_string_obj(string.to_string())
    }

    macro_rules! array {
        ($($i:expr),*) => { Value::new_array_obj(vec![$($i),*]) };
    }

    macro_rules! int_array {
        ($($i:expr),*) => { Value::new_array_obj(vec![$($i),*].into_iter().map(Value::Int).collect()) };
    }

    macro_rules! string_array {
        ($($i:expr),*) => { Value::new_array_obj(vec![$($i),*].into_iter().map(new_string_obj).collect()) };
    }

    fn interpret(input: &str) -> Option<Value> {
        let tokens = tokenize(&input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        let (_, typed_ast) = typecheck(ast).unwrap();
        let (module, _) = compile(typed_ast).unwrap();

        let mut vm = VM::new(module, VMContext::default());
        vm.run().unwrap()
    }

    #[test]
    fn test_string_length() {
        let result = interpret("\"asdf qwer\".length");
        let expected = Value::Int(9);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_to_upper() {
        let result = interpret("\"Asdf Qwer\".toUpper()");
        let expected = new_string_obj("ASDF QWER");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_to_lower() {
        let result = interpret("\"aSDF qWER\".toLower()");
        let expected = new_string_obj("asdf qwer");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_pad_left() {
        let result = interpret("\"asdf\".padLeft(7, \"!\")");
        let expected = new_string_obj("!!!asdf");
        assert_eq!(Some(expected), result);

        let result = interpret("\"asdf\".padLeft(4, \"!\")");
        let expected = new_string_obj("asdf");
        assert_eq!(Some(expected), result);

        let result = interpret("\"asdf\".padLeft(-14, \"!\")");
        let expected = new_string_obj("asdf");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_trim() {
        let result = interpret("\"  asdf   \".trim()");
        let expected = new_string_obj("asdf");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_trim_start() {
        let result = interpret("\"  asdf   \".trimStart()");
        let expected = new_string_obj("asdf   ");
        assert_eq!(Some(expected), result);

        let result = interpret("\"!!asdf   \".trimStart(pattern: \"!\")");
        let expected = new_string_obj("asdf   ");
        assert_eq!(Some(expected), result);

        let result = interpret("\"!!!asdf   \".trimStart(\"!!\")");
        let expected = new_string_obj("!asdf   ");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_trim_end() {
        let result = interpret("\"  asdf   \".trimEnd()");
        let expected = new_string_obj("  asdf");
        assert_eq!(Some(expected), result);

        let result = interpret("\"  asdf!!\".trimEnd(pattern: \"!\")");
        let expected = new_string_obj("  asdf");
        assert_eq!(Some(expected), result);

        let result = interpret("\"  asdf!!!\".trimEnd(\"!!\")");
        let expected = new_string_obj("  asdf!");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_split() {
        let result = interpret("\"a s d f\".split(splitter: \" \")");
        let expected = array![
          new_string_obj("a"),
          new_string_obj("s"),
          new_string_obj("d"),
          new_string_obj("f")
        ];
        assert_eq!(Some(expected), result);

        let result = interpret("\"  a  b  c d\".split(\"  \")");
        let expected = array![
          new_string_obj(""),
          new_string_obj("a"),
          new_string_obj("b"),
          new_string_obj("c d")
        ];
        assert_eq!(Some(expected), result);

        let result = interpret("\"asdf\".split(\"qwer\")");
        let expected = array![
          new_string_obj("asdf")
        ];
        assert_eq!(Some(expected), result);

        let result = interpret("\"asdf\".split(\"\")");
        let expected = array![
          new_string_obj("a"),
          new_string_obj("s"),
          new_string_obj("d"),
          new_string_obj("f")
        ];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_lines() {
        // let result = interpret("\"asdf\\nqwer\\nzxcv\".lines()");
        // let expected = array![
        //   new_string_obj("asdf"),
        //   new_string_obj("qwer"),
        //   new_string_obj("zxcv")
        // ];
        // assert_eq!(Some(expected), result);
        // TODO: Can't really test this until escape sequences are implemented...
    }

    #[test]
    fn test_string_chars() {
        let result = interpret("\"asdf\".chars()");
        let expected = array![
          new_string_obj("a"),
          new_string_obj("s"),
          new_string_obj("d"),
          new_string_obj("f")
        ];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_parse_int() {
        let result = interpret("\"hello\".parseInt()");
        let expected = Value::Nil;
        assert_eq!(Some(expected), result);

        let result = interpret("\"123 456\".parseInt()");
        let expected = Value::Nil;
        assert_eq!(Some(expected), result);

        let result = interpret("\"123456.7\".parseInt()");
        let expected = Value::Nil;
        assert_eq!(Some(expected), result);

        let result = interpret("\"123456\".parseInt()");
        let expected = Value::Int(123456);
        assert_eq!(Some(expected), result);

        let result = interpret("\"-123456\".parseInt()");
        let expected = Value::Int(-123456);
        assert_eq!(Some(expected), result);

        let result = interpret("\"ba55\".parseInt(radix: 16)");
        let expected = Value::Int(47701);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_string_parse_float() {
        let result = interpret("\"hello\".parseFloat()");
        let expected = Value::Nil;
        assert_eq!(Some(expected), result);

        let result = interpret("\"123 456\".parseFloat()");
        let expected = Value::Nil;
        assert_eq!(Some(expected), result);

        let result = interpret("\"123456.7\".parseFloat()");
        let expected = Value::Float(123456.7);
        assert_eq!(Some(expected), result);

        let result = interpret("\"-123456.7\".parseFloat()");
        let expected = Value::Float(-123456.7);
        assert_eq!(Some(expected), result);

        let result = interpret("\"123456\".parseFloat()");
        let expected = Value::Float(123456.0);
        assert_eq!(Some(expected), result);

        let result = interpret("\"-123456\".parseFloat()");
        let expected = Value::Float(-123456.0);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_length() {
        let result = interpret("[1, 2, 3, 4, 5].length");
        let expected = Value::Int(5);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_push() {
        let result = interpret(r#"
          val arr = [1, 2, 3]
          arr.push(4)
          arr.push(5)
          arr
        "#);
        let expected = int_array!(1, 2, 3, 4, 5);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_concat() {
        let result = interpret(r#"
          val arr1 = [1, 2, 3]
          val arr2 = [4, 5, 6]
          arr1.concat(arr2)
        "#);
        let expected = int_array![1, 2, 3, 4, 5, 6];
        assert_eq!(Some(expected), result);

        // Verify that the original arrays aren't modified
        let result = interpret(r#"
          val arr1 = [1, 2, 3]
          val arr2 = [4, 5, 6]
          val arr3 = arr1.concat(arr2)
          [arr1, arr2]
        "#);
        let expected = array![
            int_array![1, 2, 3],
            int_array![4, 5, 6]
        ];
        assert_eq!(Some(expected), result);

        // Verify that the arrays' items are copied by reference
        let result = interpret(r#"
          type Counter {
            count: Int = 0
            func inc(self): Int { self.count += 1 }
          }

          val arr1 = [Counter(), Counter()]
          val arr2 = [Counter(), Counter()]
          val arr3 = arr1.concat(arr2)

          if arr1[0] |c| c.inc()
          if arr2[1] |c| c.inc()

          [
            arr1.map(c => c.count),
            arr2.map(c => c.count),
            arr3.map(c => c.count)
          ]
        "#);
        let expected = array![
            int_array![1, 0],
            int_array![0, 1],
            int_array![1, 0, 0, 1]
        ];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_map() {
        let result = interpret(r#"
          val arr = [1, 2, 3, 4]
          arr.map(i => i * 3)
        "#);
        let expected = int_array![3, 6, 9, 12];
        assert_eq!(Some(expected), result);

        // Verify closures work
        // TODO: See #172
        let result = interpret(r#"
          var total = 0
          val arr = [1, 2, 3, 4]
          val arr2 = arr.map(i => {
            total += i
            i * 3
          })
          arr2.concat([total])
        "#);
        let expected = int_array![3, 6, 9, 12, 10];
        assert_eq!(Some(expected), result);

        // Verify deep call stack initiated from native fn call
        let result = interpret(r#"
          func mult1(a: Int) = a * 1
          func sub1(a: Int) = mult1(a) - 1
          func sameNum(a: Int) = sub1(a) + 1
          [1, 2].map(i => sameNum(i))
        "#);
        let expected = int_array![1, 2];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_filter() {
        let result = interpret(r#"
          val arr = ["a", "bc", "def", "ghij", "klmno"]
          arr.filter(w => w.length < 4)
        "#);
        let expected = string_array!["a", "bc", "def"];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_reduce() {
        let result = interpret(r#"
          val arr = [1, 2, 3, 4, 5]
          arr.reduce(0, (acc, i) => acc + i)
        "#);
        let expected = Value::Int(15);
        assert_eq!(Some(expected), result);

        let result = interpret(r#"
          val arr = [1, 2, 3, 4, 5]
          arr.reduce("", (acc, i) => acc + i)
        "#);
        let expected = new_string_obj("12345");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_join() {
        let result = interpret("[1, 2, 3, 4, 5].join()");
        let expected = new_string_obj("12345");
        assert_eq!(Some(expected), result);

        let result = interpret("[\"a\", \"b\", \"c\"].join(\", \")");
        let expected = new_string_obj("a, b, c");
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_contains() {
        let result = interpret("[1, 2, 3, 4, 5].contains(5)");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("[1, 2, 3, 4].contains(6)");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_find() {
        let result = interpret("[1, 2, 3].find(x => x >= 2)");
        let expected = Value::Int(2);
        assert_eq!(Some(expected), result);

        let result = interpret("[[1, 2], [3, 4]].find(p => p[0])");
        let expected = array![Value::Int(1), Value::Int(2)];
        assert_eq!(Some(expected), result);

        let result = interpret("[[1, 2], [3, 4]].find(p => if p[0] |f| f >= 2)");
        let expected = array![Value::Int(3), Value::Int(4)];
        assert_eq!(Some(expected), result);

        let result = interpret("[1, 2, 3].find(x => x >= 4)");
        let expected = Value::Nil;
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_any() {
        let result = interpret("[1, 2, 3, 4, 5].any(x => x > 4)");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("[1, 2, 3, 4, 5].any(x => x < 0)");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);

        let result = interpret("[[1, 2], [3, 4]].any(p => if p[0] |f| f >= 2)");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_all() {
        let result = interpret("[\"a\", \"bc\", \"def\"].all(w => w.length > 0)");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("[\"a\", \"bc\", \"def\"].all(w => w.length < 3)");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);

        let result = interpret("[\"1\", \"2\", \"24\"].all(w => w.parseInt())");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("[\"a\"].all(w => w.parseInt())");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_none() {
        let result = interpret("[\"a\", \"bc\", \"def\"].none(w => w.length > 0)");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);

        let result = interpret("[\"a\", \"bc\", \"def\"].none(w => w.length < 0)");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);

        let result = interpret("[\"1\", \"2\", \"24\"].none(w => w.parseInt())");
        let expected = Value::Bool(false);
        assert_eq!(Some(expected), result);

        let result = interpret("[\"a\", \"b\"].none(w => w.parseInt())");
        let expected = Value::Bool(true);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_array_sort_by() {
        let result = interpret(r#"
          type Person { name: String }
          val people = [
            Person(name: "Ken"),
            Person(name: "Meghan"),
            Person(name: "Brian"),
            Person(name: "Kelsey"),
          ]
          people.sortBy(p => p.name.length).map(p => p.name)
        "#);
        let expected = array![
          new_string_obj("Ken"),
          new_string_obj("Brian"),
          new_string_obj("Meghan"),
          new_string_obj("Kelsey")
        ];
        assert_eq!(Some(expected), result);

        let result = interpret("\
          [1, 8, 3, 6, 1, 11, 5839, 6].sortBy(fn: i => i, reverse: true)
        ");
        let expected = array![
            Value::Int(5839),
            Value::Int(11),
            Value::Int(8),
            Value::Int(6),
            Value::Int(6),
            Value::Int(3),
            Value::Int(1),
            Value::Int(1)
        ];
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_float_floor() {
        let result = interpret("6.24.floor()");
        let expected = Value::Int(6);
        assert_eq!(Some(expected), result);

        let result = interpret("val f = 6.7\nf.floor()");
        let expected = Value::Int(6);
        assert_eq!(Some(expected), result);

        let result = interpret("val f = -6.7\nf.floor()");
        let expected = Value::Int(-7);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_float_ceil() {
        let result = interpret("6.24.ceil()");
        let expected = Value::Int(7);
        assert_eq!(Some(expected), result);

        let result = interpret("val f = 6.7\nf.ceil()");
        let expected = Value::Int(7);
        assert_eq!(Some(expected), result);

        let result = interpret("val f = -6.7\nf.ceil()");
        let expected = Value::Int(-6);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_float_round() {
        let result = interpret("6.24.round()");
        let expected = Value::Int(6);
        assert_eq!(Some(expected), result);

        let result = interpret("6.75.round()");
        let expected = Value::Int(7);
        assert_eq!(Some(expected), result);

        let result = interpret("(-6.455).round()");
        let expected = Value::Int(-6);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_float_with_precision() {
        let result = interpret("6.12345.withPrecision(0)");
        let expected = Value::Float(6.0);
        assert_eq!(Some(expected), result);

        let result = interpret("6.98765.withPrecision(0)");
        let expected = Value::Float(6.0);
        assert_eq!(Some(expected), result);

        let result = interpret("6.98765.withPrecision(-1)");
        let expected = Value::Float(6.98765);
        assert_eq!(Some(expected), result);

        let result = interpret("1.23456.withPrecision(2)");
        let expected = Value::Float(1.23);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_float_abs() {
        let result = interpret("6.24.abs()");
        let expected = Value::Float(6.24);
        assert_eq!(Some(expected), result);

        let result = interpret("(-6.24).abs()");
        let expected = Value::Float(6.24);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_int_abs() {
        let result = interpret("6.abs()");
        let expected = Value::Int(6);
        assert_eq!(Some(expected), result);

        let result = interpret("(-6).abs()");
        let expected = Value::Int(6);
        assert_eq!(Some(expected), result);
    }

    #[test]
    fn test_as_base() {
        let result = interpret("6.asBase(0)");
        let expected = new_string_obj("6");
        assert_eq!(Some(expected), result);

        let result = interpret("6.asBase(1)");
        let expected = new_string_obj("6");
        assert_eq!(Some(expected), result);

        let result = interpret("6.asBase(37)");
        let expected = new_string_obj("6");
        assert_eq!(Some(expected), result);

        let result = interpret("6.asBase(10)");
        let expected = new_string_obj("6");
        assert_eq!(Some(expected), result);

        let result = interpret("24.asBase(8)");
        let expected = new_string_obj("30");
        assert_eq!(Some(expected), result);

        let result = interpret("4040.asBase(16)");
        let expected = new_string_obj("fc8");
        assert_eq!(Some(expected), result);

        let result = interpret("20.asBase(17)");
        let expected = new_string_obj("13");
        assert_eq!(Some(expected), result);

        let result = interpret("24032.asBase(36)");
        let expected = new_string_obj("ijk");
        assert_eq!(Some(expected), result);
    }
}
