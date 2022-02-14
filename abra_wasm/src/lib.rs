extern crate serde;
extern crate serde_derive;
extern crate serde_json;
extern crate js_sys;
extern crate wasm_bindgen;

use serde::ser::{SerializeMap, Serializer, SerializeTuple};
use serde::Serialize;
use wasm_bindgen::prelude::*;
use abra_core::{Error, typecheck, compile, compile_and_disassemble};
use abra_core::vm::value::Value;
use abra_core::vm::vm::{VMContext, VM};
use abra_core::common::display_error::DisplayError;
use abra_core::parser::ast::{ModuleId, ModulePathSegment};
use abra_core::module_loader::{ModuleReader, ModuleLoader};
use abra_core::builtins::common::to_string;
use abra_core::lexer::tokens::{Position, Range};

struct WasmPosition<'a>(&'a Position);
impl<'a> Serialize for WasmPosition<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let Position { line, col } = &self.0;
        let mut arr = serializer.serialize_tuple(2)?;
        arr.serialize_element(&line)?;
        arr.serialize_element(&col)?;
        arr.end()
    }
}

struct WasmRange(Range);
impl Serialize for WasmRange {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let Range { start, end } = &self.0;

        let mut obj = serializer.serialize_map(Some(2))?;

        obj.serialize_entry("start", &WasmPosition(start))?;
        obj.serialize_entry("end", &WasmPosition(end))?;

        obj.end()
    }
}

struct WasmError<'a>(&'a Error, &'a String);
impl<'a> Serialize for WasmError<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let err = self.0;
        let src = self.1;

        let mut obj = serializer.serialize_map(Some(3))?;

        obj.serialize_entry("success", &false)?;

        let file_name = format!("{}.abra", &err.module_id().get_path(""));
        obj.serialize_entry("errorMessage", &err.get_message(&file_name, &src))?;

        obj.serialize_entry("range", &WasmRange(err.get_range()))?;

        obj.end()
    }
}

pub struct RunResult(Result<(Value, String), Error>, String);
impl Serialize for RunResult {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match &self.0 {
            Ok((_, to_str_value)) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("success", &true)?;
                obj.serialize_entry("dataToString", to_str_value)?;
                obj.end()
            }
            Err(error) => WasmError(error, &self.1).serialize(serializer)
        }
    }
}

pub struct TypecheckedResult(Result<(), Error>, String);
impl Serialize for TypecheckedResult {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match &self.0 {
            Ok(_) => {
                let mut obj = serializer.serialize_map(Some(1))?;
                obj.serialize_entry("success", &true)?;
                obj.end()
            }
            Err(error) => WasmError(error, &self.1).serialize(serializer)
        }
    }
}

pub struct DisassembleResult(Result<String, Error>, String);

impl Serialize for DisassembleResult {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match &self.0 {
            Ok(result) => {
                let mut obj = serializer.serialize_map(Some(2))?;
                obj.serialize_entry("success", &true)?;
                obj.serialize_entry("disassembled", result)?;
                obj.end()
            }
            Err(error) => WasmError(error, &self.1).serialize(serializer)
        }
    }
}

struct UnimplementedModuleReader;

impl ModuleReader for UnimplementedModuleReader {
    fn resolve_module_path(&mut self, module_id: &ModuleId, _with_respect_to: &ModuleId) -> String {
        module_id.get_path("")
    }

    fn read_module(&mut self, _module_id: &ModuleId, _module_name: &String) -> Option<String> {
        wasm_bindgen::throw_str("Please provide a valid ModuleReader instance");
    }

    fn get_module_name(&self, module_id: &ModuleId) -> String {
        module_id.get_path("")
    }
}

#[wasm_bindgen]
extern "C" {
    pub type AbraContext;

    #[wasm_bindgen(method, js_name = println)]
    pub fn println(this: &AbraContext, input: &str);

    pub type JsModuleReader;

    #[wasm_bindgen(method, js_name = resolveModulePath)]
    pub fn _resolve_module_path(this: &JsModuleReader, module_id: &str, with_respect_to: &str) -> String;

    #[wasm_bindgen(method, js_name = readModule)]
    pub fn _read_module(this: &JsModuleReader, module_name: &str) -> Option<String>;

    #[wasm_bindgen(method, js_name = getModuleName)]
    pub fn _get_module_name(this: &JsModuleReader, module_name: &str) -> String;

    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn console_log(s: &str);
}

impl ModuleReader for JsModuleReader {
    fn resolve_module_path(&mut self, module_id: &ModuleId, with_respect_to: &ModuleId) -> String {
        let module_id = module_id.get_path("");
        let with_respect_to = with_respect_to.get_path("");
        self._resolve_module_path(&module_id, &with_respect_to)
    }

    fn read_module(&mut self, _module_id: &ModuleId, module_name: &String) -> Option<String> {
        self._read_module(&module_name)
    }

    fn get_module_name(&self, module_id: &ModuleId) -> String {
        if !module_id.0 {
            module_id.1.first()
                .map(|s| match s {
                    ModulePathSegment::Module(m) => m.to_string(),
                    _ => unreachable!()
                })
                .unwrap()
        } else {
            let module_name = module_id.get_path("");
            self._get_module_name(&module_name)
        }
    }
}

#[wasm_bindgen(js_name = disassemble)]
pub fn disassemble(input: &str) -> JsValue {
    let mut module_reader = UnimplementedModuleReader;
    let module_id = ModuleId::from_name("./_repl");
    let result = compile_and_disassemble(module_id, &input.to_string(), &mut module_reader);
    let disassemble_result = DisassembleResult(result, input.to_string());
    JsValue::from_serde(&disassemble_result)
        .unwrap_or(JsValue::NULL)
}

#[wasm_bindgen(js_name = disassembleModule)]
pub fn disassemble_module(module_name: &str, mut module_reader: JsModuleReader) -> JsValue {
    let input = match module_reader._read_module(module_name) {
        None => wasm_bindgen::throw_str(format!("Could not load module '{}'", module_name).as_str()),
        Some(input) => input
    };

    let module_id = ModuleId::from_name(module_name);
    let result = compile_and_disassemble(module_id, &input.to_string(), &mut module_reader);
    let disassemble_result = DisassembleResult(result, input.to_string());
    JsValue::from_serde(&disassemble_result)
        .unwrap_or(JsValue::NULL)
}

#[wasm_bindgen(js_name = typecheck)]
pub fn typecheck_input(input: &str) -> JsValue {
    let mut module_reader = UnimplementedModuleReader;
    let module_id = ModuleId::from_name("./_repl");
    let mut module_loader = ModuleLoader::new(&mut module_reader);
    let result = typecheck(module_id, &input.to_string(), &mut module_loader).map(|_| ());
    let typecheck_result = TypecheckedResult(result, input.to_string());
    JsValue::from_serde(&typecheck_result)
        .unwrap_or(JsValue::NULL)
}

#[wasm_bindgen(js_name = typecheckModule)]
pub fn typecheck_module(module_name: &str, mut module_reader: JsModuleReader) -> JsValue {
    let input = match module_reader._read_module(module_name) {
        None => wasm_bindgen::throw_str(format!("Could not load module '{}'", module_name).as_str()),
        Some(input) => input
    };

    let module_id = ModuleId::from_name(module_name);
    let mut module_loader = ModuleLoader::new(&mut module_reader);
    let result = typecheck(module_id, &input.to_string(), &mut module_loader).map(|_| ());
    let typecheck_result = TypecheckedResult(result, input.to_string());
    JsValue::from_serde(&typecheck_result)
        .unwrap_or(JsValue::NULL)
}

fn compile_and_run<R>(module_name: &str, input: String, ctx: VMContext, module_reader: &mut R) -> Result<(Value, String), Error>
    where R: ModuleReader
{
    let module_id = ModuleId::from_name(module_name);
    let modules = compile(module_id, &input, module_reader)?;
    let mut vm = VM::new(ctx);
    let mut res = Value::Nil;

    for module in modules {
        match vm.run(module) {
            Ok(v) => res = v,
            Err(e) => return Err(Error::InterpretError(e)),
        }
    }
    let as_str = to_string(&res, &mut vm);
    Ok((res, as_str))
}

#[wasm_bindgen(js_name = run)]
pub fn run(input: &str, js_bridge: Option<AbraContext>) -> JsValue {
    let ctx = VMContext {
        print: match js_bridge {
            Some(ctx) => Box::new(move |input| {
                let input = if input.ends_with('\n') {
                    &input[0..(input.len() - 1)]
                } else {
                    input
                };
                ctx.println(input)
            }),
            None => Box::new(|input| console_log(input))
        },
        prompt: Box::new(|_prompt| unimplemented!()),
        ..VMContext::default()
    };

    let mut module_reader = UnimplementedModuleReader;
    let result = compile_and_run("./_repl", input.to_string(), ctx, &mut module_reader);
    let run_result = RunResult(result, input.to_string().clone());
    JsValue::from_serde(&run_result)
        .unwrap_or(JsValue::NULL)
}

#[wasm_bindgen(js_name = runModule)]
pub fn run_module(module_name: &str, mut module_reader: JsModuleReader, js_bridge: Option<AbraContext>) -> JsValue {
    let ctx = VMContext {
        print: match js_bridge {
            Some(ctx) => Box::new(move |input| {
                let input = if input.ends_with('\n') {
                    &input[0..(input.len() - 1)]
                } else {
                    input
                };
                ctx.println(input)
            }),
            None => Box::new(|input| console_log(input))
        },
        prompt: Box::new(|_prompt| unimplemented!()),
        ..VMContext::default()
    };

    let input = match module_reader._read_module(module_name) {
        None => wasm_bindgen::throw_str(format!("Could not load module '{}'", module_name).as_str()),
        Some(input) => input
    };

    let result = compile_and_run(module_name, input.to_string(), ctx, &mut module_reader);
    let run_result = RunResult(result, input.to_string().clone());
    JsValue::from_serde(&run_result)
        .unwrap_or(JsValue::NULL)
}
