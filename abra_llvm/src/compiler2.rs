use std::collections::HashMap;
use std::path::PathBuf;
use std::process::{Command, ExitStatus};
use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::TargetMachine;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FloatType, FunctionType, IntType, PointerType, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue};
use itertools::Itertools;
use abra_core::lexer::tokens::{Range, Token};
use abra_core::parser::ast::{BinaryOp, BindingPattern};
use abra_core::typechecker::typechecker2::{AccessorKind, AssignmentKind, FuncId, FunctionKind, PRELUDE_BOOL_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_INT_TYPE_ID, PRELUDE_MODULE_ID, PRELUDE_SCOPE_ID, PRELUDE_STRING_TYPE_ID, PRELUDE_UNIT_TYPE_ID, PrimitiveType, Project, StructId, Type, TypedLiteral, TypedNode, TypeId, TypeKind, VariableAlias, VarId};

const STRING_NEW_FN_NAME: &str = "String.new(Pointer<Byte>,Int32):String";
const ABRA_MAIN_FN_NAME: &str = "_abra_main";

#[derive(Debug, Default)]
struct CompilerContext {
    resolved_generics: HashMap<String, TypeId>,
}

enum LLVMVar<'ctx> {
    Slot(PointerValue<'ctx>),
    Param(BasicValueEnum<'ctx>),
}

pub struct LLVMCompiler2<'a> {
    project: &'a Project,
    context: &'a Context,
    builder: Builder<'a>,
    main_module: Module<'a>,
    current_fn: FunctionValue<'a>,
    variables: HashMap<VarId, LLVMVar<'a>>,
    ctx_stack: Vec<CompilerContext>,

    // cached for convenience
    string_type: StructType<'a>,
}

impl<'a> LLVMCompiler2<'a> {
    pub fn compile(project: &Project, out_dir: &PathBuf, out_file_name: Option<String>) -> PathBuf {
        let context = Context::create();
        let mut compiler = LLVMCompiler2::new(project, &context);
        compiler.generate(project);

        let out_name = out_file_name.unwrap_or("main".into());

        let llvm_module_out_file = out_dir.join(format!("_{}.ll", &out_name));
        compiler.main_module.print_to_file(&llvm_module_out_file).unwrap();

        let exec_out_file = out_dir.join(&out_name);
        let cc_output = Command::new("clang")
            .arg(&llvm_module_out_file)
            .arg("-o")
            .arg(&exec_out_file)
            .arg("-Wno-override-module")
            .output()
            .unwrap();
        if !cc_output.stderr.is_empty() {
            eprintln!("{}", String::from_utf8(cc_output.stderr).unwrap());
        }
        if !cc_output.stdout.is_empty() {
            print!("{}", String::from_utf8(cc_output.stdout).unwrap());
        }

        exec_out_file
    }

    pub fn compile_and_run(project: &Project, out_dir: &PathBuf, out_file_name: Option<String>) -> ExitStatus {
        let exec_out_file = Self::compile(project, out_dir, out_file_name);

        let run_output = Command::new(&exec_out_file).output().unwrap();
        if !run_output.stderr.is_empty() {
            eprintln!("Error: {}", String::from_utf8(run_output.stderr).unwrap());
        }
        if !run_output.stdout.is_empty() {
            print!("{}", String::from_utf8(run_output.stdout).unwrap());
        }
        run_output.status
    }

    fn new(project: &'a Project, context: &'a Context) -> Self {
        let builder = context.create_builder();
        let main_module = context.create_module("__main");

        let triple = TargetMachine::get_default_triple();
        main_module.set_triple(&triple);

        let abra_main_fn = Self::start_abra_main(&context, &main_module, &builder);

        let string_type = context.opaque_struct_type("String");
        string_type.set_body(&[
            context.i32_type().into(), // type_id
            // context.i32_type().into(), // bytesize
            context.i32_type().into(), // length
            context.i8_type().ptr_type(AddressSpace::Generic).into(), // bytes
        ], false);

        Self {
            project,
            context,
            builder,
            main_module,
            current_fn: abra_main_fn,
            variables: HashMap::new(),
            ctx_stack: vec![CompilerContext::default()],
            string_type,
        }
    }

    // LLVM UTILS START

    fn bool(&self) -> IntType<'a> {
        self.context.bool_type()
    }

    fn const_bool(&self, value: bool) -> IntValue<'a> {
        self.bool().const_int(if value { 1 } else { 0 }, false)
    }

    fn i8(&self) -> IntType<'a> {
        self.context.i8_type()
    }

    fn i32(&self) -> IntType<'a> {
        self.context.i32_type()
    }

    fn const_i32(&self, value: u64) -> IntValue<'a> {
        self.i32().const_int(value, false)
    }

    fn i64(&self) -> IntType<'a> {
        self.context.i64_type()
    }

    fn const_i64(&self, value: u64) -> IntValue<'a> {
        self.i64().const_int(value, false)
    }

    fn f64(&self) -> FloatType<'a> {
        self.context.f64_type()
    }

    fn const_f64(&self, value: f64) -> FloatValue<'a> {
        self.f64().const_float(value)
    }

    fn ptr<T: BasicType<'a>>(&self, t: T) -> PointerType<'a> {
        t.ptr_type(AddressSpace::Generic)
    }

    fn null_ptr(&self) -> PointerValue<'a> {
        self.ptr(self.i8()).const_null()
    }

    fn fn_type<T: BasicType<'a>>(&self, ret: T, param_types: &[BasicMetadataTypeEnum<'a>]) -> FunctionType<'a> {
        ret.fn_type(param_types, false)
    }

    fn fn_type_variadic<T: BasicType<'a>>(&self, ret: T, param_types: &[BasicMetadataTypeEnum<'a>]) -> FunctionType<'a> {
        ret.fn_type(param_types, true)
    }

    fn sizeof_struct<T: BasicType<'a>>(&self, t: T) -> IntValue<'a> {
        let sizeof_struct = self.builder.build_struct_gep(self.null_ptr().const_cast(self.ptr(t)), 1, "").unwrap();
        self.builder.build_ptr_to_int(sizeof_struct, self.i64(), "")
    }

    fn llvm_type_name_by_id(&self, type_id: &TypeId, resolved_generics: &HashMap<String, TypeId>) -> String {
        let ty = self.project.get_type_by_id(type_id);
        match ty {
            Type::Primitive(PrimitiveType::Unit) => "Unit".into(),
            Type::Primitive(PrimitiveType::Any) => "Any".into(),
            Type::Primitive(PrimitiveType::Int) => "Int".into(),
            Type::Primitive(PrimitiveType::Float) => "Float".into(),
            Type::Primitive(PrimitiveType::Bool) => "Bool".into(),
            Type::Primitive(PrimitiveType::String) => "String".into(),
            Type::Generic(_, name) => {
                resolved_generics.get(name)
                    .map(|type_id| self.llvm_type_name_by_id(type_id, resolved_generics))
                    .unwrap_or(name.clone())
            }
            Type::GenericInstance(struct_id, generic_ids) => {
                let struct_ = self.project.get_struct_by_id(struct_id);
                let struct_name = &struct_.name;
                let generic_names = generic_ids.iter().map(|type_id| self.llvm_type_name_by_id(type_id, resolved_generics)).join(",");
                let generic_names = if generic_ids.is_empty() { "".into() } else { format!("<{}>", generic_names) };

                format!("{struct_name}{generic_names}")
            }
            Type::GenericEnumInstance(_, _, _) |
            Type::Function(_, _, _, _) => todo!(),
            Type::Type(kind) => match kind {
                TypeKind::Struct(struct_id) => {
                    let struct_ = self.project.get_struct_by_id(struct_id);
                    let struct_name = &struct_.name;
                    let generic_names = struct_.generic_ids.iter()
                        .map(|type_id| {
                            let generic_name = self.get_generic_name(type_id);
                            if let Some(sub_type_id) = resolved_generics.get(generic_name) {
                                self.llvm_type_name_by_id(sub_type_id, resolved_generics)
                            } else {
                                generic_name.clone()
                            }
                        })
                        .join(",");
                    let generic_names = if struct_.generic_ids.is_empty() { "".into() } else { format!("<{}>", generic_names) };
                    format!("{struct_name}{generic_names}")
                }
                TypeKind::Enum(_) => todo!(),
            }
            Type::ModuleAlias => todo!()
        }
    }

    fn llvm_underlying_type_by_id(&self, type_id: &TypeId, resolved_generics: &HashMap<String, TypeId>) -> Option<BasicTypeEnum<'a>> {
        let ty = self.project.get_type_by_id(type_id);
        let llvm_type = match ty {
            Type::Primitive(PrimitiveType::Unit) => return None,
            Type::Primitive(PrimitiveType::Any) => todo!(),
            Type::Primitive(PrimitiveType::Int) => self.i64().as_basic_type_enum(),
            Type::Primitive(PrimitiveType::Float) => self.f64().as_basic_type_enum(),
            Type::Primitive(PrimitiveType::Bool) => self.bool().as_basic_type_enum(),
            Type::Primitive(PrimitiveType::String) => self.string_type.as_basic_type_enum(),
            Type::Generic(_, name) => {
                return resolved_generics.get(name)
                    .and_then(|resolved_type_id| self.llvm_underlying_type_by_id(resolved_type_id, resolved_generics));
            }
            Type::GenericInstance(_, generics) => {
                let type_name = self.llvm_type_name_by_id(type_id, resolved_generics);

                // Handle special case of the Pointer type, which is really just an alias for raw underlying pointers
                if type_name.starts_with("Pointer<") {
                    let ptr_generic_id = &generics[0];
                    let Some(ptr_llvm_type) = self.llvm_underlying_type_by_id(ptr_generic_id, resolved_generics) else { todo!() };
                    let ptr_llvm_type = self.llvm_ptr_wrap_type_if_needed(ptr_llvm_type);
                    return Some(self.ptr(ptr_llvm_type).as_basic_type_enum());
                }

                if let Some(llvm_type) = self.main_module.get_struct_type(&type_name) {
                    llvm_type.as_basic_type_enum()
                } else {
                    let struct_type = self.compile_struct_type(type_id, resolved_generics);
                    debug_assert!(struct_type.get_name().unwrap().to_str().unwrap() == &type_name);
                    struct_type.as_basic_type_enum()
                }
            }
            Type::GenericEnumInstance(_, _, _) |
            Type::Function(_, _, _, _) |
            Type::Type(_) |
            Type::ModuleAlias => todo!()
        };

        Some(llvm_type)
    }

    fn llvm_ptr_wrap_type_if_needed(&self, llvm_type: BasicTypeEnum<'a>) -> BasicTypeEnum<'a> {
        if llvm_type.is_struct_type() { self.ptr(llvm_type).as_basic_type_enum() } else { llvm_type }
    }

    fn llvm_method_name<S: AsRef<str>>(&self, name: S, container_type_id: &TypeId, is_instance_method: bool) -> String {
        let ty = self.project.get_type_by_id(container_type_id);
        let func_id = if is_instance_method {
            ty.find_method_by_name(self.project, name.as_ref())
        } else {
            ty.find_static_method_by_name(self.project, name.as_ref())
        };

        let type_name = self.llvm_type_name_by_id(container_type_id, &HashMap::new());
        let func_id = func_id.expect(&format!("Method {} on type {} expected to exist", name.as_ref(), &type_name));

        self.llvm_function_signature(func_id, &HashMap::new())
    }

    fn llvm_function_signature(&self, func_id: &FuncId, resolved_generics: &HashMap<String, TypeId>) -> String {
        let function = self.project.get_func_by_id(func_id);

        let type_args = function.generic_ids.iter()
            .map(|type_id| self.llvm_type_name_by_id(type_id, resolved_generics))
            .join(",");
        let type_args = if function.generic_ids.is_empty() { "".into() } else { format!("<{}>", type_args) };
        let params = function.params.iter()
            .map(|p| self.llvm_type_name_by_id(&p.type_id, resolved_generics))
            .join(",");
        let ret = self.llvm_type_name_by_id(&function.return_type_id, resolved_generics);

        let prefix = match &function.kind {
            FunctionKind::Freestanding => "".into(),
            FunctionKind::Method(type_id) => format!("{}#", self.llvm_type_name_by_id(type_id, resolved_generics)),
            FunctionKind::StaticMethod(type_id) => format!("{}.", self.llvm_type_name_by_id(type_id, &HashMap::new())),
        };
        let function_name = &function.name;
        format!("{prefix}{function_name}{type_args}({params}):{ret}")
    }

    fn llvm_initializer_signature(&self, struct_id: &StructId, resolved_generics: &HashMap<String, TypeId>) -> String {
        let struct_ = self.project.get_struct_by_id(struct_id);

        let llvm_type_name = self.llvm_type_name_by_id(&struct_.self_type_id, resolved_generics);
        format!("{}.init", llvm_type_name)
    }

    fn llvm_function_type(&self, func_id: &FuncId, resolved_generics: &HashMap<String, TypeId>) -> FunctionType<'a> {
        let function = self.project.get_func_by_id(func_id);

        let params = function.params.iter()
            .map(|p| {
                let Some(llvm_type) = self.llvm_underlying_type_by_id(&p.type_id, resolved_generics) else { todo!() };
                self.llvm_ptr_wrap_type_if_needed(llvm_type)
            });
        if function.return_type_id == PRELUDE_UNIT_TYPE_ID {
            self.context.void_type().fn_type(params.map(|ty| ty.into()).collect_vec().as_slice(), false)
        } else {
            let Some(ret_llvm_type) = self.llvm_underlying_type_by_id(&function.return_type_id, resolved_generics) else { todo!() };
            let ret_llvm_type = self.llvm_ptr_wrap_type_if_needed(ret_llvm_type);

            self.fn_type(ret_llvm_type, params.map(|ty| ty.into()).collect_vec().as_slice())
        }
    }

    fn llvm_initializer_type(&self, struct_id: &StructId, resolved_generics: &HashMap<String, TypeId>) -> FunctionType<'a> {
        let struct_ = self.project.get_struct_by_id(struct_id);

        let params = struct_.fields.iter()
            .map(|f| {
                let Some(llvm_type) = self.llvm_underlying_type_by_id(&f.type_id, resolved_generics) else { todo!() };
                self.llvm_ptr_wrap_type_if_needed(llvm_type)
            });
        let Some(ret_llvm_type) = self.llvm_underlying_type_by_id(&struct_.self_type_id, resolved_generics) else { todo!() };
        let ret_llvm_type = self.llvm_ptr_wrap_type_if_needed(ret_llvm_type);

        self.fn_type(ret_llvm_type, params.map(|ty| ty.into()).collect_vec().as_slice())
    }

    fn get_generic_name(&self, type_id: &TypeId) -> &String {
        let Type::Generic(_, generic_name) = self.project.get_type_by_id(type_id) else { unreachable!("TypeId {:?} represents a type that is not a generic", type_id) };
        generic_name
    }

    // LLVM UTILS END

    fn start_abra_main(context: &'a Context, main_module: &Module<'a>, builder: &Builder<'a>) -> FunctionValue<'a> {
        let argc_t = context.i32_type();
        let argv_t = context.i8_type().ptr_type(AddressSpace::Generic).ptr_type(AddressSpace::Generic);
        let fn_type = context.void_type().fn_type(&[argc_t.into(), argv_t.into()], false);
        let abra_main_fn = main_module.add_function(ABRA_MAIN_FN_NAME, fn_type, None);
        let block = context.append_basic_block(abra_main_fn, "");
        builder.position_at_end(block);

        abra_main_fn
    }

    fn end_abra_main(&self) {
        let abra_main_fn = self.current_fn;
        debug_assert!(abra_main_fn.get_name().to_str().unwrap() == ABRA_MAIN_FN_NAME);
        let b = abra_main_fn.get_last_basic_block().expect("abra_main is guaranteed to have >=1 block");
        self.builder.position_at_end(b);
        self.builder.build_return(None);
    }

    fn build_main_fn(&self) {
        let argc_t = self.i32();
        let argv_t = self.ptr(self.ptr(self.i8()));
        let main_fn_type = self.fn_type(self.i32(), &[argc_t.into(), argv_t.into()]);
        let entry_fn = self.main_module.add_function("main", main_fn_type, None);
        let block = self.context.append_basic_block(entry_fn, "");
        self.builder.position_at_end(block);

        let abra_main_fn = self.main_module.get_function(ABRA_MAIN_FN_NAME).expect("abra_main is defined at the start");
        self.builder.build_call(abra_main_fn, &[entry_fn.get_nth_param(0).unwrap().into(), entry_fn.get_nth_param(1).unwrap().into()], "");
        self.builder.build_return(Some(&self.const_i32(0).as_basic_value_enum()));
    }

    pub fn generate(&mut self, project: &Project) {
        self.generate_prelude();
        self.build_main_fn();

        let printf = self.main_module.add_function("printf", self.fn_type_variadic(self.i64(), &[self.ptr(self.i8()).into()]), None);

        for m in &project.modules {
            if m.id == PRELUDE_MODULE_ID { continue; }

            // The top-level code in a module is executed in the special $mod_{mod_id} fn...
            let mod_fn_name = format!("$mod_{}", m.id.0);
            let mod_fn_type = self.fn_type(self.bool(), &[]);
            let mod_fn = self.main_module.add_function(&mod_fn_name, mod_fn_type, None);
            let prev_fn = self.current_fn;
            self.current_fn = mod_fn;
            let block = self.context.append_basic_block(mod_fn, "");
            self.builder.position_at_end(block);

            let num_nodes = m.code.len();
            for (idx, node) in m.code.iter().enumerate() {
                let res = self.visit_statement(node);
                let node_type_id = node.type_id();
                if idx == num_nodes - 1 && *node_type_id != PRELUDE_UNIT_TYPE_ID {
                    if let Some(res) = res {
                        let to_string_fn_name = self.llvm_method_name("toString", node_type_id, true);
                        let to_string_fn = self.main_module.get_function(&to_string_fn_name).unwrap();

                        let str_val = self.builder.build_call(to_string_fn, &[res.into()], "repr").try_as_basic_value().left().unwrap().into_pointer_value();
                        let chars_val_ptr = self.builder.build_struct_gep(str_val.const_cast(self.ptr(self.string_type)), 2, "").unwrap();
                        let chars_val = self.builder.build_load(chars_val_ptr, "repr.chars");

                        let fmt_str = self.builder.build_global_string_ptr("%s", "").as_basic_value_enum();
                        self.builder.build_call(printf, &[fmt_str.into(), chars_val.into()], "").try_as_basic_value().left().unwrap();
                    }
                }
            }

            // ...which returns true/false depending on whether it succeeded. For now, it will always
            // succeed, but when `try` is introduced, top-level code may fail.
            self.builder.build_return(Some(&self.const_bool(true)));

            self.current_fn = prev_fn;
            debug_assert!(self.current_fn.get_name().to_str().unwrap() == ABRA_MAIN_FN_NAME);
            // Call the $mod_{mod_id} fn in the _abra_main fn.
            self.builder.position_at_end(self.current_fn.get_last_basic_block().unwrap());
            self.builder.build_call(mod_fn, &[], "");
        }

        self.end_abra_main();
    }

    // TODO: This function is only for initial bootstrapping purposes and will be replaced by a real prelude implementation in Abra
    fn generate_prelude(&self) {
        let snprintf = self.main_module.add_function("snprintf", self.fn_type_variadic(self.i64(), &[self.ptr(self.i8()).into(), self.i32().into(), self.ptr(self.i8()).into()]), None);
        let malloc = self.main_module.add_function("malloc", self.fn_type(self.ptr(self.i8()), &[self.i64().into()]), None);

        // String.new(Pointer<Byte>,Int32):String
        {
            let fn_type = self.fn_type(self.ptr(self.string_type), &[self.ptr(self.i8()).into(), self.i32().into()]);
            let func = self.main_module.add_function(STRING_NEW_FN_NAME, fn_type, None);
            let mut func_params_iter = func.get_param_iter();
            func_params_iter.next().unwrap().set_name("chars");
            func_params_iter.next().unwrap().set_name("length");

            let block = self.context.append_basic_block(func, "");
            self.builder.position_at_end(block);

            let sizeof_string = self.sizeof_struct(self.string_type);
            let str_mem = self.builder.build_pointer_cast(
                self.builder.build_call(malloc, &[sizeof_string.into()], "").try_as_basic_value().left().unwrap().into_pointer_value(),
                self.ptr(self.string_type),
                "str_mem",
            );
            let mem_cursor = self.builder.build_struct_gep(str_mem, 0, "String#type_id").unwrap();
            self.builder.build_store(mem_cursor, self.const_i32(self.project.prelude_string_struct_id.1 as u64));
            let mem_cursor = self.builder.build_struct_gep(str_mem, 1, "String#length").unwrap();
            self.builder.build_store(mem_cursor, func.get_nth_param(1).unwrap());
            let mem_cursor = self.builder.build_struct_gep(str_mem, 2, "String#chars").unwrap();
            self.builder.build_store(mem_cursor, func.get_nth_param(0).unwrap());

            self.builder.build_return(Some(&str_mem.as_basic_value_enum()));
        }

        // Int#toString():String
        {
            let fn_type = self.fn_type(self.ptr(self.string_type), &[self.i64().into()]);
            let fn_name = self.llvm_method_name("toString", &PRELUDE_INT_TYPE_ID, true);
            let func = self.main_module.add_function(&fn_name, fn_type, None);
            func.get_param_iter().next().unwrap().set_name("self");
            let block = self.context.append_basic_block(func, "");
            self.builder.position_at_end(block);
            let fmt_str = self.builder.build_global_string_ptr("%jd", "").as_basic_value_enum();
            let self_param = func.get_nth_param(0).unwrap();
            let len_val = self.builder.build_call(snprintf, &[self.null_ptr().into(), self.const_i32(0).into(), fmt_str.into(), self_param.into()], "len").try_as_basic_value().left().unwrap().into_int_value();
            let len_val = self.builder.build_int_cast(len_val, self.i32(), "");
            let len_plus_1 = self.builder.build_int_add::<IntValue<'a>>(len_val.into(), self.const_i32(1).into(), "len_plus_1");
            let str_val = self.builder.build_call(malloc, &[self.builder.build_int_cast(len_plus_1, self.i64(), "").into()], "str").try_as_basic_value().left().unwrap();
            self.builder.build_call(snprintf, &[str_val.into(), len_plus_1.into(), fmt_str.into(), self_param.into()], "").try_as_basic_value().left().unwrap();

            let ret_val = self.builder.build_call(
                self.main_module.get_function(STRING_NEW_FN_NAME).unwrap(),
                &[str_val.into(), len_val.into()],
                "",
            ).try_as_basic_value().left().unwrap();

            self.builder.build_return(Some(&ret_val));
        }

        // Float#toString():String
        {
            let fn_type = self.fn_type(self.ptr(self.string_type), &[self.f64().into()]);
            let fn_name = self.llvm_method_name("toString", &PRELUDE_FLOAT_TYPE_ID, true);
            let func = self.main_module.add_function(&fn_name, fn_type, None);
            func.get_param_iter().next().unwrap().set_name("self");
            let block = self.context.append_basic_block(func, "");
            self.builder.position_at_end(block);
            let fmt_str = self.builder.build_global_string_ptr("%g", "").as_basic_value_enum();
            let self_param = func.get_nth_param(0).unwrap();
            let len_val = self.builder.build_call(snprintf, &[self.null_ptr().into(), self.const_i32(0).into(), fmt_str.into(), self_param.into()], "len").try_as_basic_value().left().unwrap().into_int_value();
            let len_val = self.builder.build_int_cast(len_val, self.i32(), "");
            let len_plus_1 = self.builder.build_int_add::<IntValue<'a>>(len_val.into(), self.const_i32(1).into(), "len_plus_1");
            let str_val = self.builder.build_call(malloc, &[self.builder.build_int_cast(len_plus_1, self.i64(), "").into()], "str").try_as_basic_value().left().unwrap();
            self.builder.build_call(snprintf, &[str_val.into(), len_plus_1.into(), fmt_str.into(), self_param.into()], "").try_as_basic_value().left().unwrap();

            let ret_val = self.builder.build_call(
                self.main_module.get_function(STRING_NEW_FN_NAME).unwrap(),
                &[str_val.into(), len_val.into()],
                "",
            ).try_as_basic_value().left().unwrap();

            self.builder.build_return(Some(&ret_val));
        }

        // Bool#toString():String
        {
            let bool_true_str_global = self.main_module.add_global(self.string_type, None, "BOOL_TRUE_STR");
            bool_true_str_global.set_constant(true);
            let true_str = self.builder.build_global_string_ptr("true", "").as_basic_value_enum();
            bool_true_str_global.set_initializer(&self.context.const_struct(&[
                self.const_i32(self.project.prelude_string_struct_id.1 as u64).into(),
                self.const_i32(4).into(),
                true_str,
            ], false));
            let bool_false_str_global = self.main_module.add_global(self.string_type, None, "BOOL_FALSE_STR");
            bool_false_str_global.set_constant(true);
            let false_str = self.builder.build_global_string_ptr("false", "").as_basic_value_enum();
            bool_false_str_global.set_initializer(&self.context.const_struct(&[
                self.const_i32(self.project.prelude_string_struct_id.1 as u64).into(),
                self.const_i32(5).into(),
                false_str,
            ], false));

            let fn_type = self.fn_type(self.ptr(self.string_type), &[self.bool().into()]);
            let fn_name = self.llvm_method_name("toString", &PRELUDE_BOOL_TYPE_ID, true);
            let func = self.main_module.add_function(&fn_name, fn_type, None);
            func.get_param_iter().next().unwrap().set_name("self");
            let block = self.context.append_basic_block(func, "");
            self.builder.position_at_end(block);

            let cond = func.get_nth_param(0).unwrap().into_int_value();
            let then_bb = self.context.append_basic_block(func, "then");
            let else_bb = self.context.append_basic_block(func, "else");
            let cont_bb = self.context.append_basic_block(func, "cont");
            self.builder.build_conditional_branch(cond, then_bb, else_bb);

            self.builder.position_at_end(then_bb);
            let then_val = bool_true_str_global.as_pointer_value();
            self.builder.build_unconditional_branch(cont_bb);

            self.builder.position_at_end(else_bb);
            let else_val = bool_false_str_global.as_pointer_value();
            self.builder.build_unconditional_branch(cont_bb);

            self.builder.position_at_end(cont_bb);
            let phi = self.builder.build_phi(self.ptr(self.string_type), "");
            phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

            self.builder.build_return(Some(&phi.as_basic_value()));
        }

        // String#toString():String
        {
            let fn_type = self.fn_type(self.ptr(self.string_type), &[self.ptr(self.string_type).into()]);
            let fn_name = self.llvm_method_name("toString", &PRELUDE_STRING_TYPE_ID, true);
            let func = self.main_module.add_function(&fn_name, fn_type, None);
            func.get_param_iter().next().unwrap().set_name("self");
            let block = self.context.append_basic_block(func, "");
            self.builder.position_at_end(block);
            self.builder.build_return(Some(&func.get_nth_param(0).unwrap()));
        }
    }

    fn visit_statement(&mut self, node: &TypedNode) -> Option<BasicValueEnum<'a>> {
        match node {
            TypedNode::If { .. } |
            TypedNode::Match { .. } |
            TypedNode::FuncDeclaration(_) |
            TypedNode::TypeDeclaration(_) |
            TypedNode::EnumDeclaration(_) => todo!(),
            TypedNode::BindingDeclaration { vars, pattern, expr, .. } => {
                let BindingPattern::Variable(token) = pattern else { todo!() };
                let var_name = Token::get_ident_name(token);
                let Some(variable) = vars.iter().find_map(|var_id| {
                    let var = self.project.get_var_by_id(var_id);
                    if var.name == var_name { Some(var) } else { None }
                }) else { unreachable!() };

                let Some(expr) = expr else { todo!() };
                let expr_val = self.visit_expression(expr);
                if let Some(expr_val) = expr_val {
                    let llvm_type = self.llvm_underlying_type_by_id(&variable.type_id, &HashMap::new()).unwrap_or_else(|| expr_val.get_type().as_basic_type_enum());
                    let llvm_type = self.llvm_ptr_wrap_type_if_needed(llvm_type);
                    let slot = self.builder.build_alloca(llvm_type, &var_name);
                    self.variables.insert(variable.id, LLVMVar::Slot(slot));

                    self.builder.build_store(slot, expr_val);
                }

                None
            }
            TypedNode::ForLoop { .. } |
            TypedNode::WhileLoop { .. } |
            TypedNode::Break { .. } |
            TypedNode::Continue { .. } |
            TypedNode::Return { .. } |
            TypedNode::Import { .. } => None,
            _ => self.visit_expression(node),
        }
    }

    fn visit_expression(&mut self, node: &TypedNode) -> Option<BasicValueEnum<'a>> {
        match node {
            TypedNode::Literal { value, .. } => {
                let value = match value {
                    TypedLiteral::Int(v) => {
                        let int = self.const_i64((*v) as u64);
                        int.as_basic_value_enum()
                    }
                    TypedLiteral::Float(f) => {
                        let float = self.const_f64(*f);
                        float.as_basic_value_enum()
                    }
                    TypedLiteral::Bool(b) => {
                        let b = self.const_bool(*b);
                        b.as_basic_value_enum()
                    }
                    TypedLiteral::String(s) => {
                        let str = self.builder.build_global_string_ptr(&s, "").as_basic_value_enum();
                        let str_len = self.const_i32(s.len() as u64);
                        let string_new_fn = self.main_module.get_function(&STRING_NEW_FN_NAME).unwrap();
                        self.builder.build_call(string_new_fn, &[str.into(), str_len.into()], "").try_as_basic_value().left().unwrap()
                    }
                };
                Some(value)
            }
            TypedNode::Unary { .. } => todo!(),
            TypedNode::Binary { left, op, right, .. } => {
                let left_type_id = left.as_ref().type_id();
                let right_type_id = right.as_ref().type_id();

                if let BinaryOp::Add = op {
                    if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                        let left = self.visit_expression(left).unwrap().into_int_value();
                        let right = self.visit_expression(right).unwrap().into_int_value();
                        Some(self.builder.build_int_add(left, right, "").into())
                    } else {
                        todo!()
                    }
                } else {
                    todo!()
                }
            }
            TypedNode::Grouped { .. } => todo!(),
            TypedNode::Array { items, type_id, .. } => {
                let Type::GenericInstance(struct_id, generics) = self.project.get_type_by_id(type_id) else { unreachable!() };
                debug_assert!(struct_id == &self.project.prelude_array_struct_id);
                let array_struct = self.project.get_struct_by_id(struct_id);
                let inner_type_id = generics[0];
                let array_type = self.project.get_type_by_id(&self.project.find_type_id(&PRELUDE_SCOPE_ID, &Type::Type(TypeKind::Struct(*struct_id))).unwrap());

                let array_with_capacity_llvm_fn = {
                    let array_with_capacity_func_id = array_type.find_static_method_by_name(self.project, "withCapacity").unwrap();
                    let array_with_capacity_fn = self.project.get_func_by_id(array_with_capacity_func_id);

                    let mut resolved_generics = HashMap::new();
                    resolved_generics.insert(self.get_generic_name(&array_with_capacity_fn.generic_ids[0]).clone(), inner_type_id);
                    self.get_or_compile_function(&array_with_capacity_func_id, &resolved_generics)
                };
                let array_push_llvm_fn = {
                    let array_push_func_id = array_type.find_method_by_name(self.project, "push").unwrap();
                    let mut resolved_generics = HashMap::new();
                    resolved_generics.insert(self.get_generic_name(&array_struct.generic_ids[0]).clone(), inner_type_id);
                    self.get_or_compile_function(&array_push_func_id, &resolved_generics)
                };

                let arr_val = self.builder.build_call(array_with_capacity_llvm_fn, &[self.const_i64(items.len() as u64).into()], "").try_as_basic_value().left().unwrap();
                for item in items {
                    let item = self.visit_expression(item).unwrap();
                    self.builder.build_call(array_push_llvm_fn, &[arr_val.into(), item.into()], "").try_as_basic_value().left();
                }

                Some(arr_val)
            }
            TypedNode::Tuple { .. } |
            TypedNode::Set { .. } |
            TypedNode::Map { .. } => todo!(),
            TypedNode::Identifier { var_id, .. } => {
                let variable = self.project.get_var_by_id(var_id);
                let llvm_var = self.variables.get(&variable.id).expect(&format!("No stored slot for variable {} ({:?})", &variable.name, &variable));
                let value = match llvm_var {
                    LLVMVar::Slot(slot) => self.builder.build_load(*slot, &variable.name),
                    LLVMVar::Param(value) => *value,
                };

                Some(value)
            }
            TypedNode::NoneValue { .. } => todo!(),
            TypedNode::Invocation { target, arguments, .. } => {
                let mut args = Vec::with_capacity(arguments.len());

                let llvm_fn_val = match &**target {
                    // Handle invocation of identifiers which could be aliases for functions or types. Non-aliased variables (which could be function values)
                    // will be handled alongside other arbitrary expressions in the catchall block of this match.
                    TypedNode::Identifier { var_id, type_arg_ids, .. } if self.project.get_var_by_id(var_id).alias != VariableAlias::None => {
                        let variable = self.project.get_var_by_id(var_id);
                        match &variable.alias {
                            VariableAlias::None => unreachable!(),
                            VariableAlias::Function(_) => todo!(),
                            VariableAlias::Type(TypeKind::Enum(_)) => unreachable!("Cannot invoke an enum directly"),
                            VariableAlias::Type(TypeKind::Struct(struct_id)) => {
                                let resolved_generics = type_arg_ids.iter()
                                    .map(|(type_arg_id, _)| {
                                        let generic_name = self.get_generic_name(type_arg_id).clone();
                                        let resolved_type_id = self.ctx_stack.last().expect("There should always be a context in ctx_stack").resolved_generics.get(&generic_name).unwrap_or(type_arg_id);
                                        (generic_name, *resolved_type_id)
                                    })
                                    .collect();
                                let init_fn_sig = self.llvm_initializer_signature(struct_id, &resolved_generics);
                                if let Some(function_val) = self.main_module.get_function(&init_fn_sig) {
                                    function_val
                                } else {
                                    let function_val = self.compile_type_initializer(&struct_id, &resolved_generics);
                                    debug_assert!(function_val.get_name().to_str().unwrap() == &init_fn_sig);
                                    function_val
                                }
                            }
                        }
                    }
                    TypedNode::Accessor { target, kind, member_idx, type_arg_ids, .. } => {
                        let target_type_id = target.type_id();
                        let target_ty = self.project.get_type_by_id(target_type_id);

                        match kind {
                            AccessorKind::Field => todo!(),
                            AccessorKind::Method => {
                                let func_id = target_ty.get_method(self.project, *member_idx).unwrap();
                                let function = self.project.get_func_by_id(&func_id);
                                if let Some(dec) = function.decorators.iter().find(|dec| dec.name == "Intrinsic") {
                                    let TypedNode::Literal { value: TypedLiteral::String(intrinsic_name), .. } = &dec.args[0] else { unreachable!("@Intrinsic requires exactly 1 String argument") };
                                    let resolved_generics = type_arg_ids.iter()
                                        .map(|(type_arg_id, _)| {
                                            let generic_name = self.get_generic_name(type_arg_id).clone();
                                            let resolved_type_id = self.ctx_stack.last().expect("There should always be a context in ctx_stack").resolved_generics.get(&generic_name).unwrap_or(type_arg_id);
                                            (generic_name, *resolved_type_id)
                                        })
                                        .collect();
                                    return self.compile_intrinsic_invocation(type_arg_ids, &resolved_generics, intrinsic_name, Some(&**target), arguments);
                                }

                                let target = self.visit_expression(target).unwrap();
                                args.push(target.into());

                                let FunctionKind::Method(type_id) = function.kind else { unreachable!() };
                                let Some((struct_, _)) = self.project.get_struct_by_type_id(&type_id) else { todo!() };
                                let Type::GenericInstance(_, instance_generics) = target_ty else { todo!() };

                                let resolved_generics = struct_.generic_ids.iter().zip(instance_generics)
                                    .map(|(generic_id, type_arg_id)| (self.get_generic_name(generic_id).clone(), *type_arg_id))
                                    .chain(function.generic_ids.iter().zip(type_arg_ids)
                                        .map(|(generic_id, (type_arg_id, _))| {
                                            (self.get_generic_name(generic_id).clone(), *type_arg_id)
                                        })
                                    )
                                    .collect();

                                self.get_or_compile_function(&func_id, &resolved_generics)
                            }
                            AccessorKind::StaticMethod => {
                                let func_id = target_ty.get_static_method(self.project, *member_idx).unwrap();
                                let function = self.project.get_func_by_id(&func_id);
                                if let Some(dec) = function.decorators.iter().find(|dec| dec.name == "Intrinsic") {
                                    let TypedNode::Literal { value: TypedLiteral::String(intrinsic_name), .. } = &dec.args[0] else { unreachable!("@Intrinsic requires exactly 1 String argument") };
                                    let resolved_generics = type_arg_ids.iter()
                                        .filter_map(|(type_arg_id, _)| {
                                            if let Type::Generic(_, name) = self.project.get_type_by_id(type_arg_id) {
                                                let resolved_type_id = self.ctx_stack.last().expect("There should always be a context in ctx_stack").resolved_generics.get(name).unwrap_or(type_arg_id);
                                                Some((name.clone(), *resolved_type_id))
                                            } else {
                                                None
                                            }
                                        })
                                        .collect();
                                    return self.compile_intrinsic_invocation(type_arg_ids, &resolved_generics, intrinsic_name, None, arguments);
                                }

                                let resolved_generics = function.generic_ids.iter().zip(type_arg_ids)
                                    .map(|(generic_id, (type_arg_id, _))| (self.get_generic_name(generic_id).clone(), *type_arg_id))
                                    .collect();

                                self.get_or_compile_function(&func_id, &resolved_generics)
                            }
                            AccessorKind::EnumVariant => todo!(),
                        }
                    }
                    _ => todo!()
                };

                for arg_node in arguments {
                    let Some(arg_node) = arg_node else { todo!() };
                    args.push(self.visit_expression(arg_node).expect("Unit cannot be a valid argument value").into());
                }

                self.builder.build_call(llvm_fn_val, args.as_slice(), "").try_as_basic_value().left()
            }
            TypedNode::Accessor { target, kind, member_idx, is_opt_safe, .. } => {
                debug_assert!(!*is_opt_safe);

                let target_ty = self.project.get_type_by_id(target.type_id());
                let target = self.visit_expression(target).unwrap();

                match kind {
                    AccessorKind::Field => {
                        let field = target_ty.get_field(self.project, *member_idx).unwrap();
                        let field_slot = self.builder.build_struct_gep(target.into_pointer_value(), *member_idx as u32, &field.name).unwrap();
                        Some(self.builder.build_load(field_slot, ""))
                    }
                    AccessorKind::Method |
                    AccessorKind::StaticMethod |
                    AccessorKind::EnumVariant => todo!()
                }
            }
            TypedNode::Indexing { .. } |
            TypedNode::Lambda { .. } => todo!(),
            TypedNode::Assignment { kind, expr, .. } => {
                let expr_val = self.visit_expression(expr).unwrap();

                match kind {
                    AssignmentKind::Identifier { var_id } => {
                        let variable = self.project.get_var_by_id(var_id);
                        let llvm_var = self.variables.get(var_id).expect(&format!("No known llvm variable for variable '{}' ({:?})", &variable.name, var_id));
                        match llvm_var {
                            LLVMVar::Slot(ptr) => { self.builder.build_store(*ptr, expr_val); }
                            LLVMVar::Param(_) => unreachable!("Parameters are not assignable")
                        }
                    }
                    AssignmentKind::Accessor { target, kind, member_idx } => {
                        let target_ty = self.project.get_type_by_id(target.type_id());
                        let target = self.visit_expression(target).unwrap();

                        match kind {
                            AccessorKind::Field => {
                                let field = target_ty.get_field(self.project, *member_idx).unwrap();
                                let field_slot = self.builder.build_struct_gep(target.into_pointer_value(), *member_idx as u32, &field.name).unwrap();
                                self.builder.build_store(field_slot, expr_val);
                            }
                            AccessorKind::Method |
                            AccessorKind::StaticMethod |
                            AccessorKind::EnumVariant => todo!()
                        }
                    }
                    AssignmentKind::Indexing { .. } => todo!()
                }

                Some(expr_val)
            }
            _ => unreachable!("Node is not an expression and should have been handled in visit_statement")
        }
    }

    fn compile_intrinsic_invocation(&mut self, type_arg_ids: &Vec<(TypeId, Range)>, resolved_generics: &HashMap<String, TypeId>, name: &String, implicit_argument: Option<&TypedNode>, arguments: &Vec<Option<TypedNode>>) -> Option<BasicValueEnum<'a>> {
        match name.as_ref() {
            "pointer_null" => { // Static method
                let pointer_type = type_arg_ids[0].0;
                let Some(llvm_type) = self.llvm_underlying_type_by_id(&pointer_type, resolved_generics) else { todo!() };
                let llvm_type = self.llvm_ptr_wrap_type_if_needed(llvm_type);

                Some(self.ptr(llvm_type).const_null().as_basic_value_enum())
            }
            "pointer_malloc" => { // Static method
                let pointer_type_id = type_arg_ids[0].0;
                let pointer_ty = self.project.get_type_by_id(&pointer_type_id);
                let pointer_ty = if let Type::Generic(_, name) = pointer_ty {
                    self.project.get_type_by_id(resolved_generics.get(name).unwrap_or(&pointer_type_id))
                } else {
                    pointer_ty
                };
                let Some(llvm_type) = self.llvm_underlying_type_by_id(&pointer_type_id, resolved_generics) else { todo!() };
                let llvm_type = self.llvm_ptr_wrap_type_if_needed(llvm_type);

                let count_arg = arguments.first().expect("Pointer.malloc has arity 1");
                let count_arg_value = if let Some(count_arg) = count_arg {
                    self.visit_expression(count_arg).expect("Pointer.malloc's count parameter is not of type Unit")
                } else {
                    self.const_i64(1).as_basic_value_enum()
                };
                let ptr_size = match pointer_ty {
                    Type::Primitive(PrimitiveType::Int) => 8,
                    Type::Primitive(PrimitiveType::Float) => 8,
                    Type::Primitive(PrimitiveType::Bool) => 1,
                    _ => 8 // If not a primitive type, allocate enough space for a pointer (since non-primitive objects are pointers under the hood)
                };
                let ptr_size = self.const_i64(ptr_size);
                let malloc_amount_val = self.builder.build_int_mul(count_arg_value.into_int_value(), ptr_size, "malloc_amt");

                let malloc = self.main_module.get_function("malloc").expect("Function 'malloc' should have been defined");
                let ptr = self.builder.build_pointer_cast(
                    self.builder.build_call(malloc, &[malloc_amount_val.into()], "").try_as_basic_value().left().unwrap().into_pointer_value(),
                    self.ptr(llvm_type),
                    "ptr",
                );
                Some(ptr.as_basic_value_enum())
            }
            "pointer_address" => { // Instance method
                let instance_node = implicit_argument.expect("pointer_address is an instance method and will have an implicit argument");
                let instance_value = self.visit_expression(instance_node).expect("Instance is not of type Unit").into_pointer_value();
                let pointer_address = self.builder.build_ptr_to_int(instance_value, self.i64(), "address");

                Some(pointer_address.as_basic_value_enum())
            }
            "pointer_store" => { // Instance method
                let instance_node = implicit_argument.expect("pointer_store is an instance method and will have an implicit argument");
                let value_arg = arguments.first().expect("Pointer#store has arity 2").as_ref().expect("Pointer#store has 1 required non-implicit argument");

                let ptr = self.visit_expression(instance_node).expect("Instance is not of type Unit").into_pointer_value();
                let value = self.visit_expression(value_arg).expect("Instance is not of type Unit");
                self.builder.build_store(ptr, value);

                None
            }
            "pointer_load" => { // Instance method
                let instance_node = implicit_argument.expect("pointer_load is an instance method and will have an implicit argument");

                let ptr = self.visit_expression(instance_node).expect("Instance is not of type Unit").into_pointer_value();
                Some(self.builder.build_load(ptr, "").as_basic_value_enum())
            }
            "pointer_offset" => { // Instance method
                let instance_node = implicit_argument.expect("pointer_offset is an instance method and will have an implicit argument");
                let offset_arg = arguments.first().expect("Pointer#offset has arity 2").as_ref().expect("Pointer#offset has 1 required non-implicit argument");

                let ptr = self.visit_expression(instance_node).expect("Instance is not of type Unit").into_pointer_value();
                let value = self.visit_expression(offset_arg).expect("Instance is not of type Unit").into_int_value();

                let offset_ptr = unsafe { self.builder.build_gep(ptr, &[value], "offset") };
                Some(offset_ptr.as_basic_value_enum())
            }
            _ => unimplemented!("Unimplemented intrinsic '{}'", name),
        }
    }

    fn get_or_compile_function(&mut self, func_id: &FuncId, resolved_generics: &HashMap<String, TypeId>) -> FunctionValue<'a> {
        let fn_sig = self.llvm_function_signature(func_id, &resolved_generics);
        let llvm_fn = if let Some(function_val) = self.main_module.get_function(&fn_sig) {
            function_val
        } else {
            let function_val = self.compile_function(&func_id, &resolved_generics);
            debug_assert!(function_val.get_name().to_str().unwrap() == &fn_sig);
            function_val
        };

        llvm_fn
    }

    fn compile_function(&mut self, func_id: &FuncId, resolved_generics: &HashMap<String, TypeId>) -> FunctionValue<'a> {
        let function = self.project.get_func_by_id(func_id);
        let has_return_value = function.return_type_id != PRELUDE_UNIT_TYPE_ID;

        let fn_sig = self.llvm_function_signature(func_id, resolved_generics);
        let fn_type = self.llvm_function_type(func_id, &resolved_generics);
        let llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);

        let prev_fn = self.current_fn;
        self.current_fn = llvm_fn;
        self.ctx_stack.push(CompilerContext { resolved_generics: resolved_generics.clone() });

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let mut params_iter = llvm_fn.get_param_iter();
        for (idx, param) in function.params.iter().enumerate() {
            params_iter.next().unwrap().set_name(&param.name);
            let llvm_param = llvm_fn.get_nth_param(idx as u32).unwrap();
            self.variables.insert(param.var_id, LLVMVar::Param(llvm_param));
        }

        let num_nodes = function.body.len();
        for (idx, node) in function.body.iter().enumerate() {
            let res = self.visit_expression(node);
            if idx == num_nodes - 1 {
                if has_return_value {
                    self.builder.build_return(Some(&res.unwrap()));
                } else {
                    self.builder.build_return(None);
                }
            }
        }

        self.ctx_stack.pop();
        self.current_fn = prev_fn;
        self.builder.position_at_end(self.current_fn.get_last_basic_block().expect("Each function is declared with at least 1 basic block"));

        llvm_fn
    }

    fn compile_struct_type(&self, type_id: &TypeId, generic_substitutions: &HashMap<String, TypeId>) -> StructType<'a> {
        let Type::GenericInstance(struct_id, generics) = self.project.get_type_by_id(type_id) else { todo!() };
        let type_name = self.llvm_type_name_by_id(type_id, generic_substitutions);
        let llvm_type = self.context.opaque_struct_type(&type_name);

        let generic_substitutions = generics.iter()
            .map(|instance_generic_id| {
                let generic_name = self.get_generic_name(instance_generic_id);
                let resolved_generic = generic_substitutions.get(generic_name).unwrap_or(&instance_generic_id);
                (generic_name.clone(), *resolved_generic)
            })
            .collect();

        let struct_ = self.project.get_struct_by_id(struct_id);
        let field_types = struct_.fields.iter()
            .map(|field| {
                let Some(field_llvm_type) = self.llvm_underlying_type_by_id(&field.type_id, &generic_substitutions) else { todo!() };
                self.llvm_ptr_wrap_type_if_needed(field_llvm_type).into()
            });

        llvm_type.set_body(field_types.collect_vec().as_slice(), false);

        llvm_type
    }

    fn compile_type_initializer(&mut self, struct_id: &StructId, resolved_generics: &HashMap<String, TypeId>) -> FunctionValue<'a> {
        let struct_ = self.project.get_struct_by_id(struct_id);
        let initializer_sig = self.llvm_initializer_signature(struct_id, resolved_generics);

        let fn_type = self.llvm_initializer_type(struct_id, resolved_generics);
        let llvm_fn = self.main_module.add_function(&initializer_sig, fn_type, None);
        let prev_fn = self.current_fn;
        self.current_fn = llvm_fn;

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let Some(llvm_struct_type) = self.llvm_underlying_type_by_id(&struct_.self_type_id, resolved_generics) else { todo!() };
        let malloc = self.main_module.get_function("malloc").unwrap();
        let sizeof_struct = self.sizeof_struct(llvm_struct_type);
        let mem = self.builder.build_pointer_cast(
            self.builder.build_call(malloc, &[sizeof_struct.into()], "").try_as_basic_value().left().unwrap().into_pointer_value(),
            self.ptr(llvm_struct_type),
            "mem",
        );

        let mut params_iter = llvm_fn.get_param_iter();
        for (idx, field) in struct_.fields.iter().enumerate() {
            params_iter.next().unwrap().set_name(&field.name);
            let field_value = llvm_fn.get_nth_param(idx as u32).unwrap();
            let field_ptr = self.builder.build_struct_gep(mem, idx as u32, &format!("{}_slot", &field.name)).unwrap();
            self.builder.build_store(field_ptr, field_value);
        }

        self.builder.build_return(Some(&mem.as_basic_value_enum()));

        self.current_fn = prev_fn;
        self.builder.position_at_end(self.current_fn.get_last_basic_block().expect("Each function is declared with at least 1 basic block"));

        llvm_fn
    }
}

#[cfg(test)]
mod tests {
    use std::env::temp_dir;
    use std::path::Path;
    use std::process::Command;
    use assert_cmd::cargo::CommandCargoExt;
    use itertools::{EitherOrBoth, Itertools};
    use abra_core::common::util::random_string;
    use crate::get_project_root;

    fn run_test_file(file_name: &str) {
        let rust_project_root = get_project_root().unwrap();

        let tests_file_path = rust_project_root.join("abra_llvm").join("tests");
        let test_file_path = tests_file_path.join(file_name);

        let test_file = std::fs::read_to_string(&test_file_path).unwrap();
        let build_dir = if let Some(test_temp_dir) = std::env::var("TEST_TMP_DIR").ok() {
            let dir = Path::new(&test_temp_dir).join(random_string(12));
            std::fs::create_dir(&dir).unwrap();
            dir
        } else {
            temp_dir()
        };
        eprintln!("running test {}, using build dir: {}", file_name, &build_dir.to_str().unwrap());
        let output = Command::cargo_bin("abra").unwrap()
            .arg("build")
            .arg("--run")
            .arg(&test_file_path)
            .arg("-o")
            .arg(file_name.replace(".abra", ""))
            .arg("-b")
            .arg(build_dir)
            .output()
            .unwrap();
        assert!(output.stderr.is_empty(), "Compilation error: {}", String::from_utf8(output.stderr).unwrap());

        let output = String::from_utf8(output.stdout).unwrap();
        let output = output.lines();

        let prefix = "/// Expect: ";
        let expectations = test_file.lines()
            .map(|line| line.trim())
            .enumerate()
            .filter(|(_, line)| line.starts_with(prefix))
            .map(|(line_num, line)| (line_num + 1, line.replace(prefix, "")))
            .collect_vec();

        for pair in expectations.iter().zip_longest(output) {
            match pair {
                EitherOrBoth::Both((line_num, expected), actual) => {
                    assert_eq!(expected, actual, "Expectation mismatch at {}:{}", test_file_path.to_str().unwrap(), line_num);
                }
                EitherOrBoth::Left((line_num, expected)) => {
                    assert!(false, "Expected: {} (line {}), but reached end of output", expected, line_num);
                }
                EitherOrBoth::Right(actual) => {
                    assert!(false, "Received line: {}, but there were no more expectations", actual);
                }
            }
        }
    }

    #[test]
    fn test_ints() {
        run_test_file("ints.abra");
    }

    #[test]
    fn test_floats() {
        run_test_file("floats.abra");
    }

    #[test]
    fn test_bools() {
        run_test_file("bools.abra");
    }

    #[test]
    fn test_strings() {
        run_test_file("strings.abra");
    }
}
