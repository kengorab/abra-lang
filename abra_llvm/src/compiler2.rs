use std::collections::HashMap;
use std::convert::TryFrom;
use std::path::PathBuf;
use std::process::{Command, ExitStatus};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::TargetMachine;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FloatType, FunctionType, IntType, PointerType, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, CallableValue, FloatValue, FunctionValue, InstructionOpcode, IntValue, PointerValue, StructValue};
use itertools::Itertools;
use abra_core::lexer::tokens::Token;
use abra_core::parser::ast::{BinaryOp, BindingPattern};
use abra_core::typechecker::typechecker2::{AccessorKind, AssignmentKind, FuncId, FunctionKind, PRELUDE_ANY_TYPE_ID, PRELUDE_BOOL_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_INT_TYPE_ID, PRELUDE_MODULE_ID, PRELUDE_SCOPE_ID, PRELUDE_STRING_TYPE_ID, PRELUDE_UNIT_TYPE_ID, PrimitiveType, Project, StructId, Type, TypedLiteral, TypedNode, TypeId, TypeKind, VariableAlias, VarId};

const ABRA_MAIN_FN_NAME: &str = "_abra_main";

#[derive(Debug, Default)]
struct CompilerContext<'ctx> {
    variables: HashMap<VarId, LLVMVar<'ctx>>,
    resolved_generics: HashMap<String, TypeId>,
}

#[derive(Debug)]
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
    ctx_stack: Vec<CompilerContext<'a>>,

    // cached for convenience
    string_type: StructType<'a>,
    malloc: FunctionValue<'a>,
    snprintf: FunctionValue<'a>,
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
            // context.i32_type().into(), // type_id
            // context.i32_type().into(), // bytesize
            context.i64_type().into(), // length
            context.i8_type().ptr_type(AddressSpace::Generic).into(), // _buffer
        ], false);

        let malloc = main_module.add_function(
            "malloc",
            context.i8_type().ptr_type(AddressSpace::Generic).fn_type(&[context.i64_type().into()], false),
            None,
        );
        let snprintf = main_module.add_function(
            "snprintf",
            context.i64_type().fn_type(&[context.i8_type().ptr_type(AddressSpace::Generic).into(), context.i32_type().into(), context.i8_type().ptr_type(AddressSpace::Generic).into()], true),
            None,
        );

        Self {
            project,
            context,
            builder,
            main_module,
            current_fn: abra_main_fn,
            ctx_stack: vec![CompilerContext::default()],

            // cached values
            string_type,
            malloc,
            snprintf,
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
        let sizeof_struct = unsafe { self.builder.build_gep(self.null_ptr().const_cast(self.ptr(t)), &[self.const_i32(1).into()], "") };
        self.builder.build_ptr_to_int(sizeof_struct, self.i64(), "")
    }

    fn llvm_type_name_by_id(&self, type_id: &TypeId, resolved_generics: &HashMap<String, TypeId>) -> String {
        let ty = self.project.get_type_by_id(type_id);
        self.llvm_type_name_by_type(ty, resolved_generics)
    }

    fn llvm_type_name_by_type(&self, ty: &Type, resolved_generics: &HashMap<String, TypeId>) -> String {
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
        self.llvm_underlying_type_by_type(ty, resolved_generics)
    }

    fn llvm_underlying_type_by_type(&self, ty: &Type, resolved_generics: &HashMap<String, TypeId>) -> Option<BasicTypeEnum<'a>> {
        let llvm_type = match ty {
            Type::Primitive(PrimitiveType::Unit) => return None,
            Type::Primitive(PrimitiveType::Any) => {
                let resolved_generics = &self.ctx_stack.last().unwrap().resolved_generics;
                self.get_or_make_trait_struct_type(&PRELUDE_ANY_TYPE_ID, resolved_generics).0.as_basic_type_enum()
            }
            Type::Primitive(PrimitiveType::Int) => self.i64().as_basic_type_enum(),
            Type::GenericInstance(struct_id, _) if struct_id == &self.project.prelude_int_struct_id => self.i64().as_basic_type_enum(),
            Type::Primitive(PrimitiveType::Float) => self.f64().as_basic_type_enum(),
            Type::GenericInstance(struct_id, _) if struct_id == &self.project.prelude_float_struct_id => self.f64().as_basic_type_enum(),
            Type::Primitive(PrimitiveType::Bool) => self.bool().as_basic_type_enum(),
            Type::GenericInstance(struct_id, _) if struct_id == &self.project.prelude_bool_struct_id => self.bool().as_basic_type_enum(),
            Type::Primitive(PrimitiveType::String) => self.string_type.as_basic_type_enum(),
            Type::Generic(_, name) => {
                return resolved_generics.get(name)
                    .and_then(|resolved_type_id| self.llvm_underlying_type_by_id(resolved_type_id, resolved_generics));
            }
            Type::GenericInstance(_, generics) => {
                let type_name = self.llvm_type_name_by_type(ty, resolved_generics);

                // Handle special cases
                if type_name == "Byte" {
                    // `Byte` is an alias for 8-bit unsigned integer
                    return Some(self.i8().as_basic_type_enum());
                } else if type_name.starts_with("Pointer<") {
                    // `Pointer<T>` is an alias for `*T`
                    let ptr_generic_id = &generics[0];
                    let Some(ptr_llvm_type) = self.llvm_underlying_type_by_id(ptr_generic_id, resolved_generics) else { todo!() };
                    let ptr_llvm_type = self.llvm_ptr_wrap_type_if_needed(ptr_llvm_type);
                    return Some(self.ptr(ptr_llvm_type).as_basic_type_enum());
                }

                if let Some(llvm_type) = self.main_module.get_struct_type(&type_name) {
                    llvm_type.as_basic_type_enum()
                } else {
                    let struct_type = self.compile_struct_type_by_type(ty, resolved_generics);
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
        if llvm_type.is_struct_type() {
            let llvm_struct_type = llvm_type.into_struct_type();
            let llvm_type_name = llvm_struct_type.get_name()
                .and_then(|name| name.to_str().ok())
                .unwrap_or("");

            if llvm_type_name.starts_with("Option<") || llvm_type_name == "Any" {
                llvm_type
            } else {
                self.ptr(llvm_type).as_basic_type_enum()
            }
        } else {
            llvm_type
        }
    }

    fn llvm_function_signature(&self, func_id: &FuncId, resolved_generics: &HashMap<String, TypeId>) -> String {
        let function = self.project.get_func_by_id(func_id);

        let type_args = function.generic_ids.iter()
            .map(|type_id| self.llvm_type_name_by_id(type_id, resolved_generics))
            .join(",");
        let type_args = if function.generic_ids.is_empty() { "".into() } else { format!("<{}>", type_args) };
        let params = function.params.iter()
            .map(|p| {
                let param_type_name = self.llvm_type_name_by_id(&p.type_id, resolved_generics);
                if p.is_variadic {
                    format!("Array<{param_type_name}>")
                } else {
                    param_type_name
                }
            })
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
                let llvm_type = if p.is_variadic {
                    let wrapped_type = self.project.array_type(p.type_id);
                    let Some(llvm_type) = self.llvm_underlying_type_by_type(&wrapped_type, resolved_generics) else { todo!() };
                    llvm_type
                } else {
                    let Some(llvm_type) = self.llvm_underlying_type_by_id(&p.type_id, resolved_generics) else { todo!() };
                    llvm_type
                };
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

    fn malloc<T: BasicValue<'a>>(&self, malloc_size: T, target_type: PointerType<'a>) -> PointerValue<'a> {
        let malloc_size = malloc_size.as_basic_value_enum();
        let mem = self.builder.build_call(self.malloc, &[malloc_size.into()], "").try_as_basic_value().left().unwrap().into_pointer_value();
        self.builder.build_pointer_cast(mem, target_type, "ptr")
    }

    fn memcpy(&self, dst: PointerValue<'a>, src: PointerValue<'a>, len: IntValue<'a>) {
        let memcpy = self.main_module.get_function("llvm.memcpy.p0.p0.i64").unwrap_or_else(|| {
            // No support for opaque `ptr` llvm type; just use i64* and cast each time
            let i64_ptr = self.ptr(self.i64());
            self.main_module.add_function("llvm.memcpy.p0.p0.i64", self.context.void_type().fn_type(&[i64_ptr.into(), i64_ptr.into(), self.i64().into(), self.bool().into()], false), None)
        });
        let dst = self.builder.build_pointer_cast(dst, self.ptr(self.i64()), "");
        let src = self.builder.build_pointer_cast(src, self.ptr(self.i64()), "");
        self.builder.build_call(memcpy, &[dst.into(), src.into(), len.into(), self.const_bool(false).into()], "").try_as_basic_value().left();
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
                        let node_type = self.project.get_type_by_id(node_type_id);
                        let (_, tostring_func_id) = node_type.find_method_by_name(self.project, "toString").unwrap();
                        let (_, generics) = self.project.get_struct_by_type_id(node_type_id).unwrap();
                        let resolved_generics = generics.iter().map(|(k, v)| (self.get_generic_name(k).clone(), *v)).collect();
                        let to_string_fn = self.get_or_compile_function(tostring_func_id, &resolved_generics);

                        let str_val = self.builder.build_call(to_string_fn, &[res.into()], "repr").try_as_basic_value().left().unwrap().into_pointer_value();
                        let len_val_ptr = self.builder.build_struct_gep(str_val.const_cast(self.ptr(self.string_type)), 0, "").unwrap();
                        let len_val = self.builder.build_load(len_val_ptr, "repr.length");
                        let chars_val_ptr = self.builder.build_struct_gep(str_val.const_cast(self.ptr(self.string_type)), 1, "").unwrap();
                        let chars_val = self.builder.build_load(chars_val_ptr, "repr.chars");

                        let fmt_str = self.builder.build_global_string_ptr("%.*s", "").as_basic_value_enum();
                        self.builder.build_call(printf, &[fmt_str.into(), len_val.into(), chars_val.into()], "").try_as_basic_value().left().unwrap();
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

    fn visit_statement(&mut self, node: &TypedNode) -> Option<BasicValueEnum<'a>> {
        match node {
            node @ TypedNode::If { .. } => self.visit_if_node(node),
            TypedNode::Match { .. } => todo!(),
            TypedNode::FuncDeclaration(_) => None,
            TypedNode::TypeDeclaration(_) => None,
            TypedNode::EnumDeclaration(_) => None,
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
                    self.ctx_stack.last_mut().unwrap().variables.insert(variable.id, LLVMVar::Slot(slot));

                    self.builder.build_store(slot, expr_val);
                }

                None
            }
            TypedNode::ForLoop { .. } => todo!(),
            TypedNode::WhileLoop { condition, condition_var_id, body, .. } => {
                debug_assert!(condition_var_id.is_none(), "Not implemented yet");
                debug_assert!(condition.as_ref().type_id() == &PRELUDE_BOOL_TYPE_ID, "Only implement while loops for boolean conditions for now (no Optionals yet)");

                let loop_cond_block = self.context.append_basic_block(self.current_fn, "while_loop_cond");
                let loop_body_block = self.context.append_basic_block(self.current_fn, "while_loop_body");
                let loop_end_block = self.context.append_basic_block(self.current_fn, "while_loop_end");

                self.builder.build_unconditional_branch(loop_cond_block);
                self.builder.position_at_end(loop_cond_block);
                let cond_val = self.visit_expression(condition).unwrap().into_int_value();
                let comp = self.builder.build_int_compare(IntPredicate::EQ, cond_val, self.const_bool(true), "");
                self.builder.build_conditional_branch(comp, loop_body_block, loop_end_block);

                self.builder.position_at_end(loop_body_block);
                for node in body {
                    self.visit_statement(node);
                }
                self.builder.build_unconditional_branch(loop_cond_block);

                self.builder.position_at_end(loop_end_block);

                None
            }
            TypedNode::Break { .. } |
            TypedNode::Continue { .. } |
            TypedNode::Return { .. } => todo!(),
            TypedNode::Import { .. } => None,
            _ => self.visit_expression(node),
        }
    }

    fn visit_expression(&mut self, node: &TypedNode) -> Option<BasicValueEnum<'a>> {
        match node {
            TypedNode::Literal { value, type_id, resolved_type_id, .. } => {
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
                        let str_len = self.const_i64(s.len() as u64);
                        let string_new_fn = self.get_or_compile_type_initializer(&self.project.prelude_string_struct_id, &HashMap::new());
                        self.builder.build_call(string_new_fn, &[str_len.into(), str.into()], "").try_as_basic_value().left().unwrap()
                    }
                };

                let resolved_generics = &self.ctx_stack.last().unwrap().resolved_generics;
                if self.project.type_is_option(resolved_type_id).is_some() {
                    return Some(self.make_option_instance(resolved_type_id, value, resolved_generics).into());
                } else if resolved_type_id == &PRELUDE_ANY_TYPE_ID {
                    let resolved_generics = resolved_generics.clone();
                    return Some(self.make_trait_instance(&PRELUDE_ANY_TYPE_ID, &type_id, value, &resolved_generics).into());
                }

                Some(value)
            }
            TypedNode::Unary { .. } => todo!(),
            TypedNode::Binary { left, op, right, type_id, resolved_type_id, .. } => {
                let left_type_id = left.as_ref().type_id();
                let right_type_id = right.as_ref().type_id();

                let value = match op {
                    BinaryOp::Add => {
                        let left_val = self.visit_expression(left).unwrap();
                        let right_val = self.visit_expression(right).unwrap();
                        if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            self.builder.build_int_add(left_val.into_int_value(), right_val.into_int_value(), "").into()
                        } else if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_FLOAT_TYPE_ID {
                            let left = self.builder.build_signed_int_to_float(left_val.into_int_value(), self.f64(), "");
                            self.builder.build_float_add(left, right_val.into_float_value(), "").into()
                        } else if left_type_id == &PRELUDE_FLOAT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            let right = self.builder.build_signed_int_to_float(right_val.into_int_value(), self.f64(), "");
                            self.builder.build_float_add(left_val.into_float_value(), right, "").into()
                        } else if left_type_id == &PRELUDE_FLOAT_TYPE_ID && right_type_id == &PRELUDE_FLOAT_TYPE_ID {
                            self.builder.build_float_add(left_val.into_float_value(), right_val.into_float_value(), "").into()
                        } else if left_type_id == &PRELUDE_STRING_TYPE_ID || right_type_id == &PRELUDE_STRING_TYPE_ID {
                            let string_ty = self.project.get_type_by_id(&PRELUDE_STRING_TYPE_ID);
                            let (member_idx, func_id) = string_ty.find_method_by_name(self.project, "concat").unwrap();
                            let function = self.project.get_func_by_id(func_id);

                            let string_concat_target = if left_type_id == &PRELUDE_STRING_TYPE_ID {
                                left.clone()
                            } else {
                                let right_ty = self.project.get_type_by_id(right_type_id);
                                let (tostring_member_idx, tostring_func_id) = right_ty.find_method_by_name(self.project, "toString").unwrap();
                                let tostring_func = self.project.get_func_by_id(tostring_func_id);

                                Box::new(TypedNode::Invocation {
                                    target: Box::new(TypedNode::Accessor {
                                        target: left.clone(),
                                        kind: AccessorKind::Method,
                                        is_opt_safe: false,
                                        member_idx: tostring_member_idx,
                                        member_span: left.span(),
                                        type_id: tostring_func.fn_type_id,
                                        type_arg_ids: vec![],
                                        resolved_type_id: tostring_func.fn_type_id,
                                    }),
                                    arguments: vec![],
                                    type_arg_ids: vec![],
                                    type_id: PRELUDE_STRING_TYPE_ID,
                                    resolved_type_id: PRELUDE_STRING_TYPE_ID,
                                })
                            };

                            return self.visit_expression(&TypedNode::Invocation {
                                target: Box::new(TypedNode::Accessor {
                                    target: string_concat_target,
                                    kind: AccessorKind::Method,
                                    is_opt_safe: false,
                                    member_idx,
                                    member_span: left.span(),
                                    type_id: function.fn_type_id,
                                    type_arg_ids: vec![(*right_type_id, left.span())],
                                    resolved_type_id: function.fn_type_id,
                                }),
                                arguments: vec![Some(*right.clone())],
                                type_arg_ids: vec![*right_type_id],
                                type_id: PRELUDE_STRING_TYPE_ID,
                                resolved_type_id: *resolved_type_id,
                            });
                        } else {
                            unreachable!("`+` operator not defined between types {} and {}", self.project.type_repr(left_type_id), self.project.type_repr(right_type_id))
                        }
                    }
                    BinaryOp::Sub => {
                        let left = self.visit_expression(left).unwrap();
                        let right = self.visit_expression(right).unwrap();
                        if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            self.builder.build_int_sub(left.into_int_value(), right.into_int_value(), "").into()
                        } else {
                            let (left, right) = if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_FLOAT_TYPE_ID {
                                let left = self.builder.build_signed_int_to_float(left.into_int_value(), self.f64(), "");
                                (left, right.into_float_value())
                            } else if left_type_id == &PRELUDE_FLOAT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                                let right = self.builder.build_signed_int_to_float(right.into_int_value(), self.f64(), "");
                                (left.into_float_value(), right)
                            } else if left_type_id == &PRELUDE_FLOAT_TYPE_ID && right_type_id == &PRELUDE_FLOAT_TYPE_ID {
                                (left.into_float_value(), right.into_float_value())
                            } else {
                                unreachable!("`-` operator not defined between types {} and {}", self.project.type_repr(left_type_id), self.project.type_repr(right_type_id))
                            };
                            self.builder.build_float_sub(left, right, "").into()
                        }
                    }
                    BinaryOp::Mul => {
                        let left = self.visit_expression(left).unwrap();
                        let right = self.visit_expression(right).unwrap();
                        if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            self.builder.build_int_mul(left.into_int_value(), right.into_int_value(), "").into()
                        } else {
                            let (left, right) = if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_FLOAT_TYPE_ID {
                                let left = self.builder.build_signed_int_to_float(left.into_int_value(), self.f64(), "");
                                (left, right.into_float_value())
                            } else if left_type_id == &PRELUDE_FLOAT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                                let right = self.builder.build_signed_int_to_float(right.into_int_value(), self.f64(), "");
                                (left.into_float_value(), right)
                            } else if left_type_id == &PRELUDE_FLOAT_TYPE_ID && right_type_id == &PRELUDE_FLOAT_TYPE_ID {
                                (left.into_float_value(), right.into_float_value())
                            } else {
                                unreachable!("`*` operator not defined between types {} and {}", self.project.type_repr(left_type_id), self.project.type_repr(right_type_id))
                            };
                            self.builder.build_float_mul(left, right, "").into()
                        }
                    }
                    BinaryOp::Div => {
                        let left = self.visit_expression(left).unwrap();
                        let right = self.visit_expression(right).unwrap();
                        let (left, right) = if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            let left = self.builder.build_signed_int_to_float(left.into_int_value(), self.f64(), "");
                            let right = self.builder.build_signed_int_to_float(right.into_int_value(), self.f64(), "");
                            (left, right)
                        } else if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_FLOAT_TYPE_ID {
                            let left = self.builder.build_signed_int_to_float(left.into_int_value(), self.f64(), "");
                            (left, right.into_float_value())
                        } else if left_type_id == &PRELUDE_FLOAT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            let right = self.builder.build_signed_int_to_float(right.into_int_value(), self.f64(), "");
                            (left.into_float_value(), right)
                        } else if left_type_id == &PRELUDE_FLOAT_TYPE_ID && right_type_id == &PRELUDE_FLOAT_TYPE_ID {
                            (left.into_float_value(), right.into_float_value())
                        } else {
                            unreachable!("`/` operator not defined between types {} and {}", self.project.type_repr(left_type_id), self.project.type_repr(right_type_id))
                        };
                        self.builder.build_float_div(left, right, "").into()
                    }
                    BinaryOp::Mod |
                    BinaryOp::And |
                    BinaryOp::Or |
                    BinaryOp::Xor |
                    BinaryOp::Coalesce => todo!(),
                    op @ BinaryOp::Lt | op @ BinaryOp::Lte | op @ BinaryOp::Gt | op @ BinaryOp::Gte => {
                        let comp_op_int = if op == &BinaryOp::Lt { IntPredicate::SLT } else if op == &BinaryOp::Lte { IntPredicate::SLE } else if op == &BinaryOp::Gt { IntPredicate::SGT } else if op == &BinaryOp::Gte { IntPredicate::SGE } else { unreachable!() };
                        let comp_op_float = if op == &BinaryOp::Lt { FloatPredicate::OLT } else if op == &BinaryOp::Lte { FloatPredicate::OLE } else if op == &BinaryOp::Gt { FloatPredicate::OGT } else if op == &BinaryOp::Gte { FloatPredicate::OGE } else { unreachable!() };

                        let left = self.visit_expression(left).unwrap();
                        let right = self.visit_expression(right).unwrap();
                        if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            self.builder.build_int_compare(comp_op_int, left.into_int_value(), right.into_int_value(), "").into()
                        } else if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_FLOAT_TYPE_ID {
                            let left = self.builder.build_signed_int_to_float(left.into_int_value(), self.f64(), "");
                            self.builder.build_float_compare(comp_op_float, left, right.into_float_value(), "").into()
                        } else if left_type_id == &PRELUDE_FLOAT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            let right = self.builder.build_signed_int_to_float(right.into_int_value(), self.f64(), "");
                            self.builder.build_float_compare(comp_op_float, left.into_float_value(), right, "").into()
                        } else if left_type_id == &PRELUDE_FLOAT_TYPE_ID && right_type_id == &PRELUDE_FLOAT_TYPE_ID {
                            self.builder.build_float_compare(comp_op_float, left.into_float_value(), right.into_float_value(), "").into()
                        } else {
                            unreachable!("`{}` operator not defined between types {} and {}", op.repr(), self.project.type_repr(left_type_id), self.project.type_repr(right_type_id))
                        }
                    }
                    op @ BinaryOp::Neq | op @ BinaryOp::Eq => {
                        let comp_op_int = if op == &BinaryOp::Neq { IntPredicate::NE } else { IntPredicate::EQ };
                        let comp_op_float = if op == &BinaryOp::Neq { FloatPredicate::ONE } else { FloatPredicate::OEQ };

                        let left = self.visit_expression(left).unwrap();
                        let right = self.visit_expression(right).unwrap();
                        if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            self.builder.build_int_compare(comp_op_int, left.into_int_value(), right.into_int_value(), "").into()
                        } else if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_FLOAT_TYPE_ID {
                            let left = self.builder.build_signed_int_to_float(left.into_int_value(), self.f64(), "");
                            self.builder.build_float_compare(comp_op_float, left, right.into_float_value(), "").into()
                        } else if left_type_id == &PRELUDE_FLOAT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            let right = self.builder.build_signed_int_to_float(right.into_int_value(), self.f64(), "");
                            self.builder.build_float_compare(comp_op_float, left.into_float_value(), right, "").into()
                        } else if left_type_id == &PRELUDE_FLOAT_TYPE_ID && right_type_id == &PRELUDE_FLOAT_TYPE_ID {
                            self.builder.build_float_compare(comp_op_float, left.into_float_value(), right.into_float_value(), "").into()
                        } else {
                            todo!()
                        }
                    }
                    BinaryOp::Pow => todo!(),
                    BinaryOp::AddEq |
                    BinaryOp::SubEq |
                    BinaryOp::MulEq |
                    BinaryOp::DivEq |
                    BinaryOp::ModEq |
                    BinaryOp::AndEq |
                    BinaryOp::OrEq |
                    BinaryOp::CoalesceEq => unreachable!("Handled in ::Assignment")
                };

                let resolved_generics = &self.ctx_stack.last().unwrap().resolved_generics;
                if self.project.type_is_option(resolved_type_id).is_some() {
                    Some(self.make_option_instance(resolved_type_id, value, resolved_generics).into())
                } else if resolved_type_id == &PRELUDE_ANY_TYPE_ID {
                    let resolved_generics = resolved_generics.clone();
                    return Some(self.make_trait_instance(&PRELUDE_ANY_TYPE_ID, &type_id, value, &resolved_generics).into());
                } else {
                    Some(value)
                }
            }
            TypedNode::Grouped { expr, .. } => self.visit_expression(expr),
            TypedNode::Array { items, type_id, resolved_type_id, .. } => {
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
                    let (_, array_push_func_id) = array_type.find_method_by_name(self.project, "push").unwrap();
                    let mut resolved_generics = HashMap::new();
                    resolved_generics.insert(self.get_generic_name(&array_struct.generic_ids[0]).clone(), inner_type_id);
                    self.get_or_compile_function(&array_push_func_id, &resolved_generics)
                };

                let arr_val = self.builder.build_call(array_with_capacity_llvm_fn, &[self.const_i64(items.len() as u64).into()], "").try_as_basic_value().left().unwrap();
                for item in items {
                    let item = self.visit_expression(item).unwrap();
                    self.builder.build_call(array_push_llvm_fn, &[arr_val.into(), item.into()], "").try_as_basic_value().left();
                }

                let resolved_generics_in_scope = &self.ctx_stack.last().unwrap().resolved_generics;
                if self.project.type_is_option(resolved_type_id).is_some() {
                    let arr_val_local = self.builder.build_alloca(arr_val.get_type(), "");
                    self.builder.build_store(arr_val_local, arr_val);
                    let arr_val = self.builder.build_load(arr_val_local, "");
                    return Some(self.make_option_instance(resolved_type_id, arr_val, resolved_generics_in_scope).into());
                } else if resolved_type_id == &PRELUDE_ANY_TYPE_ID {
                    let mut resolved_generics = resolved_generics_in_scope.clone();
                    for (generic_type_id, type_id) in array_struct.generic_ids.iter().zip(generics) {
                        resolved_generics.insert(self.get_generic_name(generic_type_id).clone(), *type_id);
                    }

                    let arr_val_local = self.builder.build_alloca(arr_val.get_type(), "");
                    self.builder.build_store(arr_val_local, arr_val);
                    let arr_val = self.builder.build_load(arr_val_local, "");
                    return Some(self.make_trait_instance(&PRELUDE_ANY_TYPE_ID, &type_id, arr_val, &resolved_generics).into());
                }

                Some(arr_val)
            }
            TypedNode::Tuple { .. } |
            TypedNode::Set { .. } |
            TypedNode::Map { .. } => todo!(),
            TypedNode::Identifier { var_id, resolved_type_id, .. } => {
                let variable = self.project.get_var_by_id(var_id);
                let llvm_var = self.ctx_stack.last().unwrap().variables.get(&variable.id).expect(&format!("No stored slot for variable {} ({:?})", &variable.name, &variable));
                let value = match llvm_var {
                    LLVMVar::Slot(slot) => self.builder.build_load(*slot, &variable.name),
                    LLVMVar::Param(value) => *value,
                };

                let resolved_generics_in_scope = &self.ctx_stack.last().unwrap().resolved_generics;
                if self.project.type_is_option(&variable.type_id).is_none() && self.project.type_is_option(resolved_type_id).is_some() {
                    Some(self.make_option_instance(resolved_type_id, value, resolved_generics_in_scope).into())
                } else if resolved_type_id == &PRELUDE_ANY_TYPE_ID && variable.type_id != PRELUDE_ANY_TYPE_ID {
                    let variable_ty = self.project.get_type_by_id(&variable.type_id);
                    let mut resolved_generics = resolved_generics_in_scope.clone();
                    if let Type::GenericInstance(struct_id, generics) = variable_ty {
                        let struct_ = self.project.get_struct_by_id(struct_id);
                        for (generic_type_id, type_id) in struct_.generic_ids.iter().zip(generics) {
                            resolved_generics.insert(self.get_generic_name(generic_type_id).clone(), *type_id);
                        }
                    }
                    Some(self.make_trait_instance(&PRELUDE_ANY_TYPE_ID, &variable.type_id, value, &resolved_generics).into())
                } else {
                    Some(value)
                }
            }
            TypedNode::NoneValue { resolved_type_id, .. } => {
                let resolved_generics = &self.ctx_stack.last().unwrap().resolved_generics;
                Some(self.make_none_option_instance(resolved_type_id, &resolved_generics).into())
            }
            TypedNode::Invocation { target, arguments, type_arg_ids, type_id, resolved_type_id, .. } => {
                let mut args = Vec::with_capacity(arguments.len());

                let llvm_fn_val = match &**target {
                    // Handle invocation of identifiers which could be aliases for functions or types. Non-aliased variables (which could be function values)
                    // will be handled alongside other arbitrary expressions in the catchall block of this match.
                    TypedNode::Identifier { var_id, .. } if self.project.get_var_by_id(var_id).alias != VariableAlias::None => {
                        let variable = self.project.get_var_by_id(var_id);
                        match &variable.alias {
                            VariableAlias::None => unreachable!(),
                            VariableAlias::Function(func_id) => {
                                let function = self.project.get_func_by_id(func_id);
                                let resolved_generics_in_scope = &self.ctx_stack.last().expect("There should always be a context in ctx_stack").resolved_generics;
                                let resolved_generics = function.generic_ids.iter().zip(type_arg_ids)
                                    .map(|(generic_id, type_arg_id)| {
                                        if let Type::Generic(_, generic_name) = self.project.get_type_by_id(type_arg_id) {
                                            let type_arg_id = resolved_generics_in_scope.get(generic_name).unwrap_or(type_arg_id);
                                            (self.get_generic_name(generic_id).clone(), *type_arg_id)
                                        } else {
                                            (self.get_generic_name(generic_id).clone(), *type_arg_id)
                                        }
                                    })
                                    .collect();
                                if let Some(dec) = function.decorators.iter().find(|dec| dec.name == "Intrinsic") {
                                    let TypedNode::Literal { value: TypedLiteral::String(intrinsic_name), .. } = &dec.args[0] else { unreachable!("@Intrinsic requires exactly 1 String argument") };
                                    return self.compile_intrinsic_invocation(type_arg_ids, &resolved_generics, intrinsic_name, Some(&**target), arguments, resolved_type_id);
                                }

                                self.get_or_compile_function(func_id, &resolved_generics).into()
                            }
                            VariableAlias::Type(TypeKind::Enum(_)) => unreachable!("Cannot invoke an enum directly"),
                            VariableAlias::Type(TypeKind::Struct(struct_id)) => {
                                let struct_ = self.project.get_struct_by_id(struct_id);
                                let resolved_generics_in_scope = &self.ctx_stack.last().expect("There should always be a context in ctx_stack").resolved_generics;
                                let resolved_generics = struct_.generic_ids.iter().zip(type_arg_ids)
                                    .map(|(generic_id, type_arg_id)| {
                                        if let Type::Generic(_, generic_name) = self.project.get_type_by_id(type_arg_id) {
                                            let type_arg_id = resolved_generics_in_scope.get(generic_name).unwrap_or(type_arg_id);
                                            (self.get_generic_name(generic_id).clone(), *type_arg_id)
                                        } else {
                                            (self.get_generic_name(generic_id).clone(), *type_arg_id)
                                        }
                                    })
                                    .collect();
                                self.get_or_compile_type_initializer(struct_id, &resolved_generics).into()
                            }
                        }
                    }
                    TypedNode::Accessor { target, kind, member_idx, .. } => {
                        let mut target_type_id = target.type_id();
                        if let Type::Generic(_, generic_name) = self.project.get_type_by_id(target_type_id) {
                            target_type_id = self.ctx_stack.last().unwrap().resolved_generics.get(generic_name).unwrap_or(target_type_id)
                        };
                        let target_ty = self.project.get_type_by_id(target_type_id);

                        match kind {
                            AccessorKind::Field => todo!(),
                            AccessorKind::Method if self.project.type_is_option(target_type_id).is_some() => {
                                let Some(inner_type_id) = self.project.type_is_option(target_type_id) else { unreachable!() };

                                let target = self.visit_expression(target).unwrap();
                                args.push(target.into());

                                self.get_or_compile_option_method(&inner_type_id, member_idx).into()
                            }
                            AccessorKind::Method if self.project.type_is_trait(target_type_id) => {
                                let instance = self.visit_expression(target).unwrap();
                                let instance_local = self.builder.build_alloca(instance.get_type(), "instance_local");
                                self.builder.build_store(instance_local, instance);

                                let value_slot = self.builder.build_struct_gep(instance_local, 1, "value_slot").unwrap();
                                let underlying_value = self.builder.build_load(value_slot, "underlying_value");
                                args.push(underlying_value.into());

                                let vtable_slot = self.builder.build_struct_gep(instance_local, 0, "vtable_slot").unwrap();
                                let vtable_value = self.builder.build_load(vtable_slot, "vtable").into_pointer_value();
                                let method_slot = self.builder.build_struct_gep(vtable_value, (*member_idx + 1) as u32, "method_slot").unwrap();
                                let method_val = self.builder.build_load(method_slot, "method").into_pointer_value();

                                let fn_ptr_type = match *member_idx {
                                    0 => {
                                        let tostring_fn_type = self.fn_type(self.ptr(self.string_type), &[self.i64().into()]);
                                        tostring_fn_type.ptr_type(AddressSpace::Generic)
                                    }
                                    _ => unimplemented!("For now, the only trait is `Any`, and only the `toString` method is implemented")
                                };
                                let callable = CallableValue::try_from(self.builder.build_pointer_cast(method_val, fn_ptr_type, "")).unwrap();

                                callable
                            }
                            AccessorKind::Method => {
                                let func_id = target_ty.get_method(self.project, *member_idx).unwrap();
                                let function = self.project.get_func_by_id(&func_id);
                                if let Some(dec) = function.decorators.iter().find(|dec| dec.name == "Intrinsic") {
                                    let TypedNode::Literal { value: TypedLiteral::String(intrinsic_name), .. } = &dec.args[0] else { unreachable!("@Intrinsic requires exactly 1 String argument") };
                                    let resolved_generics = self.ctx_stack.last().unwrap().resolved_generics.clone();
                                    return self.compile_intrinsic_invocation(type_arg_ids, &resolved_generics, intrinsic_name, Some(&**target), arguments, resolved_type_id);
                                }

                                let target = self.visit_expression(target).unwrap();
                                args.push(target.into());

                                let FunctionKind::Method(type_id) = function.kind else { unreachable!() };
                                let Some((struct_, _)) = self.project.get_struct_by_type_id(&type_id) else { todo!() };
                                let resolved_generics = if let Type::GenericInstance(_, instance_generics) = target_ty {
                                    struct_.generic_ids.iter().zip(instance_generics)
                                        .map(|(generic_id, type_arg_id)| (self.get_generic_name(generic_id).clone(), *type_arg_id))
                                        .chain(function.generic_ids.iter().zip(type_arg_ids)
                                            .map(|(generic_id, type_arg_id)| {
                                                (self.get_generic_name(generic_id).clone(), *type_arg_id)
                                            })
                                        )
                                        .collect()
                                } else {
                                    function.generic_ids.iter().zip(type_arg_ids)
                                        .map(|(generic_id, type_arg_id)| {
                                            (self.get_generic_name(generic_id).clone(), *type_arg_id)
                                        })
                                        .collect()
                                };

                                self.get_or_compile_function(&func_id, &resolved_generics).into()
                            }
                            AccessorKind::StaticMethod => {
                                let func_id = target_ty.get_static_method(self.project, *member_idx).unwrap();
                                let function = self.project.get_func_by_id(&func_id);

                                let resolved_generics_in_scope = &self.ctx_stack.last().expect("There should always be a context in ctx_stack").resolved_generics;
                                let resolved_generics = function.generic_ids.iter().zip(type_arg_ids)
                                    .map(|(generic_id, type_arg_id)| {
                                        if let Type::Generic(_, generic_name) = self.project.get_type_by_id(type_arg_id) {
                                            let type_arg_id = resolved_generics_in_scope.get(generic_name).unwrap_or(type_arg_id);
                                            (self.get_generic_name(generic_id).clone(), *type_arg_id)
                                        } else {
                                            (self.get_generic_name(generic_id).clone(), *type_arg_id)
                                        }
                                    })
                                    .collect();

                                if let Some(dec) = function.decorators.iter().find(|dec| dec.name == "Intrinsic") {
                                    let TypedNode::Literal { value: TypedLiteral::String(intrinsic_name), .. } = &dec.args[0] else { unreachable!("@Intrinsic requires exactly 1 String argument") };
                                    return self.compile_intrinsic_invocation(type_arg_ids, &resolved_generics, intrinsic_name, None, arguments, resolved_type_id);
                                }

                                self.get_or_compile_function(&func_id, &resolved_generics).into()
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

                let value = self.builder.build_call(llvm_fn_val, args.as_slice(), "").try_as_basic_value().left();
                if let Some(value) = value {
                    let resolved_generics_in_scope = &self.ctx_stack.last().unwrap().resolved_generics;

                    if !self.project.type_is_option(type_id).is_some() && self.project.type_is_option(resolved_type_id).is_some() {
                        let value_local = self.builder.build_alloca(value.get_type(), "");
                        self.builder.build_store(value_local, value);
                        let value = self.builder.build_load(value_local, "");
                        return Some(self.make_option_instance(resolved_type_id, value, resolved_generics_in_scope).into());
                    } else if resolved_type_id == &PRELUDE_ANY_TYPE_ID {
                        let mut resolved_generics = resolved_generics_in_scope.clone();
                        if let Type::GenericInstance(struct_id, generics) = self.project.get_type_by_id(type_id) {
                            let struct_ = self.project.get_struct_by_id(struct_id);
                            for (generic_type_id, type_id) in struct_.generic_ids.iter().zip(generics) {
                                resolved_generics.insert(self.get_generic_name(generic_type_id).clone(), *type_id);
                            }
                        }
                        return Some(self.make_trait_instance(&PRELUDE_ANY_TYPE_ID, &type_id, value, &resolved_generics).into());
                    }
                }

                value
            }
            TypedNode::Accessor { target, kind, member_idx, is_opt_safe, type_id, resolved_type_id, .. } => {
                debug_assert!(!*is_opt_safe);

                let target_ty = self.project.get_type_by_id(target.type_id());
                let target = self.visit_expression(target).unwrap();

                let value = match kind {
                    AccessorKind::Field => {
                        let field = target_ty.get_field(self.project, *member_idx).unwrap();
                        let field_slot = self.builder.build_struct_gep(target.into_pointer_value(), *member_idx as u32, &field.name).unwrap();
                        self.builder.build_load(field_slot, "")
                    }
                    AccessorKind::Method |
                    AccessorKind::StaticMethod |
                    AccessorKind::EnumVariant => todo!()
                };

                let resolved_generics_in_scope = &self.ctx_stack.last().unwrap().resolved_generics;
                if !self.project.type_is_option(type_id).is_some() && self.project.type_is_option(resolved_type_id).is_some() {
                    let value_local = self.builder.build_alloca(value.get_type(), "");
                    self.builder.build_store(value_local, value);
                    let value = self.builder.build_load(value_local, "");
                    return Some(self.make_option_instance(resolved_type_id, value, resolved_generics_in_scope).into());
                } else if resolved_type_id == &PRELUDE_ANY_TYPE_ID {
                    let mut resolved_generics = resolved_generics_in_scope.clone();
                    if let Type::GenericInstance(struct_id, generics) = self.project.get_type_by_id(type_id) {
                        let struct_ = self.project.get_struct_by_id(struct_id);
                        for (generic_type_id, type_id) in struct_.generic_ids.iter().zip(generics) {
                            resolved_generics.insert(self.get_generic_name(generic_type_id).clone(), *type_id);
                        }
                    }
                    return Some(self.make_trait_instance(&PRELUDE_ANY_TYPE_ID, &type_id, value, &resolved_generics).into());
                }

                Some(value.into())
            }
            TypedNode::Indexing { .. } |
            TypedNode::Lambda { .. } => todo!(),
            TypedNode::Assignment { kind, expr, .. } => {
                let expr_val = self.visit_expression(expr).unwrap();

                match kind {
                    AssignmentKind::Identifier { var_id } => {
                        let variable = self.project.get_var_by_id(var_id);
                        let llvm_var = self.ctx_stack.last().unwrap().variables.get(var_id).expect(&format!("No known llvm variable for variable '{}' ({:?})", &variable.name, var_id));
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
            node @ TypedNode::If { .. } => self.visit_if_node(node),
            _ => unreachable!("Node {:?} is not an expression and should have been handled in visit_statement", node)
        }
    }

    fn visit_if_node(&mut self, if_node: &TypedNode) -> Option<BasicValueEnum<'a>> {
        let TypedNode::If { is_statement, condition, condition_binding, if_block, else_block, type_id, resolved_type_id, .. } = if_node else { unreachable!() };

        debug_assert!(condition_binding.is_none(), "Condition bindings not yet implemented");
        debug_assert!(condition.as_ref().type_id() == &PRELUDE_BOOL_TYPE_ID, "Only implement if-statements for boolean conditions for now (no Optionals yet)");

        let then_bb = self.context.append_basic_block(self.current_fn, "then_block");
        let else_bb = self.context.append_basic_block(self.current_fn, "else_block");
        let end_bb = self.context.append_basic_block(self.current_fn, "if_end");

        let cond_val = self.visit_expression(&condition).unwrap().into_int_value();
        let cmp = self.builder.build_int_compare(IntPredicate::EQ, cond_val, self.const_bool(true), "");
        self.builder.build_conditional_branch(cmp, then_bb, else_bb);

        self.builder.position_at_end(then_bb);
        let mut if_block_value = None;
        let if_block_len = if_block.len();
        for (idx, node) in if_block.iter().enumerate() {
            if idx == if_block_len - 1 {
                if_block_value = self.visit_statement(node);
            } else {
                self.visit_statement(node);
            }
        }
        self.builder.build_unconditional_branch(end_bb);

        self.builder.position_at_end(else_bb);
        let mut else_block_value = None;
        let else_block_len = else_block.len();
        for (idx, node) in else_block.iter().enumerate() {
            if idx == else_block_len - 1 {
                else_block_value = self.visit_statement(node);
            } else {
                self.visit_statement(node);
            }
        }
        self.builder.build_unconditional_branch(end_bb);

        self.builder.position_at_end(end_bb);

        if *is_statement {
            None
        } else {
            let resolved_generics = &self.ctx_stack.last().unwrap().resolved_generics;
            let Some(llvm_type) = self.llvm_underlying_type_by_id(type_id, resolved_generics) else { todo!() };
            let llvm_type = self.llvm_ptr_wrap_type_if_needed(llvm_type);

            let resolved_generics = &self.ctx_stack.last().unwrap().resolved_generics;
            let if_block_value = if_block_value.unwrap_or_else(|| self.make_none_option_instance(type_id, &resolved_generics).into());
            let else_block_value = else_block_value.unwrap_or_else(|| self.make_none_option_instance(type_id, &resolved_generics).into());

            let phi = self.builder.build_phi(llvm_type, "");
            phi.add_incoming(&[(&if_block_value, then_bb), (&else_block_value, else_bb)]);
            let phi_value = phi.as_basic_value();

            if resolved_type_id == &PRELUDE_ANY_TYPE_ID {
                let resolved_generics = resolved_generics.clone();
                return Some(self.make_trait_instance(&PRELUDE_ANY_TYPE_ID, &type_id, phi_value, &resolved_generics).into());
            } else {
                Some(phi_value)
            }
        }
    }

    fn make_option_instance<V: BasicValue<'a>>(&self, outer_type_id: &TypeId, value: V, resolved_generics: &HashMap<String, TypeId>) -> StructValue<'a> {
        let Type::GenericInstance(struct_id, generics) = self.project.get_type_by_id(outer_type_id) else { unreachable!() };
        debug_assert!(struct_id == &self.project.prelude_option_struct_id);

        let mut inner_type_id = &generics[0];
        if let Type::Generic(_, generic_name) = self.project.get_type_by_id(inner_type_id) {
            inner_type_id = resolved_generics.get(generic_name).unwrap_or(inner_type_id);
        }
        let opt_struct = self.project.get_struct_by_id(&self.project.prelude_option_struct_id);
        let resolved_generics = vec![(self.get_generic_name(&opt_struct.generic_ids[0]).clone(), *inner_type_id)].into_iter().collect();
        let Some(llvm_type) = self.llvm_underlying_type_by_id(outer_type_id, &resolved_generics) else { todo!() };

        let instance_ptr = self.builder.build_alloca(llvm_type, "opt_instance_ptr");
        let is_set_slot = self.builder.build_struct_gep(instance_ptr, 0, "is_set_slot").unwrap();
        self.builder.build_store(is_set_slot, self.const_bool(true));
        let value_slot = self.builder.build_struct_gep(instance_ptr, 1, "value_slot").unwrap();
        self.builder.build_store(value_slot, value.as_basic_value_enum());

        self.builder.build_load(instance_ptr, "opt_instance").into_struct_value()
    }

    fn make_none_option_instance(&self, outer_type_id: &TypeId, resolved_generics: &HashMap<String, TypeId>) -> StructValue<'a> {
        let Type::GenericInstance(struct_id, generics) = self.project.get_type_by_id(outer_type_id) else { unreachable!() };
        debug_assert!(struct_id == &self.project.prelude_option_struct_id);
        let mut inner_type_id = &generics[0];
        if let Type::Generic(_, generic_name) = self.project.get_type_by_id(inner_type_id) {
            inner_type_id = resolved_generics.get(generic_name).unwrap_or(inner_type_id);
        }

        let opt_struct = self.project.get_struct_by_id(&self.project.prelude_option_struct_id);
        let resolved_generics = vec![(self.get_generic_name(&opt_struct.generic_ids[0]).clone(), *inner_type_id)].into_iter().collect();
        let Some(llvm_type) = self.llvm_underlying_type_by_id(outer_type_id, &resolved_generics) else { todo!() };

        llvm_type.into_struct_type().const_zero()
    }

    fn make_trait_instance<V: BasicValue<'a>>(&mut self, trait_type_id: &TypeId, value_type_id: &TypeId, value: V, resolved_generics: &HashMap<String, TypeId>) -> StructValue<'a> {
        debug_assert!(trait_type_id == &PRELUDE_ANY_TYPE_ID, "When other trait implementations come along, this implementation will need to be made generic");

        let trait_type_name = self.llvm_type_name_by_id(trait_type_id, resolved_generics);
        let (trait_type, vtable_struct_type) = self.get_or_make_trait_struct_type(trait_type_id, resolved_generics);

        let trait_instance = self.builder.build_alloca(trait_type, "trait_instance_ptr");

        let value_type_name = self.llvm_type_name_by_id(value_type_id, resolved_generics);
        let vtable_name = format!("VTable({} as {})", &value_type_name, &trait_type_name);
        let vtable_global = if let Some(vtable_ptr) = self.main_module.get_global(&vtable_name) {
            vtable_ptr
        } else {
            let global = self.main_module.add_global(vtable_struct_type, None, &vtable_name);
            global.set_constant(true);

            let tostring_fn_ptr = {
                let prev_bb = self.builder.get_insert_block().unwrap();

                let tostring_fn_type = self.fn_type(self.ptr(self.string_type), &[self.i64().into()]);
                let wrapper_llvm_fn = self.main_module.add_function(&format!("{value_type_name}@Any#toString(ValWrapper):String"), tostring_fn_type, None);
                let block = self.context.append_basic_block(wrapper_llvm_fn, "");
                self.builder.position_at_end(block);

                let raw_value = wrapper_llvm_fn.get_nth_param(0).unwrap().into_int_value();
                let value = if value_type_id == &PRELUDE_INT_TYPE_ID {
                    raw_value.as_basic_value_enum()
                } else if value_type_id == &PRELUDE_FLOAT_TYPE_ID {
                    self.builder.build_cast(InstructionOpcode::BitCast, raw_value, self.f64(), "").as_basic_value_enum()
                } else if value_type_id == &PRELUDE_BOOL_TYPE_ID {
                    self.builder.build_cast(InstructionOpcode::Trunc, raw_value, self.bool(), "").as_basic_value_enum()
                } else {
                    let Some(value_type) = self.llvm_underlying_type_by_id(value_type_id, resolved_generics) else { todo!() };
                    let value_type = self.llvm_ptr_wrap_type_if_needed(value_type);
                    if self.project.type_is_option(value_type_id).is_some() {
                        let ptr = self.builder.build_int_to_ptr(raw_value, self.ptr(value_type), "self");
                        self.builder.build_load(ptr, "self").as_basic_value_enum()
                    } else {
                        self.builder.build_int_to_ptr(raw_value, value_type.into_pointer_type(), "self").as_basic_value_enum()
                    }
                };

                let tostring_llvm_fn = if let Some(inner_type_id) = self.project.type_is_option(value_type_id) {
                    self.get_or_compile_option_method(&inner_type_id, &0)
                } else {
                    let value_ty = self.project.get_type_by_id(value_type_id);
                    let (_, tostring_func_id) = value_ty.find_method_by_name(self.project, "toString").unwrap();
                    self.get_or_compile_function(tostring_func_id, resolved_generics)
                };

                let tostring_ret = self.builder.build_call(tostring_llvm_fn, &[value.into()], "").try_as_basic_value().left().unwrap();
                self.builder.build_return(Some(&tostring_ret));

                self.builder.position_at_end(prev_bb);

                wrapper_llvm_fn.as_global_value().as_pointer_value()
            };

            global.set_initializer(&self.context.const_struct(&[self.const_i32(value_type_id.1 as u64).into(), tostring_fn_ptr.into()], false));
            global
        };
        let vtable_slot = self.builder.build_struct_gep(trait_instance, 0, "vtable_slot").unwrap();
        self.builder.build_store(vtable_slot, vtable_global.as_pointer_value());

        let value_slot = self.builder.build_struct_gep(trait_instance, 1, "value_slot").unwrap();
        if value_type_id == &PRELUDE_INT_TYPE_ID {
            self.builder.build_store(value_slot, value);
        } else if value_type_id == &PRELUDE_FLOAT_TYPE_ID {
            let value = self.builder.build_cast(InstructionOpcode::BitCast, value, self.i64(), "float_as_value");
            self.builder.build_store(value_slot, value);
        } else if value_type_id == &PRELUDE_BOOL_TYPE_ID {
            let value = self.builder.build_int_cast(value.as_basic_value_enum().into_int_value(), self.i64(), "bool_as_value");
            self.builder.build_store(value_slot, value);
        } else {
            // Otherwise, value should be treated a pointer
            let ptr = if self.project.type_is_option(value_type_id).is_some() {
                let option_struct_llvm_type = value.as_basic_value_enum().get_type();
                let value_local = self.builder.build_alloca(option_struct_llvm_type, "");
                self.builder.build_store(value_local, value);

                let mem = self.malloc(self.sizeof_struct(option_struct_llvm_type), self.ptr(option_struct_llvm_type));
                let is_set_slot = self.builder.build_struct_gep(mem, 0, "is_set_slot").unwrap();
                self.builder.build_store(
                    is_set_slot,
                    self.builder.build_load(self.builder.build_struct_gep(value_local, 0, "").unwrap(), "is_set_value_orig"),
                );
                let value_slot = self.builder.build_struct_gep(mem, 1, "value_slot").unwrap();
                self.builder.build_store(
                    value_slot,
                    self.builder.build_load(self.builder.build_struct_gep(value_local, 1, "").unwrap(), "value_value_orig"),
                );

                mem
            } else {
                value.as_basic_value_enum().into_pointer_value()
            };
            let value = self.builder.build_ptr_to_int(ptr, self.i64(), "ptr_as_value");
            self.builder.build_store(value_slot, value);
        }

        self.builder.build_load(trait_instance, "trait_instance").into_struct_value()
    }

    fn compile_intrinsic_invocation(
        &mut self,
        type_arg_ids: &Vec<TypeId>,
        resolved_generics: &HashMap<String, TypeId>,
        name: &String,
        implicit_argument: Option<&TypedNode>,
        arguments: &Vec<Option<TypedNode>>,
        resolved_type_id: &TypeId,
    ) -> Option<BasicValueEnum<'a>> {
        let get_ptr_size = |pointer_type_id: &TypeId| -> IntValue<'a> {
            let pointer_type_id = if let Some(inner_type_id) = self.project.type_is_option(pointer_type_id) {
                inner_type_id
            } else {
                *pointer_type_id
            };

            let Some(llvm_type) = self.llvm_underlying_type_by_id(&pointer_type_id, &resolved_generics) else { todo!() };
            llvm_type.size_of().expect("We should only ever be working with sized types")
        };

        let value = match name.as_ref() {
            "pointer_null" => { // Static method
                let pointer_type = type_arg_ids[0];
                let Some(llvm_type) = self.llvm_underlying_type_by_id(&pointer_type, &resolved_generics) else { todo!() };
                let llvm_type = self.llvm_ptr_wrap_type_if_needed(llvm_type);

                self.ptr(llvm_type).const_null().as_basic_value_enum()
            }
            "pointer_malloc" => { // Static method
                let pointer_type_id = type_arg_ids[0];
                let ptr_size = get_ptr_size(&pointer_type_id);
                let Some(llvm_type) = self.llvm_underlying_type_by_id(&pointer_type_id, &resolved_generics) else { todo!() };
                let llvm_type = self.llvm_ptr_wrap_type_if_needed(llvm_type);

                let count_arg = arguments.first().expect("Pointer.malloc has arity 1");
                let count_arg_value = if let Some(count_arg) = count_arg {
                    self.visit_expression(count_arg).expect("Pointer.malloc's count parameter is not of type Unit")
                } else {
                    self.const_i64(1).as_basic_value_enum()
                };
                let malloc_amount_val = self.builder.build_int_mul(count_arg_value.into_int_value(), ptr_size, "malloc_amt");

                let ptr = self.malloc(malloc_amount_val, self.ptr(llvm_type));
                ptr.as_basic_value_enum()
            }
            "pointer_address" => { // Instance method
                let instance_node = implicit_argument.expect("pointer_address is an instance method and will have an implicit argument");
                let instance_value = self.visit_expression(instance_node).expect("Instance is not of type Unit").into_pointer_value();
                let pointer_address = self.builder.build_ptr_to_int(instance_value, self.i64(), "address");

                pointer_address.as_basic_value_enum()
            }
            "pointer_store" => { // Instance method
                let instance_node = implicit_argument.expect("pointer_store is an instance method and will have an implicit argument");
                let value_arg = arguments.first().expect("Pointer#store has arity 2").as_ref().expect("Pointer#store has 1 required non-implicit argument");

                let ptr = self.visit_expression(instance_node).expect("Instance is not of type Unit").into_pointer_value();
                let value = self.visit_expression(value_arg).expect("Instance is not of type Unit");
                self.builder.build_store(ptr, value);

                return None;
            }
            "pointer_load" => { // Instance method
                let instance_node = implicit_argument.expect("pointer_load is an instance method and will have an implicit argument");

                let ptr = self.visit_expression(instance_node).expect("Instance is not of type Unit").into_pointer_value();
                self.builder.build_load(ptr, "").as_basic_value_enum()
            }
            "pointer_offset" => { // Instance method
                let instance_node = implicit_argument.expect("pointer_offset is an instance method and will have an implicit argument");
                let offset_arg = arguments.first().expect("Pointer#offset has arity 2").as_ref().expect("Pointer#offset has 1 required non-implicit argument");

                let ptr = self.visit_expression(instance_node).expect("Instance is not of type Unit").into_pointer_value();
                let value = self.visit_expression(offset_arg).expect("Instance is not of type Unit").into_int_value();

                let offset_ptr = unsafe { self.builder.build_gep(ptr, &[value], "offset") };
                offset_ptr.as_basic_value_enum()
            }
            "pointer_copy_from" => { // Instance method
                let instance_node = implicit_argument.expect("pointer_copy_from is an instance method and will have an implicit argument");
                let mut arguments = arguments.iter();
                let other_arg = arguments.next().expect("Pointer#copyFrom has arity 3").as_ref().expect("Pointer#copyFrom has 2 required non-implicit arguments");
                let size_arg = arguments.next().expect("Pointer#copyFrom has arity 3").as_ref().expect("Pointer#copyFrom has 2 required non-implicit arguments");

                let Type::GenericInstance(_, generics) = self.project.get_type_by_id(instance_node.type_id()) else { unreachable!() };
                let ptr_size = get_ptr_size(&generics[0]);

                let ptr = self.visit_expression(instance_node).expect("Instance is not of type Unit").into_pointer_value();
                let other_val = self.visit_expression(other_arg).expect("Instance is not of type Unit").into_pointer_value();
                let size_val = self.visit_expression(size_arg).expect("Instance is not of type Unit").into_int_value();
                let size_val = self.builder.build_int_mul(size_val, ptr_size, "");

                self.memcpy(ptr, other_val, size_val);

                return None;
            }
            "stdout_write" => { // Freestanding function
                let arg_node = arguments.first().expect("stdoutWrite has arity 1").as_ref().expect("stdoutWrite has 1 required argument");
                let arg_value = self.visit_expression(arg_node).unwrap().into_pointer_value();
                let arg_value_len_ptr = self.builder.build_struct_gep(arg_value, 0, "len_slot").unwrap();
                let arg_value_len = self.builder.build_load(arg_value_len_ptr, "len");
                let arg_value_chars_ptr = self.builder.build_struct_gep(arg_value, 1, "chars_slot").unwrap();
                let arg_value_chars = self.builder.build_load(arg_value_chars_ptr, "chars");

                let printf = self.main_module.get_function("printf").unwrap();
                let fmt_str = self.builder.build_global_string_ptr("%.*s", "").as_basic_value_enum();
                self.builder.build_call(printf, &[fmt_str.into(), arg_value_len.into(), arg_value_chars.into()], "").try_as_basic_value().left();

                return None;
            }
            "byte_from_int" => { // Static method
                let arg_node = arguments.first().expect("Byte.fromInt has arity 1").as_ref().expect("Byte.fromInt has 1 required argument");
                let arg_value = self.visit_expression(arg_node).unwrap().into_int_value();
                let i8_val = self.builder.build_int_cast(arg_value, self.i8(), "");
                i8_val.as_basic_value_enum()
            }
            _ => unimplemented!("Unimplemented intrinsic '{}'", name),
        };

        if self.project.type_is_option(resolved_type_id).is_some() {
            Some(self.make_option_instance(resolved_type_id, value, resolved_generics).into())
        } else {
            Some(value)
        }
    }

    fn get_or_make_trait_struct_type(&self, trait_type_id: &TypeId, resolved_generics: &HashMap<String, TypeId>) -> (/* trait_type: */ StructType<'a>, /* vtable_type: */ StructType<'a>) {
        let type_name = self.llvm_type_name_by_id(trait_type_id, resolved_generics);

        let vtable_struct_type = self.trait_vtable_struct_type(trait_type_id);
        let trait_struct_type = if let Some(llvm_type) = self.main_module.get_struct_type(&type_name) {
            llvm_type
        } else {
            let llvm_type = self.context.opaque_struct_type(&type_name);
            llvm_type.set_body(&[self.ptr(vtable_struct_type.as_basic_type_enum()).into(), self.i64().into()], false);
            llvm_type
        };

        (trait_struct_type, vtable_struct_type)
    }

    fn trait_vtable_struct_type(&self, trait_type_id: &TypeId) -> StructType<'a> {
        if trait_type_id == &PRELUDE_ANY_TYPE_ID {
            let tostring_fn_type = self.fn_type(self.ptr(self.string_type), &[self.i64().into()]);
            let tostring_fn_ptr_type = tostring_fn_type.ptr_type(AddressSpace::Generic);
            self.context.struct_type(&[
                self.i32().into(), // type_id
                tostring_fn_ptr_type.into(), // toString_wrapper fn pointer
            ], false)
        } else {
            unimplemented!("No traits other than `Any` are implemented yet")
        }
    }

    fn get_or_compile_function(&mut self, func_id: &FuncId, resolved_generics: &HashMap<String, TypeId>) -> FunctionValue<'a> {
        let fn_sig = self.llvm_function_signature(func_id, &resolved_generics);
        let llvm_fn = if let Some(function_val) = self.main_module.get_function(&fn_sig) {
            function_val
        } else {
            let function = self.project.get_func_by_id(func_id);
            let function_val = if matches!(function.kind, FunctionKind::Method(_)) && function.name == "toString" && function.body.is_empty() {
                self.compile_to_string_function(&func_id, &resolved_generics)
            } else {
                self.compile_function(&func_id, &resolved_generics)
            };
            debug_assert!(function_val.get_name().to_str().unwrap() == &fn_sig);
            function_val
        };

        llvm_fn
    }

    fn get_or_compile_option_method(&mut self, inner_type_id: &TypeId, member_idx: &usize) -> FunctionValue<'a> {
        // The toString method is always member_idx = 0
        if *member_idx != 0 {
            unreachable!();
        }

        let resolved_generics = if let Type::GenericInstance(struct_id, generics) = self.project.get_type_by_id(inner_type_id) {
            let struct_ = self.project.get_struct_by_id(struct_id);
            struct_.generic_ids.iter().zip(generics)
                .map(|(generic_id, type_arg_id)| {
                    (self.get_generic_name(generic_id).clone(), *type_arg_id)
                })
                .collect()
        } else {
            HashMap::new()
        };

        let type_name = format!("Option<{}>", self.llvm_type_name_by_id(inner_type_id, &resolved_generics));
        let fn_sig = format!("{}#toString({}):String", &type_name, &type_name);
        let fn_type = self.fn_type(self.ptr(self.string_type), &[self.main_module.get_struct_type(&type_name).unwrap().into()]);
        let llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = llvm_fn;
        self.ctx_stack.push(CompilerContext { variables: HashMap::new(), resolved_generics: resolved_generics.clone() });

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let self_ = llvm_fn.get_nth_param(0).unwrap();
        let self_var = self.builder.build_alloca(self_.get_type(), "self");
        self.builder.build_store(self_var, self_);

        let is_none_block = self.context.append_basic_block(llvm_fn, "is_none");
        let is_some_block = self.context.append_basic_block(llvm_fn, "is_some");
        let end_block = self.context.append_basic_block(llvm_fn, "end");

        let is_set_slot = self.builder.build_struct_gep(self_var, 0, "is_set_slot").unwrap();
        let is_set_val = self.builder.build_load(is_set_slot, "is_set");
        let cond = self.builder.build_int_compare(IntPredicate::NE, is_set_val.into_int_value(), self.const_bool(true), "cond");
        self.builder.build_conditional_branch(cond, is_none_block, is_some_block);

        self.builder.position_at_end(is_none_block);
        let is_none_value = self.main_module.get_global("NONE_STRING_VALUE").unwrap_or_else(|| {
            let global = self.main_module.add_global(self.string_type, None, "NONE_STRING_VALUE");
            global.set_constant(true);
            let str = self.builder.build_global_string_ptr("None", "").as_basic_value_enum();
            global.set_initializer(&self.context.const_struct(&[self.const_i64(4).into(), str], false));
            global
        }).as_pointer_value();
        self.builder.build_unconditional_branch(end_block);

        self.builder.position_at_end(is_some_block);
        let value_slot = self.builder.build_struct_gep(self_var, 1, "value_slot").unwrap();
        let value_val = self.builder.build_load(value_slot, "value");
        let inner_type = self.project.get_type_by_id(inner_type_id);
        let (_, inner_tostring_func_id) = inner_type.find_method_by_name(self.project, "toString").unwrap();
        let inner_tostring_llvm_fn = self.get_or_compile_function(&inner_tostring_func_id, &resolved_generics);
        let is_some_value = self.builder.build_call(inner_tostring_llvm_fn, &[value_val.into()], "").try_as_basic_value().left().unwrap();
        self.builder.build_unconditional_branch(end_block);

        self.builder.position_at_end(end_block);

        let phi = self.builder.build_phi(self.ptr(self.string_type), "");
        phi.add_incoming(&[(&is_none_value, is_none_block), (&is_some_value, is_some_block)]);
        self.builder.build_return(Some(&phi.as_basic_value()));

        self.ctx_stack.pop();
        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn compile_function(&mut self, func_id: &FuncId, resolved_generics: &HashMap<String, TypeId>) -> FunctionValue<'a> {
        let function = self.project.get_func_by_id(func_id);
        let has_return_value = function.return_type_id != PRELUDE_UNIT_TYPE_ID;

        let fn_sig = self.llvm_function_signature(func_id, resolved_generics);
        let fn_type = self.llvm_function_type(func_id, &resolved_generics);
        let llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = llvm_fn;
        self.ctx_stack.push(CompilerContext { variables: HashMap::new(), resolved_generics: resolved_generics.clone() });

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let mut params_iter = llvm_fn.get_param_iter();
        for (idx, param) in function.params.iter().enumerate() {
            params_iter.next().unwrap().set_name(&param.name);
            let llvm_param = llvm_fn.get_nth_param(idx as u32).unwrap();
            self.ctx_stack.last_mut().unwrap().variables.insert(param.var_id, LLVMVar::Param(llvm_param));
        }

        let num_nodes = function.body.len();
        for (idx, node) in function.body.iter().enumerate() {
            let res = self.visit_statement(node);
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
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    // fn compile_struct_type_by_type_id(&self, type_id: &TypeId, resolved_generics: &HashMap<String, TypeId>) -> StructType<'a> {
    //     let ty = self.project.get_type_by_id(type_id);
    //     self.compile_struct_type_by_type(ty, resolved_generics)
    // }

    fn compile_struct_type_by_type(&self, ty: &Type, resolved_generics: &HashMap<String, TypeId>) -> StructType<'a> {
        let Type::GenericInstance(struct_id, generics) = ty else { todo!() };
        if struct_id == &self.project.prelude_option_struct_id {
            return self.compile_option_struct_type_by_type(ty, resolved_generics);
        }

        let struct_ = self.project.get_struct_by_id(struct_id);
        let type_name = self.llvm_type_name_by_type(ty, resolved_generics);
        let llvm_type = self.context.opaque_struct_type(&type_name);

        let generic_substitutions = struct_.generic_ids.iter().zip(generics)
            .map(|(generic_id, type_arg_id)| {
                if let Type::Generic(_, generic_name) = self.project.get_type_by_id(type_arg_id) {
                    let type_arg_id = resolved_generics.get(generic_name).unwrap_or(type_arg_id);
                    (self.get_generic_name(generic_id).clone(), *type_arg_id)
                } else {
                    (self.get_generic_name(generic_id).clone(), *type_arg_id)
                }
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

    // fn compile_option_struct_type_by_type_id(&self, type_id: &TypeId, resolved_generics: &HashMap<String, TypeId>) -> StructType<'a> {
    //     let ty = self.project.get_type_by_id(type_id);
    //     self.compile_option_struct_type_by_type(ty, resolved_generics)
    // }

    fn compile_option_struct_type_by_type(&self, ty: &Type, resolved_generics: &HashMap<String, TypeId>) -> StructType<'a> {
        let Type::GenericInstance(struct_id, generics) = ty else { todo!() };
        debug_assert!(struct_id == &self.project.prelude_option_struct_id);

        let type_name = self.llvm_type_name_by_type(ty, resolved_generics);

        let Some(inner_llvm_type) = self.llvm_underlying_type_by_id(&generics[0], resolved_generics) else { todo!() };
        let inner_llvm_type = self.llvm_ptr_wrap_type_if_needed(inner_llvm_type);

        let llvm_type = self.context.opaque_struct_type(&type_name);
        llvm_type.set_body(&[self.bool().into(), inner_llvm_type.into()], false);

        llvm_type
    }

    fn get_or_compile_type_initializer(&mut self, struct_id: &StructId, resolved_generics: &HashMap<String, TypeId>) -> FunctionValue<'a> {
        let init_fn_sig = self.llvm_initializer_signature(struct_id, &resolved_generics);
        if let Some(function_val) = self.main_module.get_function(&init_fn_sig) {
            function_val
        } else {
            let function_val = self.compile_type_initializer(&struct_id, &resolved_generics);
            debug_assert!(function_val.get_name().to_str().unwrap() == &init_fn_sig);
            function_val
        }
    }

    fn compile_type_initializer(&mut self, struct_id: &StructId, resolved_generics: &HashMap<String, TypeId>) -> FunctionValue<'a> {
        let struct_ = self.project.get_struct_by_id(struct_id);
        let initializer_sig = self.llvm_initializer_signature(struct_id, resolved_generics);

        let fn_type = self.llvm_initializer_type(struct_id, resolved_generics);
        let llvm_fn = self.main_module.add_function(&initializer_sig, fn_type, None);
        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = llvm_fn;

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let Some(llvm_struct_type) = self.llvm_underlying_type_by_id(&struct_.self_type_id, resolved_generics) else { todo!() };
        let mem = self.malloc(self.sizeof_struct(llvm_struct_type), self.ptr(llvm_struct_type));

        let mut params_iter = llvm_fn.get_param_iter();
        for (idx, field) in struct_.fields.iter().enumerate() {
            params_iter.next().unwrap().set_name(&field.name);
            let field_value = llvm_fn.get_nth_param(idx as u32).unwrap();
            let field_ptr = self.builder.build_struct_gep(mem, idx as u32, &format!("{}_slot", &field.name)).unwrap();
            self.builder.build_store(field_ptr, field_value);
        }

        self.builder.build_return(Some(&mem.as_basic_value_enum()));

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn compile_to_string_function(&mut self, func_id: &FuncId, resolved_generics: &HashMap<String, TypeId>) -> FunctionValue<'a> {
        let initializer_sig = self.llvm_function_signature(func_id, resolved_generics);
        let function = self.project.get_func_by_id(func_id);
        let FunctionKind::Method(type_id) = &function.kind else { unreachable!() };
        let Some((struct_, _)) = self.project.get_struct_by_type_id(type_id) else { todo!() };

        if struct_.id == self.project.prelude_int_struct_id {
            return self.compile_int_to_string_method(func_id);
        } else if struct_.id == self.project.prelude_float_struct_id {
            return self.compile_float_to_string_method(func_id);
        } else if struct_.id == self.project.prelude_bool_struct_id {
            return self.compile_bool_to_string_method(func_id);
        } else if struct_.id == self.project.prelude_string_struct_id {
            return self.compile_string_to_string_method(func_id);
        }

        let fn_type = self.llvm_function_type(func_id, resolved_generics);
        let llvm_fn = self.main_module.add_function(&initializer_sig, fn_type, None);
        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = llvm_fn;

        let mut params_iter = llvm_fn.get_param_iter();
        params_iter.next().unwrap().set_name("self");
        let llvm_self_param = llvm_fn.get_nth_param(0).unwrap().into_pointer_value();

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let fields_fmt = struct_.fields.iter()
            .map(|f| {
                if f.type_id == PRELUDE_STRING_TYPE_ID {
                    format!("{}: \"%s\"", &f.name)
                } else {
                    format!("{}: %s", &f.name)
                }
            })
            .join(", ");
        let to_string_fmt = format!("{}({})", struct_.name, fields_fmt);
        let fmt_str_val = self.builder.build_global_string_ptr(&to_string_fmt, "").as_basic_value_enum();
        let mut len_val = self.const_i64(to_string_fmt.len() as u64);

        let mut snprintf_args = Vec::with_capacity(struct_.fields.len());
        for (idx, field) in struct_.fields.iter().enumerate() {
            let field_slot = self.builder.build_struct_gep(llvm_self_param, idx as u32, &format!("{}_slot", &field.name)).unwrap();
            let field_val = self.builder.build_load(field_slot, &field.name);

            let field_ty = self.project.get_type_by_id(&field.type_id);
            let (_, tostring_func_id) = field_ty.find_method_by_name(self.project, "toString").unwrap();
            let tostring_fn_val = self.get_or_compile_function(tostring_func_id, resolved_generics);
            let field_tostring_val = self.builder.build_call(tostring_fn_val, &[field_val.into()], &format!("{}_to_string", &field.name)).try_as_basic_value().left().unwrap().into_pointer_value();

            let field_tostring_chars_slot = self.builder.build_struct_gep(field_tostring_val, 1, &format!("{}_to_string.chars slot", &field.name)).unwrap();
            let field_tostring_chars = self.builder.build_load(field_tostring_chars_slot, &format!("{}_to_string.chars", &field.name));
            snprintf_args.push(field_tostring_chars.into());

            let field_tostring_len_slot = self.builder.build_struct_gep(field_tostring_val, 0, &format!("{}_to_string.length slot", &field.name)).unwrap();
            let field_tostring_len = self.builder.build_load(field_tostring_len_slot, &format!("{}_to_string.length", &field.name)).into_int_value();
            len_val = self.builder.build_int_add(len_val, field_tostring_len, "");
        }

        let len_plus_1 = self.builder.build_int_add::<IntValue<'a>>(len_val.into(), self.const_i64(1).into(), "len_plus_1");
        let str_val = self.malloc(len_plus_1, self.ptr(self.i8()));
        let snprintf_len_val = self.builder.build_int_cast(len_plus_1, self.i32(), "");
        let mut args = vec![str_val.into(), snprintf_len_val.into(), fmt_str_val.into()];
        args.append(&mut snprintf_args);
        self.builder.build_call(self.snprintf, args.as_slice(), "").try_as_basic_value().left().unwrap();

        let string_initializer = self.get_or_compile_type_initializer(&self.project.prelude_string_struct_id, &HashMap::new());
        let ret_val = self.builder.build_call(string_initializer, &[len_val.into(), str_val.into()], "").try_as_basic_value().left().unwrap();

        self.builder.build_return(Some(&ret_val));

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn compile_int_to_string_method(&mut self, func_id: &FuncId) -> FunctionValue<'a> {
        let llvm_fn_sig = self.llvm_function_signature(func_id, &HashMap::new());
        let llvm_fn_type = self.llvm_function_type(func_id, &HashMap::new());
        let llvm_fn = self.main_module.add_function(&llvm_fn_sig, llvm_fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = llvm_fn;

        llvm_fn.get_param_iter().next().unwrap().set_name("self");
        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let fmt_str = self.builder.build_global_string_ptr("%jd", "").as_basic_value_enum();
        let self_param = llvm_fn.get_nth_param(0).unwrap();
        let len_val = self.builder.build_call(self.snprintf, &[self.null_ptr().into(), self.const_i32(0).into(), fmt_str.into(), self_param.into()], "len").try_as_basic_value().left().unwrap().into_int_value();
        let len_plus_1 = self.builder.build_int_add::<IntValue<'a>>(len_val.into(), self.const_i64(1).into(), "len_plus_1");
        let str_val = self.malloc(len_plus_1, self.ptr(self.i8()));
        let snprintf_len_val = self.builder.build_int_cast(len_plus_1, self.i32(), "");
        self.builder.build_call(self.snprintf, &[str_val.into(), snprintf_len_val.into(), fmt_str.into(), self_param.into()], "").try_as_basic_value().left().unwrap();

        let string_initializer = self.get_or_compile_type_initializer(&self.project.prelude_string_struct_id, &HashMap::new());
        let ret_val = self.builder.build_call(string_initializer, &[len_val.into(), str_val.into()], "").try_as_basic_value().left().unwrap();

        self.builder.build_return(Some(&ret_val));

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn compile_float_to_string_method(&mut self, func_id: &FuncId) -> FunctionValue<'a> {
        let llvm_fn_sig = self.llvm_function_signature(func_id, &HashMap::new());
        let llvm_fn_type = self.llvm_function_type(func_id, &HashMap::new());
        let llvm_fn = self.main_module.add_function(&llvm_fn_sig, llvm_fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = llvm_fn;

        llvm_fn.get_param_iter().next().unwrap().set_name("self");
        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let fmt_str = self.builder.build_global_string_ptr("%g", "").as_basic_value_enum();
        let self_param = llvm_fn.get_nth_param(0).unwrap();
        let len_val = self.builder.build_call(self.snprintf, &[self.null_ptr().into(), self.const_i32(0).into(), fmt_str.into(), self_param.into()], "len").try_as_basic_value().left().unwrap().into_int_value();
        let len_plus_1 = self.builder.build_int_add::<IntValue<'a>>(len_val.into(), self.const_i64(1).into(), "len_plus_1");
        let str_val = self.malloc(len_plus_1, self.ptr(self.i8()));
        let sprintf_len_val = self.builder.build_int_cast(len_plus_1, self.i32(), "");
        self.builder.build_call(self.snprintf, &[str_val.into(), sprintf_len_val.into(), fmt_str.into(), self_param.into()], "").try_as_basic_value().left().unwrap();

        let string_initializer = self.get_or_compile_type_initializer(&self.project.prelude_string_struct_id, &HashMap::new());
        let ret_val = self.builder.build_call(string_initializer, &[len_val.into(), str_val.into()], "").try_as_basic_value().left().unwrap();

        self.builder.build_return(Some(&ret_val));

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn compile_bool_to_string_method(&mut self, func_id: &FuncId) -> FunctionValue<'a> {
        let bool_true_str_global = self.main_module.add_global(self.string_type, None, "BOOL_TRUE_STR");
        bool_true_str_global.set_constant(true);
        let true_str = self.builder.build_global_string_ptr("true", "").as_basic_value_enum();
        bool_true_str_global.set_initializer(&self.context.const_struct(&[self.const_i64(4).into(), true_str], false));
        let bool_false_str_global = self.main_module.add_global(self.string_type, None, "BOOL_FALSE_STR");
        bool_false_str_global.set_constant(true);
        let false_str = self.builder.build_global_string_ptr("false", "").as_basic_value_enum();
        bool_false_str_global.set_initializer(&self.context.const_struct(&[self.const_i64(5).into(), false_str], false));

        let llvm_fn_sig = self.llvm_function_signature(func_id, &HashMap::new());
        let llvm_fn_type = self.llvm_function_type(func_id, &HashMap::new());
        let llvm_fn = self.main_module.add_function(&llvm_fn_sig, llvm_fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = llvm_fn;

        llvm_fn.get_param_iter().next().unwrap().set_name("self");
        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let cond = llvm_fn.get_nth_param(0).unwrap().into_int_value();
        let then_bb = self.context.append_basic_block(llvm_fn, "then");
        let else_bb = self.context.append_basic_block(llvm_fn, "else");
        let cont_bb = self.context.append_basic_block(llvm_fn, "cont");
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

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn compile_string_to_string_method(&mut self, func_id: &FuncId) -> FunctionValue<'a> {
        let llvm_fn_sig = self.llvm_function_signature(func_id, &HashMap::new());
        let llvm_fn_type = self.llvm_function_type(func_id, &HashMap::new());
        let llvm_fn = self.main_module.add_function(&llvm_fn_sig, llvm_fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = llvm_fn;

        llvm_fn.get_param_iter().next().unwrap().set_name("self");
        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);
        self.builder.build_return(Some(&llvm_fn.get_nth_param(0).unwrap()));

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

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

    #[test]
    fn test_arrays() {
        run_test_file("arrays.abra");
    }

    #[test]
    fn test_types() {
        run_test_file("types.abra");
    }

    #[test]
    fn test_loops() {
        run_test_file("loops.abra");
    }

    #[test]
    fn test_ifs() {
        run_test_file("ifs.abra");
    }

    #[test]
    fn test_optionals() {
        run_test_file("optionals.abra");
    }

    #[test]
    fn test_any() {
        run_test_file("any.abra");
    }
}
