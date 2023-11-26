use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt::Debug;
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
use abra_core::parser::ast::{BinaryOp, BindingPattern, IndexingMode, UnaryOp};
use abra_core::typechecker::typechecker2::{AccessorKind, AssignmentKind, FuncId, Function, FunctionKind, PRELUDE_ANY_TYPE_ID, PRELUDE_BOOL_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_INT_TYPE_ID, PRELUDE_MODULE_ID, PRELUDE_SCOPE_ID, PRELUDE_STRING_TYPE_ID, PRELUDE_UNIT_TYPE_ID, PrimitiveType, Project, Struct, StructId, Type, TypedLiteral, TypedNode, TypeId, TypeKind, VariableAlias, VarId};

const ABRA_MAIN_FN_NAME: &str = "_abra_main";

#[derive(Debug, Default)]
struct CompilerContext<'ctx> {
    variables: HashMap<VarId, LLVMVar<'ctx>>,
}

#[derive(Debug)]
enum LLVMVar<'ctx> {
    Slot(PointerValue<'ctx>),
    Param(BasicValueEnum<'ctx>),
}

#[derive(Clone, Default)]
struct ResolvedGenerics(HashMap<String, TypeId>);

impl ResolvedGenerics {
    pub fn from_pairs(pairs: Vec<(String, TypeId)>) -> ResolvedGenerics {
        ResolvedGenerics(pairs.into_iter().collect())
    }

    // pub fn dump(&self, project: &Project) {
    //     println!("resolved_generics:");
    //     for (key, type_id) in &self.0 {
    //         println!("  {key}: {}", project.type_repr(type_id))
    //     }
    // }

    pub fn resolve(&self, name: &String) -> Option<&TypeId> {
        self.0.get(name)
    }

    pub fn extend_via_instance(&self, type_id: &TypeId, project: &Project) -> ResolvedGenerics {
        let mut map = self.clone().0;
        if let Type::GenericInstance(struct_id, generics) = project.get_type_by_id(type_id) {
            let struct_ = project.get_struct_by_id(struct_id);
            for (generic_type_id, type_id) in struct_.generic_ids.iter().zip(generics) {
                map.insert(self.get_generic_name(generic_type_id, project), *type_id);
            }
        }
        ResolvedGenerics(map)
    }

    pub fn extend_via_struct(&self, struct_: &Struct, realized_generics: &Vec<TypeId>, project: &Project) -> ResolvedGenerics {
        let mut map = self.clone().0;
        for (generic_type_id, type_id) in struct_.generic_ids.iter().zip(realized_generics) {
            map.insert(self.get_generic_name(generic_type_id, project), *type_id);
        }
        ResolvedGenerics(map)
    }

    pub fn extend_via_func_call(&self, invokee: &Function, realized_generics: &Vec<TypeId>, project: &Project) -> ResolvedGenerics {
        let mut map = self.clone().0;
        for (generic_type_id, type_id) in invokee.generic_ids.iter().zip(realized_generics) {
            map.insert(self.get_generic_name(generic_type_id, project), self.resolve_chase_generic(type_id, project));
        }
        ResolvedGenerics(map)
    }

    fn resolve_chase_generic(&self, realized_generic_id: &TypeId, project: &Project) -> TypeId {
        if let Type::Generic(_, generic_name) = project.get_type_by_id(realized_generic_id) {
            *self.resolve(generic_name).unwrap_or(realized_generic_id)
        } else {
            *realized_generic_id
        }
    }

    pub fn new_via_func_call(&self, invokee: &Function, realized_generics: &Vec<TypeId>, project: &Project) -> ResolvedGenerics {
        let map = invokee.generic_ids.iter().zip(realized_generics)
            .map(|(generic_id, type_arg_id)| {
                (self.get_generic_name(generic_id, project), self.resolve_chase_generic(type_arg_id, project))
            })
            .collect();
        ResolvedGenerics(map)
    }

    pub fn new_via_struct(&self, struct_: &Struct, realized_generics: &Vec<TypeId>, project: &Project) -> ResolvedGenerics {
        let map = struct_.generic_ids.iter().zip(realized_generics)
            .map(|(generic_id, type_arg_id)| {
                (self.get_generic_name(generic_id, project), self.resolve_chase_generic(type_arg_id, project))
            })
            .collect();
        ResolvedGenerics(map)
    }

    pub fn new_via_instance(&self, ty: &Type, project: &Project) -> ResolvedGenerics {
        if let Type::GenericInstance(struct_id, generics) = ty {
            let struct_ = project.get_struct_by_id(struct_id);
            let map = struct_.generic_ids.iter().zip(generics)
                .map(|(generic_id, type_arg_id)| {
                    (self.get_generic_name(generic_id, project), self.resolve_chase_generic(type_arg_id, project))
                })
                .collect();
            ResolvedGenerics(map)
        } else {
            ResolvedGenerics::default()
        }
    }

    pub fn new_via_method_call(&self, target_ty: &Type, method: &Function, realized_generics: &Vec<TypeId>, project: &Project) -> ResolvedGenerics {
        // TODO: Rework this? It's a little inefficient because it collects twice (and clones the underlying map).
        self.new_via_instance(target_ty, project)
            .extend_via_func_call(method, realized_generics, project)
    }

    fn get_generic_name(&self, type_id: &TypeId, project: &Project) -> String {
        let Type::Generic(_, generic_name) = project.get_type_by_id(type_id) else { unreachable!("TypeId {:?} represents a type that is not a generic", type_id) };
        generic_name.clone()
    }
}

pub struct LLVMCompiler2<'a> {
    project: &'a Project,
    context: &'a Context,
    builder: Builder<'a>,
    main_module: Module<'a>,
    current_fn: (FunctionValue<'a>, Option<FuncId>),
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
            current_fn: (abra_main_fn, None),
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

    fn i16(&self) -> IntType<'a> {
        self.context.i16_type()
    }

    fn const_i16(&self, value: i16) -> IntValue<'a> {
        self.i16().const_int(value as u64, false)
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

    fn closure_captures_t(&self) -> PointerType<'a> {
        self.ptr(self.i64())
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

    fn llvm_type_name_by_id(&self, type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> String {
        let ty = self.project.get_type_by_id(type_id);
        self.llvm_type_name_by_type(ty, resolved_generics)
    }

    fn llvm_type_name_by_type(&self, ty: &Type, resolved_generics: &ResolvedGenerics) -> String {
        match ty {
            Type::Primitive(PrimitiveType::Unit) => "Unit".into(),
            Type::Primitive(PrimitiveType::Any) => "Any".into(),
            Type::Primitive(PrimitiveType::Int) => "Int".into(),
            Type::Primitive(PrimitiveType::Float) => "Float".into(),
            Type::Primitive(PrimitiveType::Bool) => "Bool".into(),
            Type::Primitive(PrimitiveType::String) => "String".into(),
            Type::Generic(_, name) => {
                resolved_generics.resolve(name)
                    .map(|type_id| {
                        if let Type::Generic(_, other_name) = self.project.get_type_by_id(type_id) {
                            if other_name == name { panic!("Self-referential generic type; stackoverflow detected"); }
                        }
                        self.llvm_type_name_by_id(type_id, resolved_generics)
                    })
                    .unwrap_or(name.clone())
            }
            Type::GenericInstance(struct_id, generic_ids) => {
                let struct_ = self.project.get_struct_by_id(struct_id);
                let struct_name = &struct_.name;
                let generic_names = generic_ids.iter().map(|type_id| self.llvm_type_name_by_id(type_id, resolved_generics)).join(",");
                let generic_names = if generic_ids.is_empty() { "".into() } else { format!("<{}>", generic_names) };

                format!("{struct_name}{generic_names}")
            }
            Type::GenericEnumInstance(_, _, _) => todo!(),
            Type::Function(parameter_type_ids, num_required_params, is_variadic, return_type_id) => {
                debug_assert!(!*is_variadic, "Not yet implemented");

                let arity = *num_required_params;

                let ret_type_name = self.llvm_type_name_by_id(return_type_id, resolved_generics);
                let param_type_names = parameter_type_ids.iter().take(*num_required_params).map(|type_id| self.llvm_type_name_by_id(type_id, resolved_generics));//.join(",");
                let mut function_type_args = vec![ret_type_name];
                function_type_args.extend(param_type_names);
                let function_type_args = function_type_args.join(",");

                format!("Function{arity}<{function_type_args}>")
            }
            Type::Type(kind) => match kind {
                TypeKind::Struct(struct_id) => {
                    let struct_ = self.project.get_struct_by_id(struct_id);
                    let struct_name = &struct_.name;
                    let generic_names = struct_.generic_ids.iter()
                        .map(|type_id| {
                            let generic_name = self.get_generic_name(type_id);
                            if let Some(sub_type_id) = resolved_generics.resolve(&generic_name) {
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

    fn llvm_underlying_type_by_id(&self, type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> Option<BasicTypeEnum<'a>> {
        let ty = self.project.get_type_by_id(type_id);
        self.llvm_underlying_type_by_type(ty, resolved_generics)
    }

    fn llvm_underlying_type_by_type(&self, ty: &Type, resolved_generics: &ResolvedGenerics) -> Option<BasicTypeEnum<'a>> {
        let llvm_type = match ty {
            Type::Primitive(PrimitiveType::Unit) => return None,
            Type::Primitive(PrimitiveType::Any) => {
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
                return resolved_generics.resolve(name)
                    .and_then(|resolved_type_id| {
                        if let Type::Generic(_, other_name) = self.project.get_type_by_id(resolved_type_id) {
                            if other_name == name { panic!("Self-referential generic type; stackoverflow detected"); }
                        }
                        self.llvm_underlying_type_by_id(resolved_type_id, resolved_generics)
                    });
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
            Type::GenericEnumInstance(_, _, _) => todo!(),
            Type::Function(_, _, _, _) => self.make_function_value_type_by_type(ty, resolved_generics).0.as_basic_type_enum(),
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

    fn llvm_function_signature(&self, func_id: &FuncId, resolved_generics: &ResolvedGenerics) -> String {
        let function = self.project.get_func_by_id(func_id);

        let params = function.params.iter().map(|p| (p.type_id, p.is_variadic)).collect();
        self.llvm_function_signature_by_parts(&function.name, Some(&function.kind), &function.generic_ids, &params, &function.return_type_id, resolved_generics)
    }

    fn llvm_function_signature_by_parts(&self, name: &String, kind: Option<&FunctionKind>, generic_ids: &Vec<TypeId>, params: &Vec<(TypeId, bool)>, return_type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> String {
        let type_args = generic_ids.iter()
            .map(|type_id| self.llvm_type_name_by_id(type_id, resolved_generics))
            .join(",");
        let type_args = if generic_ids.is_empty() { "".into() } else { format!("<{}>", type_args) };
        let params = params.iter()
            .map(|(param_type_id, param_is_variadic)| {
                let param_type_name = self.llvm_type_name_by_id(param_type_id, resolved_generics);
                if *param_is_variadic {
                    format!("Array<{param_type_name}>")
                } else {
                    param_type_name
                }
            })
            .join(",");
        let ret = self.llvm_type_name_by_id(return_type_id, resolved_generics);

        let prefix = match &kind {
            None | Some(FunctionKind::Freestanding) => "".into(),
            Some(FunctionKind::Method(type_id)) => format!("{}#", self.llvm_type_name_by_id(type_id, resolved_generics)),
            Some(FunctionKind::StaticMethod(type_id)) => format!("{}.", self.llvm_type_name_by_id(type_id, &ResolvedGenerics::default())),
        };
        format!("{prefix}{name}{type_args}({params}):{ret}")
    }

    fn llvm_initializer_signature(&self, struct_id: &StructId, resolved_generics: &ResolvedGenerics) -> String {
        let struct_ = self.project.get_struct_by_id(struct_id);

        let llvm_type_name = self.llvm_type_name_by_id(&struct_.self_type_id, resolved_generics);
        format!("{}.init", llvm_type_name)
    }

    fn llvm_function_type(&self, func_id: &FuncId, resolved_generics: &ResolvedGenerics) -> FunctionType<'a> {
        let function = self.project.get_func_by_id(func_id);

        let mut num_optional_params = 0;
        let params = function.params.iter()
            .map(|p| {
                if p.default_value.is_some() {
                    num_optional_params += 1;
                }

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
        let mut params = params.map(|ty| ty.into()).collect_vec();
        if num_optional_params > 0 {
            if num_optional_params > 16 { unimplemented!("A function can have at most 16 optional parameters currently"); }
            params.push(self.i16().into());
        }
        if !function.captured_vars.is_empty() {
            params.insert(0, self.closure_captures_t().into());
        }

        if function.return_type_id == PRELUDE_UNIT_TYPE_ID {
            self.context.void_type().fn_type(params.as_slice(), false)
        } else {
            let Some(ret_llvm_type) = self.llvm_underlying_type_by_id(&function.return_type_id, resolved_generics) else { todo!() };
            let ret_llvm_type = self.llvm_ptr_wrap_type_if_needed(ret_llvm_type);

            self.fn_type(ret_llvm_type, params.as_slice())
        }
    }

    fn llvm_function_type_by_parts(&self, param_type_ids: &Vec<TypeId>, num_required_params: usize, is_variadic: bool, return_type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> FunctionType<'a> {
        debug_assert!(!is_variadic);

        let param_types = param_type_ids.iter()
            .take(num_required_params)
            .map(|param_type_id| {
                let Some(llvm_type) = self.llvm_underlying_type_by_id(param_type_id, resolved_generics) else { todo!() };
                self.llvm_ptr_wrap_type_if_needed(llvm_type).into()
            })
            .collect_vec();

        if return_type_id == &PRELUDE_UNIT_TYPE_ID {
            self.context.void_type().fn_type(param_types.as_slice(), false)
        } else {
            let Some(ret_llvm_type) = self.llvm_underlying_type_by_id(&return_type_id, resolved_generics) else { todo!() };
            let ret_llvm_type = self.llvm_ptr_wrap_type_if_needed(ret_llvm_type);

            self.fn_type(ret_llvm_type, param_types.as_slice())
        }
    }

    fn llvm_initializer_type(&self, struct_id: &StructId, resolved_generics: &ResolvedGenerics) -> FunctionType<'a> {
        let struct_ = self.project.get_struct_by_id(struct_id);

        let mut num_optional_fields = 0;
        let mut params = struct_.fields.iter()
            .map(|f| {
                if f.default_value.is_some() {
                    num_optional_fields += 1;
                }

                let Some(llvm_type) = self.llvm_underlying_type_by_id(&f.type_id, resolved_generics) else { todo!() };
                self.llvm_ptr_wrap_type_if_needed(llvm_type).into()
            })
            .collect_vec();

        if num_optional_fields > 0 {
            if num_optional_fields > 16 { unimplemented!("A type can have at most 16 optional fields currently"); }
            params.push(self.i16().into());
        }

        let Some(ret_llvm_type) = self.llvm_underlying_type_by_id(&struct_.self_type_id, resolved_generics) else { todo!() };
        let ret_llvm_type = self.llvm_ptr_wrap_type_if_needed(ret_llvm_type);

        self.fn_type(ret_llvm_type, params.as_slice())
    }

    fn get_generic_name(&self, type_id: &TypeId) -> String {
        let Type::Generic(_, generic_name) = self.project.get_type_by_id(type_id) else { unreachable!("TypeId {:?} represents a type that is not a generic", type_id) };
        generic_name.clone()
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
        let (abra_main_fn, _) = self.current_fn;
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
            // There's no top-level code to execute in the prelude, so we can skip it.
            if m.id == PRELUDE_MODULE_ID { continue; }

            // The top-level code in a module is executed in the special $mod_{mod_id} fn...
            let mod_fn_name = format!("$mod_{}", m.id.0);
            let mod_fn_type = self.fn_type(self.bool(), &[]);
            let mod_fn = self.main_module.add_function(&mod_fn_name, mod_fn_type, None);
            let prev_fn = self.current_fn;
            self.current_fn = (mod_fn, None);
            let block = self.context.append_basic_block(mod_fn, "");
            self.builder.position_at_end(block);

            let empty_generics = ResolvedGenerics::default();
            let num_nodes = m.code.len();
            for (idx, node) in m.code.iter().enumerate() {
                let res = self.visit_statement(node, &empty_generics);
                let node_type_id = node.type_id();
                if idx == num_nodes - 1 && *node_type_id != PRELUDE_UNIT_TYPE_ID {
                    if let Some(res) = res {
                        let node_type = self.project.get_type_by_id(node_type_id);
                        let (_, tostring_func_id) = node_type.find_method_by_name(self.project, "toString").unwrap();
                        let (_, generics) = self.project.get_struct_by_type_id(node_type_id).unwrap();
                        let resolved_generics = ResolvedGenerics(generics.iter().map(|(k, v)| (self.get_generic_name(k), *v)).collect());
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
            let (current_fn, _) = self.current_fn;
            debug_assert!(current_fn.get_name().to_str().unwrap() == ABRA_MAIN_FN_NAME);
            // Call the $mod_{mod_id} fn in the _abra_main fn.
            self.builder.position_at_end(current_fn.get_last_basic_block().unwrap());
            self.builder.build_call(mod_fn, &[], "");
        }

        self.end_abra_main();
    }

    fn create_closure_captures(&self, function: &Function) -> PointerValue<'a> {
        // Create array of captured variables for a closure. This is implemented as an `i64*`, where each `i64` item is an encoded representation
        // of the closed-over value. Variables are known to be captured at compile-time, so when they're initialized they're moved to the heap.
        // When constructing this array, allocate enough memory to hold all known captured variables (each of which will be a pointer), and treat
        // that pointer as an i64 which is stored in this chunk of memory. Upon retrieval, the value will be converted back into the appropriate
        // type, which is also known at compile-time. Using an `i64*` as the captures array helps simplify the model behind the scenes, and makes
        // calling functions/closures simpler.
        let malloc_size = self.const_i64((function.captured_vars.len() * 8) as u64);
        let captured_vars_mem = self.malloc(malloc_size, self.closure_captures_t());
        for (idx, captured_var_id) in function.captured_vars.iter().enumerate() {
            let captured_var = self.project.get_var_by_id(captured_var_id);
            let llvm_var = self.ctx_stack.last().unwrap().variables.get(&captured_var_id).expect(&format!("No stored slot for variable {} ({:?})", &captured_var.name, &captured_var));
            let captured_var_value = match llvm_var {
                LLVMVar::Slot(slot) => {
                    let val = self.builder.build_load(*slot, &captured_var.name);
                    debug_assert!(val.is_pointer_value(), "Captured variables should be lifted to heap space upon initialization");
                    self.builder.build_ptr_to_int(val.into_pointer_value(), self.i64(), &format!("capture_{}_ptr_as_value", &captured_var.name))
                }
                LLVMVar::Param(_) => todo!(),
            };
            let slot = unsafe { self.builder.build_gep(captured_vars_mem, &[self.const_i32(idx as u64).into()], &format!("captured_var_{}_slot", &captured_var.name)) };
            self.builder.build_store(slot, captured_var_value);
        }

        captured_vars_mem
    }

    fn get_captured_var_slot(&self, var_id: &VarId, resolved_generics: &ResolvedGenerics) -> Option<PointerValue<'a>> {
        // See `self.create_closure_captures` for more explanation of the underlying data model for the captures array.
        // When retrieving a captured variable, we expect that we are in a function context, and that the variable being
        // resolved, if it's a capture of that function, will be included in the function's `captured_vars` list. If so,
        // retrieve it by index (known at compile-time). This value will be an `i64`, since the captures is of type `i64*`,
        // so we need to decode that `i64` back into a pointer of the appropriate type for that captured variable.
        let variable = self.project.get_var_by_id(var_id);
        if variable.is_captured {
            if let Some(func_id) = self.current_fn.1 {
                let current_function = self.project.get_func_by_id(&func_id);
                if let Some((captured_var_idx, _)) = current_function.captured_vars.iter().find_position(|v| v == &var_id) {
                    let current_func_captures_arg = self.current_fn.0.get_nth_param(0).unwrap().into_pointer_value();
                    let captured_arg_slot = unsafe { self.builder.build_gep(current_func_captures_arg, &[self.const_i32(captured_var_idx as u64).into()], &format!("captured_arg_{}_slot", &variable.name)) };
                    let encoded_captured_arg = self.builder.build_load(captured_arg_slot, &format!("captured_arg_{}", &variable.name)).into_int_value();

                    let Some(llvm_type) = self.llvm_underlying_type_by_id(&variable.type_id, resolved_generics) else { todo!() };
                    let llvm_type = self.llvm_ptr_wrap_type_if_needed(llvm_type);
                    let ptr = self.builder.build_int_to_ptr(encoded_captured_arg, self.ptr(llvm_type), "");
                    return Some(ptr);
                }
            }
        }

        None
    }

    fn visit_statement(&mut self, node: &TypedNode, resolved_generics: &ResolvedGenerics) -> Option<BasicValueEnum<'a>> {
        match node {
            node @ TypedNode::If { .. } => self.visit_if_node(node, resolved_generics),
            TypedNode::Match { .. } => todo!(),
            TypedNode::FuncDeclaration(func_id) => {
                let function = self.project.get_func_by_id(func_id);
                if !function.captured_vars.is_empty() {
                    // If a function captures variables, gather those captures into a captures array, and store as global (this works
                    // for now because at the moment all functions are defined at the top-level, but this will need to change in order
                    // to support nested functions, methods, and lambda expressions). This global is used later on to create a runtime
                    // function value, when a function-aliased identifier is referenced in a non-invocation context.
                    let captured_vars_mem = self.create_closure_captures(function);
                    let captures_name = format!("captures_{}_{}_{}_{}", func_id.0.0.0, func_id.0.1, func_id.1, &function.name);
                    let global = self.main_module.add_global(self.ptr(self.i64()), None, &captures_name);
                    global.set_constant(false);
                    global.set_initializer(&self.closure_captures_t().const_null());
                    self.builder.build_store(global.as_pointer_value(), captured_vars_mem);
                }

                None
            }
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
                let expr_val = self.visit_expression(expr, resolved_generics);
                if let Some(expr_val) = expr_val {
                    let llvm_type = self.llvm_underlying_type_by_id(&variable.type_id, resolved_generics).unwrap_or_else(|| expr_val.get_type().as_basic_type_enum());
                    let llvm_type = self.llvm_ptr_wrap_type_if_needed(llvm_type);

                    // If variable is captured, move value to heap so its lifetime extends beyond the current stack frame. There is specific logic
                    // to handle references to the variable later on (see TypedNode::Identifier and TypedNode::Assignment logic).
                    if variable.is_captured {
                        let ptr_type = llvm_type.ptr_type(AddressSpace::Generic);
                        let heap_mem = self.malloc(self.const_i64(8), ptr_type);
                        self.builder.build_store(heap_mem, expr_val);

                        let slot = self.builder.build_alloca(ptr_type, &var_name);
                        self.ctx_stack.last_mut().unwrap().variables.insert(variable.id, LLVMVar::Slot(slot));

                        self.builder.build_store(slot, heap_mem);
                    } else {
                        let slot = self.builder.build_alloca(llvm_type, &var_name);
                        self.ctx_stack.last_mut().unwrap().variables.insert(variable.id, LLVMVar::Slot(slot));

                        self.builder.build_store(slot, expr_val);
                    }
                }

                None
            }
            TypedNode::ForLoop { .. } => todo!(),
            TypedNode::WhileLoop { condition, condition_var_id, body, .. } => {
                debug_assert!(condition_var_id.is_none(), "Not implemented yet");
                debug_assert!(condition.as_ref().type_id() == &PRELUDE_BOOL_TYPE_ID, "Only implement while loops for boolean conditions for now (no Optionals yet)");

                let loop_cond_block = self.context.append_basic_block(self.current_fn.0, "while_loop_cond");
                let loop_body_block = self.context.append_basic_block(self.current_fn.0, "while_loop_body");
                let loop_end_block = self.context.append_basic_block(self.current_fn.0, "while_loop_end");

                self.builder.build_unconditional_branch(loop_cond_block);
                self.builder.position_at_end(loop_cond_block);
                let cond_val = self.visit_expression(condition, resolved_generics).unwrap().into_int_value();
                let comp = self.builder.build_int_compare(IntPredicate::EQ, cond_val, self.const_bool(true), "");
                self.builder.build_conditional_branch(comp, loop_body_block, loop_end_block);

                self.builder.position_at_end(loop_body_block);
                for node in body {
                    self.visit_statement(node, resolved_generics);
                }
                self.builder.build_unconditional_branch(loop_cond_block);

                self.builder.position_at_end(loop_end_block);

                None
            }
            TypedNode::Break { .. } |
            TypedNode::Continue { .. } |
            TypedNode::Return { .. } => todo!(),
            TypedNode::Import { .. } => None,
            _ => self.visit_expression(node, resolved_generics),
        }
    }

    fn visit_expression(&mut self, node: &TypedNode, resolved_generics: &ResolvedGenerics) -> Option<BasicValueEnum<'a>> {
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
                        let str_val = self.builder.build_global_string_ptr(&s, "").as_pointer_value();
                        let len_val = self.const_i64(s.len() as u64);
                        self.construct_string(len_val, str_val)
                    }
                };

                if self.project.type_is_option(resolved_type_id).is_some() {
                    return Some(self.make_option_instance(resolved_type_id, value, resolved_generics).into());
                } else if resolved_type_id == &PRELUDE_ANY_TYPE_ID {
                    return Some(self.make_trait_instance(&PRELUDE_ANY_TYPE_ID, &type_id, value, resolved_generics).into());
                }

                Some(value)
            }
            TypedNode::Unary { op, expr, resolved_type_id, .. } => {
                let type_id = expr.as_ref().type_id();

                let expr_val = self.visit_expression(expr, resolved_generics).unwrap();

                let value = match op {
                    UnaryOp::Minus => {
                        if type_id == &PRELUDE_INT_TYPE_ID {
                            self.builder.build_int_neg(expr_val.into_int_value(), "").into()
                        } else if type_id == &PRELUDE_FLOAT_TYPE_ID {
                            self.builder.build_float_neg(expr_val.into_float_value(), "").into()
                        } else {
                            unreachable!("`-` unary operator not defined for type {}", self.project.type_repr(type_id))
                        }
                    }
                    UnaryOp::Negate => {
                        if type_id == &PRELUDE_BOOL_TYPE_ID {
                            self.builder.build_not(expr_val.into_int_value(), "").into()
                        } else {
                            unreachable!("`!` unary operator not defined for type {}", self.project.type_repr(type_id))
                        }
                    }
                };

                if self.project.type_is_option(resolved_type_id).is_some() {
                    return Some(self.make_option_instance(resolved_type_id, value, resolved_generics).into());
                } else if resolved_type_id == &PRELUDE_ANY_TYPE_ID {
                    return Some(self.make_trait_instance(&PRELUDE_ANY_TYPE_ID, &type_id, value, resolved_generics).into());
                }

                Some(value)
            }
            TypedNode::Binary { left, op, right, type_id, resolved_type_id, .. } => {
                let left_type_id = left.as_ref().type_id();
                let right_type_id = right.as_ref().type_id();

                let value = match op {
                    BinaryOp::Add => {
                        if left_type_id == &PRELUDE_STRING_TYPE_ID || right_type_id == &PRELUDE_STRING_TYPE_ID {
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
                                arguments: vec![
                                    Some(*right.clone()),
                                    Some({
                                        // TODO: Fix this, this is a terrible hack. We need the TypeId for `Any[]` in order to properly construct the empty varargs parameter.
                                        let any_array_type_id = self.project.prelude_module().scopes.iter()
                                            .find_map(|scope| {
                                                self.project.find_type_id(&scope.id, &self.project.array_type(PRELUDE_ANY_TYPE_ID))
                                            })
                                            .expect("TypeId for Any[] should exist because it was defined for `print` and `println`");
                                        TypedNode::Array {
                                            token: Token::LBrack(left.span().start, false),
                                            items: vec![],
                                            type_id: any_array_type_id,
                                            resolved_type_id: any_array_type_id,
                                        }
                                    }),
                                ],
                                type_arg_ids: vec![*right_type_id],
                                type_id: PRELUDE_STRING_TYPE_ID,
                                resolved_type_id: *resolved_type_id,
                            }, resolved_generics);
                        } else {
                            let left_val = self.visit_expression(left, resolved_generics).unwrap();
                            let right_val = self.visit_expression(right, resolved_generics).unwrap();
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
                            } else {
                                unreachable!("`+` operator not defined between types {} and {}", self.project.type_repr(left_type_id), self.project.type_repr(right_type_id))
                            }
                        }
                    }
                    BinaryOp::Sub => {
                        let left = self.visit_expression(left, resolved_generics).unwrap();
                        let right = self.visit_expression(right, resolved_generics).unwrap();
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
                        let left = self.visit_expression(left, resolved_generics).unwrap();
                        let right = self.visit_expression(right, resolved_generics).unwrap();
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
                        let left = self.visit_expression(left, resolved_generics).unwrap();
                        let right = self.visit_expression(right, resolved_generics).unwrap();
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
                    BinaryOp::Mod => {
                        let left = self.visit_expression(left, resolved_generics).unwrap();
                        let right = self.visit_expression(right, resolved_generics).unwrap();
                        if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            self.builder.build_int_signed_rem(left.into_int_value(), right.into_int_value(), "").into()
                        } else if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_FLOAT_TYPE_ID {
                            let left = self.builder.build_signed_int_to_float(left.into_int_value(), self.f64(), "");
                            self.builder.build_float_rem(left, right.into_float_value(), "").into()
                        } else if left_type_id == &PRELUDE_FLOAT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            let right = self.builder.build_signed_int_to_float(right.into_int_value(), self.f64(), "");
                            self.builder.build_float_rem(left.into_float_value(), right, "").into()
                        } else if left_type_id == &PRELUDE_FLOAT_TYPE_ID && right_type_id == &PRELUDE_FLOAT_TYPE_ID {
                            self.builder.build_float_rem(left.into_float_value(), right.into_float_value(), "").into()
                        } else {
                            unreachable!("`%` operator not defined between types {} and {}", self.project.type_repr(left_type_id), self.project.type_repr(right_type_id))
                        }
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        debug_assert!(left_type_id == &PRELUDE_BOOL_TYPE_ID);
                        debug_assert!(right_type_id == &PRELUDE_BOOL_TYPE_ID);

                        let left_val = self.visit_expression(left, resolved_generics).unwrap();

                        let op_name = if op == &BinaryOp::And { "and" } else { "or" };
                        let then_bb = self.context.append_basic_block(self.current_fn.0, &format!("binary_{op_name}_then"));
                        let else_bb = self.context.append_basic_block(self.current_fn.0, &format!("binary_{op_name}_else"));
                        let cont_bb = self.context.append_basic_block(self.current_fn.0, &format!("binary_{op_name}_cont"));

                        let cond = self.builder.build_int_compare(IntPredicate::EQ, left_val.into_int_value(), self.const_bool(true), "");
                        self.builder.build_conditional_branch(cond, then_bb, else_bb);

                        self.builder.position_at_end(then_bb);
                        let then_value = if op == &BinaryOp::And { self.visit_expression(right, resolved_generics).unwrap() } else { left_val };
                        let then_bb = self.builder.get_insert_block().unwrap();
                        self.builder.build_unconditional_branch(cont_bb);

                        self.builder.position_at_end(else_bb);
                        let else_value = if op == &BinaryOp::And { left_val } else { self.visit_expression(right, resolved_generics).unwrap() };
                        let else_bb = self.builder.get_insert_block().unwrap();
                        self.builder.build_unconditional_branch(cont_bb);

                        self.builder.position_at_end(cont_bb);
                        let phi = self.builder.build_phi(self.bool(), &format!("{op_name}_value"));
                        phi.add_incoming(&[(&then_value, then_bb), (&else_value, else_bb)]);

                        phi.as_basic_value()
                    }
                    BinaryOp::Xor |
                    BinaryOp::Coalesce => todo!(),
                    op @ BinaryOp::Lt | op @ BinaryOp::Lte | op @ BinaryOp::Gt | op @ BinaryOp::Gte => {
                        let comp_op_int = if op == &BinaryOp::Lt { IntPredicate::SLT } else if op == &BinaryOp::Lte { IntPredicate::SLE } else if op == &BinaryOp::Gt { IntPredicate::SGT } else if op == &BinaryOp::Gte { IntPredicate::SGE } else { unreachable!() };
                        let comp_op_float = if op == &BinaryOp::Lt { FloatPredicate::OLT } else if op == &BinaryOp::Lte { FloatPredicate::OLE } else if op == &BinaryOp::Gt { FloatPredicate::OGT } else if op == &BinaryOp::Gte { FloatPredicate::OGE } else { unreachable!() };

                        let left = self.visit_expression(left, resolved_generics).unwrap();
                        let right = self.visit_expression(right, resolved_generics).unwrap();
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

                        let left = self.visit_expression(left, resolved_generics).unwrap();
                        let right = self.visit_expression(right, resolved_generics).unwrap();
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

                if self.project.type_is_option(resolved_type_id).is_some() {
                    Some(self.make_option_instance(resolved_type_id, value, resolved_generics).into())
                } else if resolved_type_id == &PRELUDE_ANY_TYPE_ID {
                    return Some(self.make_trait_instance(&PRELUDE_ANY_TYPE_ID, &type_id, value, resolved_generics).into());
                } else {
                    Some(value)
                }
            }
            TypedNode::Grouped { expr, .. } => self.visit_expression(expr, resolved_generics),
            TypedNode::Array { items, type_id, resolved_type_id, .. } => {
                let Type::GenericInstance(struct_id, generics) = self.project.get_type_by_id(type_id) else { unreachable!() };
                debug_assert!(struct_id == &self.project.prelude_array_struct_id);
                let array_struct = self.project.get_struct_by_id(struct_id);
                let inner_type_id = generics[0];
                let array_type = self.project.get_type_by_id(&self.project.find_type_id(&PRELUDE_SCOPE_ID, &Type::Type(TypeKind::Struct(*struct_id))).unwrap());

                let array_with_capacity_llvm_fn = {
                    let array_with_capacity_func_id = array_type.find_static_method_by_name(self.project, "withCapacity").unwrap();
                    let array_with_capacity_fn = self.project.get_func_by_id(array_with_capacity_func_id);

                    let resolved_generics = ResolvedGenerics::from_pairs(vec![(self.get_generic_name(&array_with_capacity_fn.generic_ids[0]), inner_type_id)]);
                    self.get_or_compile_function(&array_with_capacity_func_id, &resolved_generics)
                };
                let array_push_llvm_fn = {
                    let (_, array_push_func_id) = array_type.find_method_by_name(self.project, "push").unwrap();
                    let resolved_generics = ResolvedGenerics::from_pairs(vec![(self.get_generic_name(&array_struct.generic_ids[0]), inner_type_id)]);
                    self.get_or_compile_function(&array_push_func_id, &resolved_generics)
                };

                let arr_val = self.builder.build_call(array_with_capacity_llvm_fn, &[self.const_i64(items.len() as u64).into()], "").try_as_basic_value().left().unwrap();
                for item in items {
                    let item = self.visit_expression(item, resolved_generics).unwrap();
                    self.builder.build_call(array_push_llvm_fn, &[arr_val.into(), item.into()], "").try_as_basic_value().left();
                }

                if self.project.type_is_option(resolved_type_id).is_some() {
                    let arr_val_local = self.builder.build_alloca(arr_val.get_type(), "");
                    self.builder.build_store(arr_val_local, arr_val);
                    let arr_val = self.builder.build_load(arr_val_local, "");
                    return Some(self.make_option_instance(resolved_type_id, arr_val, resolved_generics).into());
                } else if resolved_type_id == &PRELUDE_ANY_TYPE_ID {
                    let resolved_generics = resolved_generics.extend_via_struct(array_struct, generics, &self.project);
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

                let value = match variable.alias {
                    VariableAlias::None => {
                        // If the variable is captured, then the underlying model is different and needs to be handled specially.
                        // Captured variables are moved to the heap upon initialization (see TypedNode::BindingDeclaration), and
                        // their backing type uses pointer indirection. If we're currently in a function which closes over the
                        // variable, we handle it here...
                        if let Some(ptr) = self.get_captured_var_slot(var_id, resolved_generics) {
                            let decoded_captured_val = self.builder.build_load(ptr, "");
                            return Some(decoded_captured_val);
                        }

                        let llvm_var = self.ctx_stack.last().unwrap().variables.get(&variable.id).expect(&format!("No stored slot for variable {} ({:?})", &variable.name, &variable));
                        match llvm_var {
                            LLVMVar::Slot(slot) => {
                                let val = self.builder.build_load(*slot, &variable.name);
                                // ...otherwise, we need to handle it here. If the variable is captured by some closure, then it'll
                                // be represented as a pointer value which needs to be dereferenced upon access.
                                if variable.is_captured {
                                    self.builder.build_load(val.into_pointer_value(), "")
                                } else {
                                    val
                                }
                            }
                            LLVMVar::Param(value) => *value,
                        }
                    }
                    VariableAlias::Function(func_id) => {
                        // todo: cache value to not create duplicate?
                        let function = self.project.get_func_by_id(&func_id);
                        let captures = if !function.captured_vars.is_empty() {
                            let captures_name = format!("captures_{}_{}_{}_{}", func_id.0.0.0, func_id.0.1, func_id.1, &function.name);
                            let captures_ptr = self.main_module.get_global(&captures_name).unwrap().as_pointer_value();
                            Some(self.builder.build_load(captures_ptr, "").into_pointer_value())
                        } else {
                            None
                        };

                        self.make_function_value(&func_id, resolved_type_id, captures, resolved_generics)
                    }
                    VariableAlias::Type(_) => todo!()
                };

                if self.project.type_is_option(&variable.type_id).is_none() && self.project.type_is_option(resolved_type_id).is_some() {
                    Some(self.make_option_instance(resolved_type_id, value, resolved_generics).into())
                } else if resolved_type_id == &PRELUDE_ANY_TYPE_ID && variable.type_id != PRELUDE_ANY_TYPE_ID {
                    let resolved_generics = resolved_generics.extend_via_instance(&variable.type_id, &self.project);
                    Some(self.make_trait_instance(&PRELUDE_ANY_TYPE_ID, &variable.type_id, value, &resolved_generics).into())
                } else {
                    Some(value)
                }
            }
            TypedNode::NoneValue { resolved_type_id, .. } => {
                Some(self.make_none_option_instance(resolved_type_id, resolved_generics).into())
            }
            TypedNode::Invocation { target, arguments, type_arg_ids, type_id, resolved_type_id, .. } => {
                let mut args = Vec::with_capacity(arguments.len());

                let params_data;
                let mut new_resolved_generics = ResolvedGenerics::default();
                let llvm_fn_val = match &**target {
                    // Handle invocation of identifiers which could be aliases for functions or types. Non-aliased variables (which could be function values)
                    // will be handled alongside other arbitrary expressions in the catchall block of this match.
                    TypedNode::Identifier { var_id, .. } if self.project.get_var_by_id(var_id).alias != VariableAlias::None => {
                        let variable = self.project.get_var_by_id(var_id);
                        match &variable.alias {
                            VariableAlias::None => unreachable!(),
                            VariableAlias::Function(func_id) => {
                                let function = self.project.get_func_by_id(func_id);
                                params_data = function.params.iter().map(|p| (p.type_id, p.default_value.is_some())).collect_vec();

                                new_resolved_generics = resolved_generics.new_via_func_call(function, type_arg_ids, &self.project);

                                if let Some(dec) = function.decorators.iter().find(|dec| dec.name == "Intrinsic") {
                                    let TypedNode::Literal { value: TypedLiteral::String(intrinsic_name), .. } = &dec.args[0] else { unreachable!("@Intrinsic requires exactly 1 String argument") };
                                    return self.compile_intrinsic_invocation(type_arg_ids, &new_resolved_generics, intrinsic_name, Some(&**target), arguments, resolved_type_id);
                                }

                                if !function.captured_vars.is_empty() {
                                    let fn_obj = self.visit_expression(target, resolved_generics).unwrap().into_pointer_value();

                                    let captures_slot = self.builder.build_struct_gep(fn_obj, 0, "captures_slot").unwrap();
                                    let captures_arr = self.builder.build_load(captures_slot, "captures");
                                    args.push(captures_arr.into());

                                    let fn_ptr_slot = self.builder.build_struct_gep(fn_obj, 1, "fn_ptr_slot").unwrap();
                                    let fn_ptr = self.builder.build_load(fn_ptr_slot, "fn_ptr").into_pointer_value();

                                    // The fn_ptr of a Function value is typed in such a way that the `captures` parameter is lost. If we know it's a closure (which
                                    // we can determine programmatically if necessary based on whether there's a non-NULL `captures` value), we need to programmatically
                                    // cast this function pointer to a different type (namely, the same signature, but with an `i64*` as the first parameter). Here
                                    // though, we don't need to do it programmatically since we know here at compile-time that the function is a closure. When all we
                                    // have is a function value though (see wildcard case further on), this is where we'll need to do the programmatic check.
                                    let underlying_fn_type = fn_ptr.get_type().get_element_type().into_function_type();
                                    let underlying_fn_param_types = underlying_fn_type.get_param_types();
                                    let mut new_fn_param_types = vec![self.closure_captures_t().into()];
                                    new_fn_param_types.extend(underlying_fn_param_types);
                                    let new_fn_param_types = new_fn_param_types.into_iter().map(|t| t.into()).collect_vec();
                                    let new_fn_type = if let Some(ret_type) = underlying_fn_type.get_return_type() {
                                        ret_type.fn_type(&new_fn_param_types.as_slice(), false)
                                    } else {
                                        self.context.void_type().fn_type(&new_fn_param_types.as_slice(), false)
                                    };
                                    let fn_ptr = self.builder.build_pointer_cast(fn_ptr, new_fn_type.ptr_type(AddressSpace::Generic), "");

                                    CallableValue::try_from(fn_ptr).unwrap()
                                } else {
                                    self.get_or_compile_function(func_id, &new_resolved_generics).into()
                                }
                            }
                            VariableAlias::Type(TypeKind::Enum(_)) => unreachable!("Cannot invoke an enum directly"),
                            VariableAlias::Type(TypeKind::Struct(struct_id)) => {
                                let struct_ = self.project.get_struct_by_id(struct_id);
                                params_data = struct_.fields.iter().map(|f| (f.type_id, f.default_value.is_some())).collect_vec();

                                new_resolved_generics = resolved_generics.new_via_struct(struct_, type_arg_ids, &self.project);
                                self.get_or_compile_type_initializer(struct_id, &new_resolved_generics).into()
                            }
                        }
                    }
                    TypedNode::Accessor { target, kind, member_idx, .. } => {
                        let mut target_type_id = target.type_id();
                        if let Type::Generic(_, generic_name) = self.project.get_type_by_id(target_type_id) {
                            target_type_id = resolved_generics.resolve(generic_name).unwrap_or(target_type_id)
                        };
                        let target_ty = self.project.get_type_by_id(target_type_id);

                        match kind {
                            AccessorKind::Field => todo!(),
                            AccessorKind::Method if self.project.type_is_option(target_type_id).is_some() => {
                                let Some(inner_type_id) = self.project.type_is_option(target_type_id) else { unreachable!() };

                                let func_id = self.project.get_type_by_id(&inner_type_id).get_method(self.project, *member_idx).unwrap();
                                let function = self.project.get_func_by_id(&func_id);
                                params_data = function.params.iter().skip(1).map(|p| (p.type_id, p.default_value.is_some())).collect_vec();

                                let target = self.visit_expression(target, &resolved_generics).unwrap();
                                args.push(target.into());

                                self.get_or_compile_option_method(&resolved_generics, &inner_type_id, member_idx).into()
                            }
                            AccessorKind::Method if self.project.type_is_trait(target_type_id) => {
                                debug_assert!(arguments.iter().all(|a| a.is_some()), "Trait methods with default-valued parameters not yet supported");
                                let target_type_id = *target_type_id;

                                let instance = self.visit_expression(target, &resolved_generics).unwrap();
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
                                        params_data = vec![(target_type_id, false)];

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
                                params_data = function.params.iter().skip(1).map(|p| (p.type_id, p.default_value.is_some())).collect_vec();

                                if let Some(dec) = function.decorators.iter().find(|dec| dec.name == "Intrinsic") {
                                    let TypedNode::Literal { value: TypedLiteral::String(intrinsic_name), .. } = &dec.args[0] else { unreachable!("@Intrinsic requires exactly 1 String argument") };
                                    return self.compile_intrinsic_invocation(type_arg_ids, &resolved_generics, intrinsic_name, Some(&**target), arguments, resolved_type_id);
                                }

                                let target = self.visit_expression(target, &resolved_generics).unwrap();
                                args.push(target.into());

                                new_resolved_generics = resolved_generics.new_via_method_call(target_ty, function, type_arg_ids, &self.project);
                                self.get_or_compile_function(&func_id, &new_resolved_generics).into()
                            }
                            AccessorKind::StaticMethod => {
                                let func_id = target_ty.get_static_method(self.project, *member_idx).unwrap();
                                let function = self.project.get_func_by_id(&func_id);
                                params_data = function.params.iter().map(|p| (p.type_id, p.default_value.is_some())).collect_vec();

                                new_resolved_generics = resolved_generics.new_via_func_call(function, type_arg_ids, &self.project);

                                if let Some(dec) = function.decorators.iter().find(|dec| dec.name == "Intrinsic") {
                                    let TypedNode::Literal { value: TypedLiteral::String(intrinsic_name), .. } = &dec.args[0] else { unreachable!("@Intrinsic requires exactly 1 String argument") };
                                    return self.compile_intrinsic_invocation(type_arg_ids, &new_resolved_generics, intrinsic_name, None, arguments, resolved_type_id);
                                }

                                self.get_or_compile_function(&func_id, &new_resolved_generics).into()
                            }
                            AccessorKind::EnumVariant => todo!(),
                        }
                    }
                    _ => {
                        let Type::Function(parameter_type_ids, _num_required_params, _is_variadic, _return_type_id) = self.project.get_type_by_id(target.type_id()) else { unreachable!() };
                        params_data = parameter_type_ids.iter().map(|type_id| (*type_id, false)).collect_vec();
                        new_resolved_generics = resolved_generics.clone();

                        let fn_obj = self.visit_expression(target, resolved_generics).unwrap().into_pointer_value();
                        let fn_ptr_slot = self.builder.build_struct_gep(fn_obj, 1, "fn_ptr_slot").unwrap();
                        let fn_ptr = self.builder.build_load(fn_ptr_slot, "fn_ptr").into_pointer_value();
                        CallableValue::try_from(fn_ptr).unwrap()
                    }
                };

                let num_optional_params = params_data.iter().filter(|(_, has_default)| *has_default).count();

                let mut default_value_flags = 0i16;
                let mut default_value_param_idx = 0;
                for (arg_node, (param_type_id, param_has_default_value)) in arguments.iter().zip(&params_data) {
                    let arg_value = if let Some(arg_node) = arg_node {
                        self.visit_expression(arg_node, &new_resolved_generics).expect("Unit cannot be a valid argument value")
                    } else {
                        if *param_has_default_value {
                            default_value_flags |= 1 << default_value_param_idx;
                        }

                        let Some(llvm_type) = self.llvm_underlying_type_by_id(param_type_id, &new_resolved_generics) else { todo!() };
                        let llvm_type = self.llvm_ptr_wrap_type_if_needed(llvm_type);
                        llvm_type.const_zero()
                    };

                    if *param_has_default_value {
                        default_value_param_idx += 1;
                    }

                    args.push(arg_value.into());
                }

                if num_optional_params > 0 {
                    args.push(self.const_i16(default_value_flags).into());
                }

                let value = self.builder.build_call(llvm_fn_val, args.as_slice(), "").try_as_basic_value().left();
                if let Some(value) = value {
                    if !self.project.type_is_option(type_id).is_some() && self.project.type_is_option(resolved_type_id).is_some() {
                        let value_local = self.builder.build_alloca(value.get_type(), "");
                        self.builder.build_store(value_local, value);
                        let value = self.builder.build_load(value_local, "");
                        return Some(self.make_option_instance(resolved_type_id, value, &new_resolved_generics).into());
                    } else if resolved_type_id == &PRELUDE_ANY_TYPE_ID {
                        let resolved_generics = new_resolved_generics.extend_via_instance(type_id, &self.project);
                        return Some(self.make_trait_instance(&PRELUDE_ANY_TYPE_ID, &type_id, value, &resolved_generics).into());
                    }
                }

                value
            }
            TypedNode::Accessor { target, kind, member_idx, is_opt_safe, type_id, resolved_type_id, .. } => {
                debug_assert!(!*is_opt_safe);

                let target_ty = self.project.get_type_by_id(target.type_id());
                let target = self.visit_expression(target, resolved_generics).unwrap();

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

                if !self.project.type_is_option(type_id).is_some() && self.project.type_is_option(resolved_type_id).is_some() {
                    let value_local = self.builder.build_alloca(value.get_type(), "");
                    self.builder.build_store(value_local, value);
                    let value = self.builder.build_load(value_local, "");
                    return Some(self.make_option_instance(resolved_type_id, value, resolved_generics).into());
                } else if resolved_type_id == &PRELUDE_ANY_TYPE_ID {
                    let resolved_generics = resolved_generics.extend_via_instance(type_id, &self.project);
                    return Some(self.make_trait_instance(&PRELUDE_ANY_TYPE_ID, &type_id, value, &resolved_generics).into());
                }

                Some(value.into())
            }
            TypedNode::Indexing { target, index, type_id, resolved_type_id, .. } => {
                let target_type_id = target.as_ref().type_id();
                let target_ty = self.project.get_type_by_id(target_type_id);

                let empty_generics = vec![];
                let (target_struct_id, target_generics) = if let Type::GenericInstance(target_struct_id, target_generics) = target_ty {
                    (target_struct_id, target_generics)
                } else if target_type_id == &PRELUDE_STRING_TYPE_ID {
                    (&self.project.prelude_string_struct_id, &empty_generics)
                } else {
                    unreachable!("All indexable types are struct instances")
                };

                match index {
                    IndexingMode::Index(idx_expr) if target_struct_id == &self.project.prelude_array_struct_id => {
                        let array_inner_type_id = &target_generics[0];
                        let (array_get_member_idx, array_get_func_id) = target_ty.find_method_by_name(self.project, "get").unwrap();
                        let array_get_function = self.project.get_func_by_id(array_get_func_id);

                        return self.visit_expression(&TypedNode::Invocation {
                            target: Box::new(TypedNode::Accessor {
                                target: target.clone(),
                                kind: AccessorKind::Method,
                                is_opt_safe: false,
                                member_idx: array_get_member_idx,
                                member_span: target.span(),
                                type_id: array_get_function.fn_type_id,
                                type_arg_ids: vec![],
                                resolved_type_id: array_get_function.fn_type_id,
                            }),
                            arguments: vec![
                                Some(*idx_expr.clone()),
                            ],
                            type_arg_ids: vec![*array_inner_type_id],
                            type_id: *type_id,
                            resolved_type_id: *resolved_type_id,
                        }, resolved_generics);
                    }
                    IndexingMode::Index(idx_expr) if target_struct_id == &self.project.prelude_string_struct_id => {
                        let (string_get_member_idx, string_get_func_id) = target_ty.find_method_by_name(self.project, "get").unwrap();
                        let string_get_function = self.project.get_func_by_id(string_get_func_id);

                        return self.visit_expression(&TypedNode::Invocation {
                            target: Box::new(TypedNode::Accessor {
                                target: target.clone(),
                                kind: AccessorKind::Method,
                                is_opt_safe: false,
                                member_idx: string_get_member_idx,
                                member_span: target.span(),
                                type_id: string_get_function.fn_type_id,
                                type_arg_ids: vec![],
                                resolved_type_id: string_get_function.fn_type_id,
                            }),
                            arguments: vec![
                                Some(*idx_expr.clone()),
                            ],
                            type_arg_ids: vec![],
                            type_id: *type_id,
                            resolved_type_id: *resolved_type_id,
                        }, resolved_generics);
                    }
                    IndexingMode::Range(start_expr, end_expr) if target_struct_id == &self.project.prelude_array_struct_id => {
                        let array_inner_type_id = &target_generics[0];
                        let (array_get_range_member_idx, array_get_range_func_id) = target_ty.find_method_by_name(self.project, "getRange").unwrap();
                        let array_get_range_function = self.project.get_func_by_id(array_get_range_func_id);

                        return self.visit_expression(&TypedNode::Invocation {
                            target: Box::new(TypedNode::Accessor {
                                target: target.clone(),
                                kind: AccessorKind::Method,
                                is_opt_safe: false,
                                member_idx: array_get_range_member_idx,
                                member_span: target.span(),
                                type_id: array_get_range_function.fn_type_id,
                                type_arg_ids: vec![],
                                resolved_type_id: array_get_range_function.fn_type_id,
                            }),
                            arguments: vec![
                                start_expr.as_ref().map(|e| *(e.clone())),
                                end_expr.as_ref().map(|e| *(e.clone())),
                            ],
                            type_arg_ids: vec![*array_inner_type_id],
                            type_id: *type_id,
                            resolved_type_id: *resolved_type_id,
                        }, resolved_generics);
                    }
                    IndexingMode::Range(start_expr, end_expr) if target_struct_id == &self.project.prelude_string_struct_id => {
                        let (string_get_range_member_idx, string_get_range_func_id) = target_ty.find_method_by_name(self.project, "getRange").unwrap();
                        let string_get_range_function = self.project.get_func_by_id(string_get_range_func_id);

                        return self.visit_expression(&TypedNode::Invocation {
                            target: Box::new(TypedNode::Accessor {
                                target: target.clone(),
                                kind: AccessorKind::Method,
                                is_opt_safe: false,
                                member_idx: string_get_range_member_idx,
                                member_span: target.span(),
                                type_id: string_get_range_function.fn_type_id,
                                type_arg_ids: vec![],
                                resolved_type_id: string_get_range_function.fn_type_id,
                            }),
                            arguments: vec![
                                start_expr.as_ref().map(|e| *(e.clone())),
                                end_expr.as_ref().map(|e| *(e.clone())),
                            ],
                            type_arg_ids: vec![],
                            type_id: *type_id,
                            resolved_type_id: *resolved_type_id,
                        }, resolved_generics);
                    }
                    _ => unreachable!()
                }
            }
            TypedNode::Lambda { func_id, resolved_type_id, .. } => {
                Some(self.make_function_value(func_id, resolved_type_id, None, resolved_generics))
            }
            TypedNode::Assignment { kind, expr, .. } => {
                let expr_val = self.visit_expression(expr, resolved_generics).unwrap();

                match kind {
                    AssignmentKind::Identifier { var_id } => {
                        let variable = self.project.get_var_by_id(var_id);

                        // If the variable is captured, then the underlying model is different and needs to be handled specially.
                        // Captured variables are moved to the heap upon initialization (see TypedNode::BindingDeclaration), and
                        // their backing type uses pointer indirection. If we're currently in a function which closes over the
                        // variable, we handle it here...
                        if let Some(captured_arg_ptr) = self.get_captured_var_slot(var_id, resolved_generics) {
                            self.builder.build_store(captured_arg_ptr, expr_val);
                            return Some(expr_val);
                        }

                        let llvm_var = self.ctx_stack.last().unwrap().variables.get(var_id).expect(&format!("No known llvm variable for variable '{}' ({:?})", &variable.name, var_id));
                        match llvm_var {
                            LLVMVar::Slot(ptr) => {
                                // ...otherwise, we need to handle it here. If the variable is captured by some closure, then it'll
                                // be represented as a pointer value which needs to be dereferenced upon store.
                                if variable.is_captured {
                                    let captured_var_ptr = self.builder.build_load(*ptr, "").into_pointer_value();
                                    self.builder.build_store(captured_var_ptr, expr_val);
                                } else {
                                    self.builder.build_store(*ptr, expr_val);
                                }
                            }
                            LLVMVar::Param(_) => unreachable!("Parameters are not assignable")
                        }
                    }
                    AssignmentKind::Accessor { target, kind, member_idx } => {
                        let target_ty = self.project.get_type_by_id(target.type_id());
                        let target = self.visit_expression(target, resolved_generics).unwrap();

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
            node @ TypedNode::If { .. } => self.visit_if_node(node, resolved_generics),
            _ => unreachable!("Node {:?} is not an expression and should have been handled in visit_statement", node)
        }
    }

    fn construct_string(&mut self, len_val: IntValue<'a>, str_val: PointerValue<'a>) -> BasicValueEnum<'a> {
        let string_initializer = self.get_or_compile_type_initializer(&self.project.prelude_string_struct_id, &ResolvedGenerics::default());
        self.builder.build_call(string_initializer, &[len_val.into(), str_val.into(), self.const_i16(0).into()], "").try_as_basic_value().left().unwrap()
    }

    fn visit_if_node(&mut self, if_node: &TypedNode, resolved_generics: &ResolvedGenerics) -> Option<BasicValueEnum<'a>> {
        let TypedNode::If { is_statement, condition, condition_binding, if_block, else_block, type_id, resolved_type_id, .. } = if_node else { unreachable!() };

        debug_assert!(condition_binding.is_none(), "Condition bindings not yet implemented");
        debug_assert!(condition.as_ref().type_id() == &PRELUDE_BOOL_TYPE_ID, "Only implement if-statements for boolean conditions for now (no Optionals yet)");

        let then_bb = self.context.append_basic_block(self.current_fn.0, "then_block");
        let else_bb = self.context.append_basic_block(self.current_fn.0, "else_block");
        let end_bb = self.context.append_basic_block(self.current_fn.0, "if_end");

        let cond_val = self.visit_expression(&condition, resolved_generics).unwrap().into_int_value();
        let cmp = self.builder.build_int_compare(IntPredicate::EQ, cond_val, self.const_bool(true), "");
        self.builder.build_conditional_branch(cmp, then_bb, else_bb);

        self.builder.position_at_end(then_bb);
        let mut if_block_value = None;
        let if_block_len = if_block.len();
        for (idx, node) in if_block.iter().enumerate() {
            if idx == if_block_len - 1 {
                if_block_value = self.visit_statement(node, resolved_generics);
            } else {
                self.visit_statement(node, resolved_generics);
            }
        }
        let then_bb = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(end_bb);

        self.builder.position_at_end(else_bb);
        let mut else_block_value = None;
        let else_block_len = else_block.len();
        for (idx, node) in else_block.iter().enumerate() {
            if idx == else_block_len - 1 {
                else_block_value = self.visit_statement(node, resolved_generics);
            } else {
                self.visit_statement(node, resolved_generics);
            }
        }
        let else_bb = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(end_bb);

        self.builder.position_at_end(end_bb);

        if *is_statement {
            None
        } else {
            let Some(llvm_type) = self.llvm_underlying_type_by_id(type_id, resolved_generics) else { todo!() };
            let llvm_type = self.llvm_ptr_wrap_type_if_needed(llvm_type);

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

    fn make_option_instance<V: BasicValue<'a>>(&self, outer_type_id: &TypeId, value: V, resolved_generics: &ResolvedGenerics) -> StructValue<'a> {
        let Type::GenericInstance(struct_id, generics) = self.project.get_type_by_id(outer_type_id) else { unreachable!() };
        debug_assert!(struct_id == &self.project.prelude_option_struct_id);

        let mut inner_type_id = &generics[0];
        if let Type::Generic(_, generic_name) = self.project.get_type_by_id(inner_type_id) {
            inner_type_id = resolved_generics.resolve(generic_name).unwrap_or(inner_type_id);
        }
        let opt_struct = self.project.get_struct_by_id(&self.project.prelude_option_struct_id);
        let resolved_generics = ResolvedGenerics::from_pairs(vec![(self.get_generic_name(&opt_struct.generic_ids[0]), *inner_type_id)]);
        let Some(llvm_type) = self.llvm_underlying_type_by_id(outer_type_id, &resolved_generics) else { todo!() };

        let instance_ptr = self.builder.build_alloca(llvm_type, "opt_instance_ptr");
        let is_set_slot = self.builder.build_struct_gep(instance_ptr, 0, "is_set_slot").unwrap();
        self.builder.build_store(is_set_slot, self.const_bool(true));
        let value_slot = self.builder.build_struct_gep(instance_ptr, 1, "value_slot").unwrap();
        self.builder.build_store(value_slot, value.as_basic_value_enum());

        self.builder.build_load(instance_ptr, "opt_instance").into_struct_value()
    }

    fn make_none_option_instance(&self, outer_type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> StructValue<'a> {
        let Type::GenericInstance(struct_id, generics) = self.project.get_type_by_id(outer_type_id) else { unreachable!() };
        debug_assert!(struct_id == &self.project.prelude_option_struct_id);
        let mut inner_type_id = &generics[0];
        if let Type::Generic(_, generic_name) = self.project.get_type_by_id(inner_type_id) {
            inner_type_id = resolved_generics.resolve(generic_name).unwrap_or(inner_type_id);
        }

        let opt_struct = self.project.get_struct_by_id(&self.project.prelude_option_struct_id);
        let resolved_generics = ResolvedGenerics::from_pairs(vec![(self.get_generic_name(&opt_struct.generic_ids[0]), *inner_type_id)]);
        let Some(llvm_type) = self.llvm_underlying_type_by_id(outer_type_id, &resolved_generics) else { todo!() };

        llvm_type.into_struct_type().const_zero()
    }

    fn make_trait_instance<V: BasicValue<'a>>(&mut self, trait_type_id: &TypeId, value_type_id: &TypeId, value: V, resolved_generics: &ResolvedGenerics) -> StructValue<'a> {
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
                    self.get_or_compile_option_method(resolved_generics, &inner_type_id, &0)
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
        resolved_generics: &ResolvedGenerics,
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
                    self.visit_expression(count_arg, resolved_generics).expect("Pointer.malloc's count parameter is not of type Unit")
                } else {
                    self.const_i64(1).as_basic_value_enum()
                };
                let malloc_amount_val = self.builder.build_int_mul(count_arg_value.into_int_value(), ptr_size, "malloc_amt");

                let ptr = self.malloc(malloc_amount_val, self.ptr(llvm_type));
                ptr.as_basic_value_enum()
            }
            "pointer_address" => { // Instance method
                let instance_node = implicit_argument.expect("pointer_address is an instance method and will have an implicit argument");
                let instance_value = self.visit_expression(instance_node, resolved_generics).expect("Instance is not of type Unit").into_pointer_value();
                let pointer_address = self.builder.build_ptr_to_int(instance_value, self.i64(), "address");

                pointer_address.as_basic_value_enum()
            }
            "pointer_store" => { // Instance method
                let instance_node = implicit_argument.expect("pointer_store is an instance method and will have an implicit argument");
                let value_arg = arguments.first().expect("Pointer#store has arity 2").as_ref().expect("Pointer#store has 1 required non-implicit argument");

                let ptr = self.visit_expression(instance_node, resolved_generics).expect("Instance is not of type Unit").into_pointer_value();
                let value = self.visit_expression(value_arg, resolved_generics).expect("Instance is not of type Unit");
                self.builder.build_store(ptr, value);

                return None;
            }
            "pointer_load" => { // Instance method
                let instance_node = implicit_argument.expect("pointer_load is an instance method and will have an implicit argument");

                let ptr = self.visit_expression(instance_node, resolved_generics).expect("Instance is not of type Unit").into_pointer_value();
                self.builder.build_load(ptr, "").as_basic_value_enum()
            }
            "pointer_offset" => { // Instance method
                let instance_node = implicit_argument.expect("pointer_offset is an instance method and will have an implicit argument");
                let offset_arg = arguments.first().expect("Pointer#offset has arity 2").as_ref().expect("Pointer#offset has 1 required non-implicit argument");

                let ptr = self.visit_expression(instance_node, resolved_generics).expect("Instance is not of type Unit").into_pointer_value();
                let value = self.visit_expression(offset_arg, resolved_generics).expect("Instance is not of type Unit").into_int_value();

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

                let ptr = self.visit_expression(instance_node, resolved_generics).expect("Instance is not of type Unit").into_pointer_value();
                let other_val = self.visit_expression(other_arg, resolved_generics).expect("Instance is not of type Unit").into_pointer_value();
                let size_val = self.visit_expression(size_arg, resolved_generics).expect("Instance is not of type Unit").into_int_value();
                let size_val = self.builder.build_int_mul(size_val, ptr_size, "");

                self.memcpy(ptr, other_val, size_val);

                return None;
            }
            "stdout_write" => { // Freestanding function
                let arg_node = arguments.first().expect("stdoutWrite has arity 1").as_ref().expect("stdoutWrite has 1 required argument");
                let arg_value = self.visit_expression(arg_node, resolved_generics).unwrap().into_pointer_value();
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
                let arg_value = self.visit_expression(arg_node, resolved_generics).unwrap().into_int_value();
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

    fn get_or_make_trait_struct_type(&self, trait_type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> (/* trait_type: */ StructType<'a>, /* vtable_type: */ StructType<'a>) {
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

    fn get_or_compile_function(&mut self, func_id: &FuncId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
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

    fn get_or_compile_option_method(&mut self, resolved_generics: &ResolvedGenerics, inner_type_id: &TypeId, member_idx: &usize) -> FunctionValue<'a> {
        // The toString method is always member_idx = 0
        if *member_idx != 0 {
            unreachable!();
        }

        let new_resolved_generics = resolved_generics.new_via_instance(&self.project.get_type_by_id(inner_type_id), &self.project);

        let type_name = format!("Option<{}>", self.llvm_type_name_by_id(inner_type_id, &new_resolved_generics));
        let fn_sig = format!("{}#toString({}):String", &type_name, &type_name);
        let fn_type = self.fn_type(self.ptr(self.string_type), &[self.main_module.get_struct_type(&type_name).unwrap().into()]);
        let llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, None);
        self.ctx_stack.push(CompilerContext { variables: HashMap::new() });

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
        let inner_tostring_llvm_fn = self.get_or_compile_function(&inner_tostring_func_id, &new_resolved_generics);
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

    fn compile_function(&mut self, func_id: &FuncId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let function = self.project.get_func_by_id(func_id);
        let has_return_value = function.return_type_id != PRELUDE_UNIT_TYPE_ID;

        let fn_sig = self.llvm_function_signature(func_id, resolved_generics);
        let fn_type = self.llvm_function_type(func_id, &resolved_generics);
        let llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, Some(*func_id));
        self.ctx_stack.push(CompilerContext { variables: HashMap::new() });

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let mut params_iter = llvm_fn.get_param_iter();
        if !function.captured_vars.is_empty() {
            params_iter.next().unwrap().set_name("captures");
        }

        let mut default_value_param_idx = 0;
        for (idx, param) in function.params.iter().enumerate() {
            params_iter.next().unwrap().set_name(&param.name);
            let llvm_param = llvm_fn.get_nth_param(idx as u32).unwrap();
            let variable = if let Some(default_value_node) = &param.default_value {
                let param_local = self.builder.build_alloca(llvm_param.get_type(), &param.name);

                let default_value_block = self.context.append_basic_block(llvm_fn, &format!("param_{}_default_value", &param.name));
                self.builder.build_unconditional_branch(default_value_block);
                self.builder.position_at_end(default_value_block);

                let flags_value = llvm_fn.get_last_param().unwrap();
                debug_assert!(flags_value.get_type() == self.i16().as_basic_type_enum(), "If the function has default-valued params, then the last parameter will be a 16-bit flags value");
                let flags_value = flags_value.into_int_value();
                let flag = self.builder.build_right_shift(flags_value, self.const_i16(default_value_param_idx), false, "flag");
                let is_flag_set = self.builder.build_and(flag, self.const_i16(1), "is_flag_set");
                let cond = self.builder.build_int_compare(IntPredicate::EQ, is_flag_set, self.const_i16(1), "cond");

                let then_bb = self.context.append_basic_block(llvm_fn, &format!("param_{}_default_value_then", &param.name));
                let else_bb = self.context.append_basic_block(llvm_fn, &format!("param_{}_default_value_else", &param.name));
                let cont_bb = self.context.append_basic_block(llvm_fn, &format!("param_{}_default_value_cont", &param.name));
                self.builder.build_conditional_branch(cond, then_bb, else_bb);

                self.builder.position_at_end(then_bb);
                let then_value = self.visit_expression(default_value_node, resolved_generics).unwrap();
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(else_bb);
                let else_value = llvm_param;
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(cont_bb);
                let phi = self.builder.build_phi(llvm_param.get_type(), "");
                phi.add_incoming(&[(&then_value, then_bb), (&else_value, else_bb)]);
                self.builder.build_store(param_local, phi.as_basic_value());

                default_value_param_idx += 1;
                LLVMVar::Slot(param_local)
            } else {
                LLVMVar::Param(llvm_param)
            };
            self.ctx_stack.last_mut().unwrap().variables.insert(param.var_id, variable);
        }

        let num_nodes = function.body.len();
        for (idx, node) in function.body.iter().enumerate() {
            let res = self.visit_statement(node, resolved_generics);
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

    fn compile_struct_type_by_type(&self, ty: &Type, resolved_generics: &ResolvedGenerics) -> StructType<'a> {
        let Type::GenericInstance(struct_id, generics) = ty else { todo!() };
        if struct_id == &self.project.prelude_option_struct_id {
            return self.compile_option_struct_type_by_type(ty, resolved_generics);
        }

        let struct_ = self.project.get_struct_by_id(struct_id);
        let type_name = self.llvm_type_name_by_type(ty, resolved_generics);
        let llvm_type = self.context.opaque_struct_type(&type_name);

        let resolved_generics = resolved_generics.new_via_struct(struct_, generics, &self.project);

        let struct_ = self.project.get_struct_by_id(struct_id);
        let field_types = struct_.fields.iter()
            .map(|field| {
                let Some(field_llvm_type) = self.llvm_underlying_type_by_id(&field.type_id, &resolved_generics) else { todo!() };
                self.llvm_ptr_wrap_type_if_needed(field_llvm_type).into()
            });

        llvm_type.set_body(field_types.collect_vec().as_slice(), false);

        llvm_type
    }

    // fn compile_option_struct_type_by_type_id(&self, type_id: &TypeId, resolved_generics: &HashMap<String, TypeId>) -> StructType<'a> {
    //     let ty = self.project.get_type_by_id(type_id);
    //     self.compile_option_struct_type_by_type(ty, resolved_generics)
    // }

    fn compile_option_struct_type_by_type(&self, ty: &Type, resolved_generics: &ResolvedGenerics) -> StructType<'a> {
        let Type::GenericInstance(struct_id, generics) = ty else { todo!() };
        debug_assert!(struct_id == &self.project.prelude_option_struct_id);

        let type_name = self.llvm_type_name_by_type(ty, resolved_generics);

        let Some(inner_llvm_type) = self.llvm_underlying_type_by_id(&generics[0], resolved_generics) else { todo!() };
        let inner_llvm_type = self.llvm_ptr_wrap_type_if_needed(inner_llvm_type);

        let llvm_type = self.context.opaque_struct_type(&type_name);
        llvm_type.set_body(&[self.bool().into(), inner_llvm_type.into()], false);

        llvm_type
    }

    fn get_or_compile_type_initializer(&mut self, struct_id: &StructId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let init_fn_sig = self.llvm_initializer_signature(struct_id, &resolved_generics);
        if let Some(function_val) = self.main_module.get_function(&init_fn_sig) {
            function_val
        } else {
            let function_val = self.compile_type_initializer(&struct_id, &resolved_generics);
            debug_assert!(function_val.get_name().to_str().unwrap() == &init_fn_sig);
            function_val
        }
    }

    fn compile_type_initializer(&mut self, struct_id: &StructId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let struct_ = self.project.get_struct_by_id(struct_id);
        let initializer_sig = self.llvm_initializer_signature(struct_id, resolved_generics);

        let fn_type = self.llvm_initializer_type(struct_id, resolved_generics);
        let llvm_fn = self.main_module.add_function(&initializer_sig, fn_type, None);
        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, None);

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let Some(llvm_struct_type) = self.llvm_underlying_type_by_id(&struct_.self_type_id, resolved_generics) else { todo!() };
        let mem = self.malloc(self.sizeof_struct(llvm_struct_type), self.ptr(llvm_struct_type));

        let mut params_iter = llvm_fn.get_param_iter();
        let mut default_value_param_idx = 0;
        for (idx, field) in struct_.fields.iter().enumerate() {
            params_iter.next().unwrap().set_name(&field.name);
            let llvm_param = llvm_fn.get_nth_param(idx as u32).unwrap();
            let field_value = if let Some(default_value_node) = &field.default_value {
                let default_value_block = self.context.append_basic_block(llvm_fn, &format!("field_{}_default_value", &field.name));
                self.builder.build_unconditional_branch(default_value_block);
                self.builder.position_at_end(default_value_block);

                let flags_value = llvm_fn.get_last_param().unwrap();
                debug_assert!(flags_value.get_type() == self.i16().as_basic_type_enum(), "If the type has default-valued fields, then the last parameter in its init function will be a 16-bit flags value");
                let flags_value = flags_value.into_int_value();
                let flag = self.builder.build_right_shift(flags_value, self.const_i16(default_value_param_idx), false, "flag");
                let is_flag_set = self.builder.build_and(flag, self.const_i16(1), "is_flag_set");
                let cond = self.builder.build_int_compare(IntPredicate::EQ, is_flag_set, self.const_i16(1), "cond");

                let then_bb = self.context.append_basic_block(llvm_fn, &format!("field_{}_default_value_then", &field.name));
                let else_bb = self.context.append_basic_block(llvm_fn, &format!("field_{}_default_value_else", &field.name));
                let cont_bb = self.context.append_basic_block(llvm_fn, &format!("field_{}_default_value_cont", &field.name));
                self.builder.build_conditional_branch(cond, then_bb, else_bb);

                self.builder.position_at_end(then_bb);
                let field_resolved_generics = resolved_generics.new_via_instance(self.project.get_type_by_id(&field.type_id), &self.project);
                self.ctx_stack.push(CompilerContext { variables: HashMap::new() });
                let then_value = self.visit_expression(default_value_node, &field_resolved_generics).unwrap();
                self.ctx_stack.pop();
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(else_bb);
                let else_value = llvm_param;
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(cont_bb);
                let phi = self.builder.build_phi(llvm_param.get_type(), "");
                phi.add_incoming(&[(&then_value, then_bb), (&else_value, else_bb)]);

                default_value_param_idx += 1;

                phi.as_basic_value()
            } else {
                llvm_param
            };
            let field_ptr = self.builder.build_struct_gep(mem, idx as u32, &format!("{}_slot", &field.name)).unwrap();
            self.builder.build_store(field_ptr, field_value);
        }

        self.builder.build_return(Some(&mem.as_basic_value_enum()));

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn compile_to_string_function(&mut self, func_id: &FuncId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
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
        self.current_fn = (llvm_fn, Some(*func_id));

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
            let resolved_generics = resolved_generics.new_via_instance(field_ty, &self.project);// if let Type::GenericInstance(struct_id, generics) = &field_ty {
            let tostring_fn_val = self.get_or_compile_function(tostring_func_id, &resolved_generics);
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

        let ret_val = self.construct_string(len_val, str_val);

        self.builder.build_return(Some(&ret_val));

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn compile_int_to_string_method(&mut self, func_id: &FuncId) -> FunctionValue<'a> {
        let llvm_fn_sig = self.llvm_function_signature(func_id, &ResolvedGenerics::default());
        let llvm_fn_type = self.llvm_function_type(func_id, &ResolvedGenerics::default());
        let llvm_fn = self.main_module.add_function(&llvm_fn_sig, llvm_fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, Some(*func_id));

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

        let ret_val = self.construct_string(len_val, str_val);

        self.builder.build_return(Some(&ret_val));

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn compile_float_to_string_method(&mut self, func_id: &FuncId) -> FunctionValue<'a> {
        let llvm_fn_sig = self.llvm_function_signature(func_id, &ResolvedGenerics::default());
        let llvm_fn_type = self.llvm_function_type(func_id, &ResolvedGenerics::default());
        let llvm_fn = self.main_module.add_function(&llvm_fn_sig, llvm_fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, Some(*func_id));

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

        let ret_val = self.construct_string(len_val, str_val);

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

        let llvm_fn_sig = self.llvm_function_signature(func_id, &ResolvedGenerics::default());
        let llvm_fn_type = self.llvm_function_type(func_id, &ResolvedGenerics::default());
        let llvm_fn = self.main_module.add_function(&llvm_fn_sig, llvm_fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, Some(*func_id));

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
        let llvm_fn_sig = self.llvm_function_signature(func_id, &ResolvedGenerics::default());
        let llvm_fn_type = self.llvm_function_type(func_id, &ResolvedGenerics::default());
        let llvm_fn = self.main_module.add_function(&llvm_fn_sig, llvm_fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, Some(*func_id));

        llvm_fn.get_param_iter().next().unwrap().set_name("self");
        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);
        self.builder.build_return(Some(&llvm_fn.get_nth_param(0).unwrap()));

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn make_function_value(&mut self, func_id: &FuncId, target_type_id: &TypeId, captures_arr: Option<PointerValue<'a>>, resolved_generics: &ResolvedGenerics) -> BasicValueEnum<'a> {
        let Type::Function(target_param_type_ids, target_num_required_params, _, target_return_type_id) = self.project.get_type_by_id(target_type_id) else { unreachable!() };
        let target_arity = *target_num_required_params;
        debug_assert!(target_param_type_ids.len() == target_arity);

        let function = self.project.get_func_by_id(func_id);

        let llvm_fn = self.get_or_compile_function(func_id, resolved_generics);

        let num_params = function.params.len();
        let num_required_params = function.params.iter().filter(|p| p.default_value.is_none()).count();
        let num_optional_params = function.params.iter().filter(|p| p.default_value.is_some()).count();
        debug_assert!(num_required_params + num_optional_params == num_params);
        // For the cases below, consider the function
        //   func callFn(fn: (Int) => Int) = ...
        let llvm_fn = if num_optional_params == 0 {
            if target_arity == num_params {
                // If the referenced function's arity matches the required arity, and it has no optional parameters,
                // then we don't need to create a wrapper for it.
                llvm_fn
            } else if target_arity < num_params {
                // In this case, consider the following example:
                //   func foo(a: Int, b: Int): Int = ...
                //   callFn(foo)
                // In this case, typechecking fails since `callFn` won't provide a value for the parameter `b`.
                unreachable!("This should be caught during typechecking")
            } else {
                // In this case, consider the following example:
                //   func foo(): Int = ...
                //   callFn(foo)
                // Create a wrapper of higher arity which discards parameters.
                let num_throwaway_params = target_arity - num_params;
                let wrapper_fn_name = format!("{}$wrapper(discard={})", &function.name, num_throwaway_params);

                let wrapper_params = target_param_type_ids.iter().map(|param_type_id| (*param_type_id, false)).collect();
                let fn_sig = self.llvm_function_signature_by_parts(&wrapper_fn_name, None, &vec![], &wrapper_params, target_return_type_id, resolved_generics);
                let fn_type = self.llvm_function_type_by_parts(target_param_type_ids, target_arity, false, target_return_type_id, &resolved_generics);
                let wrapper_llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);

                let prev_bb = self.builder.get_insert_block().unwrap();
                let prev_fn = self.current_fn;
                self.current_fn = (wrapper_llvm_fn, None);
                self.ctx_stack.push(CompilerContext { variables: HashMap::new() });

                let block = self.context.append_basic_block(wrapper_llvm_fn, "");
                self.builder.position_at_end(block);

                let args = (0..num_params).map(|i| wrapper_llvm_fn.get_nth_param(i as u32).unwrap().into()).collect_vec();
                let wrapped_fn_result = self.builder.build_call(llvm_fn, args.as_slice(), "").try_as_basic_value().left();
                if let Some(result) = wrapped_fn_result {
                    self.builder.build_return(Some(&result));
                } else {
                    self.builder.build_return(None);
                }

                self.ctx_stack.pop();
                self.current_fn = prev_fn;
                self.builder.position_at_end(prev_bb);

                wrapper_llvm_fn
            }
        } else if target_arity < num_required_params {
            // In this case, consider the following example:
            //   func foo(a: Int, b: Int, c = 12): Int = ...
            //   callFn(foo)
            // In this case, typechecking fails since `callFn` won't provide a value for the parameter `b`.
            // This is similar to the case above, except here we have an optional parameter.
            unreachable!("This should be caught during typechecking")
        } else if num_required_params <= target_arity && target_arity <= num_params {
            // In this case, we need to "artificially (monotonically) shrink" the arity of the underlying function.
            // It's "monotonically" because the arity itself might not actually shrink; consider this example:
            //   func foo(x = 12): Int = ...
            //   callFn(foo)
            // In this case, the optional parameter `x` must be treated as if it's a required parameter, and so the
            // arity of the wrapper function becomes 1.
            // Nominally though, the arity must be artificially shrunk in these cases:
            //   func foo1(x: Int, y = 12): Int = ...
            //   callFn(foo1)
            //   func foo2(x = 12, y = 16): Int = ...
            //   callFn(foo2)
            // In the case of `foo1`, the wrapper function has arity 1 and the parameter `y` will receive its default
            // value. In the case of `foo2`, the wrapper function still has arity 1 and the parameter `y` will still
            // receive its default value, but `x` will _not_.
            let num_orig_optional_params = function.params.len() - num_required_params;
            debug_assert!((target_arity - num_required_params) <= num_orig_optional_params, "Prevent against underflow");
            let num_optional_params_being_given_default_value = num_orig_optional_params - (target_arity - num_required_params);
            let wrapper_fn_name = format!("{}$wrapper(optsRvcDef={})", &function.name, num_optional_params_being_given_default_value);

            let wrapper_params = target_param_type_ids.iter().map(|param_type_id| (*param_type_id, false)).collect();
            let fn_sig = self.llvm_function_signature_by_parts(&wrapper_fn_name, None, &vec![], &wrapper_params, target_return_type_id, resolved_generics);
            let fn_type = self.llvm_function_type_by_parts(target_param_type_ids, target_arity, false, target_return_type_id, &resolved_generics);
            let wrapper_llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);

            let prev_bb = self.builder.get_insert_block().unwrap();
            let prev_fn = self.current_fn;
            self.current_fn = (wrapper_llvm_fn, None);
            self.ctx_stack.push(CompilerContext { variables: HashMap::new() });

            let block = self.context.append_basic_block(wrapper_llvm_fn, "");
            self.builder.position_at_end(block);

            let mut args = (0..target_arity).map(|i| wrapper_llvm_fn.get_nth_param(i as u32).unwrap()).collect_vec();
            args.extend(function.params.iter().skip(target_arity).map(|p| {
                let Some(llvm_type) = self.llvm_underlying_type_by_id(&p.type_id, &resolved_generics) else { todo!() };
                let llvm_type = self.llvm_ptr_wrap_type_if_needed(llvm_type);
                llvm_type.const_zero().as_basic_value_enum()
            }));
            let mut flag = 0i16;
            let mut shift_val = num_orig_optional_params - num_optional_params_being_given_default_value;
            for _ in 0..num_optional_params_being_given_default_value {
                flag |= 1 << shift_val;
                shift_val += 1;
            }
            args.push(self.const_i16(flag).as_basic_value_enum());

            let args = args.into_iter().map(|a| a.into()).collect_vec();
            let wrapped_fn_result = self.builder.build_call(llvm_fn, args.as_slice(), "").try_as_basic_value().left();
            if let Some(result) = wrapped_fn_result {
                self.builder.build_return(Some(&result));
            } else {
                self.builder.build_return(None);
            }

            self.ctx_stack.pop();
            self.current_fn = prev_fn;
            self.builder.position_at_end(prev_bb);

            wrapper_llvm_fn
        } else if target_arity > num_params {
            // In this case, consider the following example:
            //   func callFn2(fn: (Int, Int, Int) => Int) = ...
            //   func foo(x: Int, y = 12): Int = ...
            //   callFn2(foo)
            // Create a wrapper function of higher arity which discards parameters and _also_ passes 0 to the optional params flag; we know we can do this
            // because in order to reach this case, it must be the case that all of the optional parameters are passed a value.
            let num_throwaway_params = target_arity - num_params;
            let wrapper_fn_name = format!("{}$wrapper(discard={})", &function.name, num_throwaway_params);

            let wrapper_params = target_param_type_ids.iter().map(|param_type_id| (*param_type_id, false)).collect();
            let fn_sig = self.llvm_function_signature_by_parts(&wrapper_fn_name, None, &vec![], &wrapper_params, target_return_type_id, resolved_generics);
            let fn_type = self.llvm_function_type_by_parts(target_param_type_ids, target_arity, false, target_return_type_id, &resolved_generics);
            let wrapper_llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);

            let prev_bb = self.builder.get_insert_block().unwrap();
            let prev_fn = self.current_fn;
            self.current_fn = (wrapper_llvm_fn, None);
            self.ctx_stack.push(CompilerContext { variables: HashMap::new() });

            let block = self.context.append_basic_block(wrapper_llvm_fn, "");
            self.builder.position_at_end(block);

            let mut args = (0..num_params).map(|i| wrapper_llvm_fn.get_nth_param(i as u32).unwrap()).collect_vec();
            args.push(self.const_i16(0).as_basic_value_enum());
            let args = args.into_iter().map(|a| a.into()).collect_vec();
            let wrapped_fn_result = self.builder.build_call(llvm_fn, args.as_slice(), "").try_as_basic_value().left();
            if let Some(result) = wrapped_fn_result {
                self.builder.build_return(Some(&result));
            } else {
                self.builder.build_return(None);
            }

            self.ctx_stack.pop();
            self.current_fn = prev_fn;
            self.builder.position_at_end(prev_bb);

            wrapper_llvm_fn
        } else {
            unreachable!("All prior cases should have been exhausted above")
        };

        let (fn_value_type, fn_value_fn_ptr_type) = self.make_function_value_type_by_type_id(target_type_id, resolved_generics);

        let mem = self.malloc(self.sizeof_struct(fn_value_type), fn_value_type.ptr_type(AddressSpace::Generic));

        let captures_slot = self.builder.build_struct_gep(mem, 0, "captures_slot").unwrap();
        let captures_val = captures_arr.unwrap_or(self.ptr(self.i64()).const_null());
        self.builder.build_store(captures_slot, captures_val);

        let fn_ptr_slot = self.builder.build_struct_gep(mem, 1, "fn_ptr_slot").unwrap();
        let llvm_fn_ptr = llvm_fn.as_global_value().as_pointer_value();
        let llvm_fn_ptr = self.builder.build_pointer_cast(llvm_fn_ptr, fn_value_fn_ptr_type, "");
        self.builder.build_store(fn_ptr_slot, llvm_fn_ptr);

        mem.as_basic_value_enum()
    }

    fn make_function_value_type_by_type_id(&self, func_type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> (StructType<'a>, PointerType<'a>) {
        self.make_function_value_type_by_type(self.project.get_type_by_id(func_type_id), resolved_generics)
    }

    fn make_function_value_type_by_type(&self, func_ty: &Type, resolved_generics: &ResolvedGenerics) -> (StructType<'a>, PointerType<'a>) {
        let Type::Function(param_type_ids, num_required_params, is_variadic, return_type_id) = func_ty else { unreachable!() };

        let fn_value_type_name = self.llvm_type_name_by_type(func_ty, resolved_generics);

        let llvm_fn_type = self.llvm_function_type_by_parts(param_type_ids, *num_required_params, *is_variadic, return_type_id, resolved_generics);
        let fn_ptr_type = llvm_fn_type.ptr_type(AddressSpace::Generic);

        if let Some(llvm_type) = self.main_module.get_struct_type(&fn_value_type_name) {
            (llvm_type, fn_ptr_type)
        } else {
            let fn_val_type = self.context.opaque_struct_type(&fn_value_type_name);
            fn_val_type.set_body(&[
                // self.i32().into(), // param_trait_flag
                // self.i32().array_type(arity as u32).into(), // param_type_ids
                self.context.i64_type().ptr_type(AddressSpace::Generic).into(), // captures
                fn_ptr_type.into(), // fn_ptr
            ], false);

            (fn_val_type, fn_ptr_type)
        }
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
    fn test_functions() {
        run_test_file("functions.abra");
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
