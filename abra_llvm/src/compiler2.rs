use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt::Debug;
use std::path::PathBuf;
use std::process::{Command, ExitStatus};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::TargetMachine;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FloatType, FunctionType, IntType, PointerType, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, CallableValue, FloatValue, FunctionValue, InstructionOpcode, IntValue, PointerValue, StructValue};
use itertools::Itertools;
use abra_core::lexer::tokens::{POSITION_BOGUS, Token};
use abra_core::parser::ast::{BinaryOp, BindingPattern, IndexingMode, UnaryOp};
use abra_core::typechecker::typechecker2::{AccessorKind, AssignmentKind, EnumId, EnumVariantKind, FuncId, Function, FunctionKind, METHOD_IDX_EQ, METHOD_IDX_HASH, METHOD_IDX_TOSTRING, PRELUDE_ANY_TYPE_ID, PRELUDE_BOOL_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_INT_TYPE_ID, PRELUDE_MODULE_ID, PRELUDE_STRING_TYPE_ID, PRELUDE_UNIT_TYPE_ID, PrimitiveType, Project, ScopeId, Struct, StructId, Type, TypedLiteral, TypedMatchCaseArgument, TypedMatchCaseKind, TypedNode, TypeId, TypeKind, VariableAlias, VarId};

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

#[derive(Clone)]
struct ResolvedGeneric {
    type_id: TypeId,
    llvm_type_name: String,
}

#[derive(Clone, Default)]
struct ResolvedGenerics(Vec<HashMap<TypeId, ResolvedGeneric>>);

impl ResolvedGenerics {
    pub fn _dump<'ctx>(&self, compiler: &LLVMCompiler2<'ctx>) {
        println!("resolved_generics:");
        for layer in &self.0 {
            println!("--------");
            for (key, resolved) in layer {
                println!("  {} ({:?}): {} ({})", compiler.project.type_repr(key), key, compiler.project.type_repr(&resolved.type_id), &resolved.llvm_type_name)
            }
        }
    }

    pub fn resolve(&self, type_id: &TypeId) -> Option<&ResolvedGeneric> {
        self.0.iter().rev().find_map(|m| m.get(type_id))
    }

    pub fn resolve_if_generic(&self, type_id: &TypeId, project: &Project) -> Option<&ResolvedGeneric> {
        if let Type::Generic(_, _) = project.get_type_by_id(type_id) {
            self.resolve(type_id)
        } else {
            None
        }
    }

    fn extend(&self, next_layer: HashMap<TypeId, ResolvedGeneric>) -> ResolvedGenerics {
        let mut new = self.clone();
        if !next_layer.is_empty() {
            new.0.push(next_layer);
        }
        new
    }

    pub fn extend_via_struct(&self, struct_: &Struct, realized_generics: &Vec<ResolvedGeneric>) -> ResolvedGenerics {
        let mut map = HashMap::new();
        for (generic_id, resolved_generic) in struct_.generic_ids.iter().zip(realized_generics) {
            map.insert(*generic_id, resolved_generic.clone());
        }
        self.extend(map)
    }

    pub fn extend_via_func_call(&self, invokee: &Function, realized_generics: &Vec<ResolvedGeneric>) -> ResolvedGenerics {
        let mut map = HashMap::new();
        self.clone().0;
        for (generic_type_id, resolved) in invokee.generic_ids.iter().zip(realized_generics) {
            map.insert(*generic_type_id, resolved.clone());
        }
        self.extend(map)
    }

    pub fn extend_via_pairs(&self, pairs: Vec<(TypeId, ResolvedGeneric)>) -> ResolvedGenerics {
        let mut map = HashMap::new();
        for (generic_id, resolved) in pairs {
            map.insert(generic_id, resolved);
        }
        self.extend(map)
    }
}

const RUNTIME_TYPEID_INT: usize = 0;
const RUNTIME_TYPEID_FLOAT: usize = 1;
const RUNTIME_TYPEID_BOOL: usize = 2;
const RUNTIME_TYPEID_STRING: usize = 3;

const ENUM_TYPENAME_TAG: &str = "enum#";

pub struct LLVMCompiler2<'a> {
    project: &'a Project,
    context: &'a Context,
    builder: Builder<'a>,
    main_module: Module<'a>,
    current_fn: (FunctionValue<'a>, Option<FuncId>),
    ctx_stack: Vec<CompilerContext<'a>>,
    loop_stack: Vec<(/* loop_start: */ BasicBlock<'a>, /* loop_end: */ BasicBlock<'a>)>,
    closure_captures: HashMap<FuncId, PointerValue<'a>>,
    typeids: RefCell<HashMap<String, usize>>,
    adhoc_types: RefCell<Vec<Type>>,

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
            .arg("-lm")
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

        let mut typeids = HashMap::new();
        typeids.insert("Int".to_string(), RUNTIME_TYPEID_INT);
        typeids.insert("Float".to_string(), RUNTIME_TYPEID_FLOAT);
        typeids.insert("Bool".to_string(), RUNTIME_TYPEID_BOOL);
        typeids.insert("String".to_string(), RUNTIME_TYPEID_STRING);

        let string_type = context.opaque_struct_type("String");
        string_type.set_body(&[
            context.i32_type().into(), // typeid
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
            loop_stack: vec![],
            closure_captures: HashMap::new(),
            typeids: RefCell::new(typeids),
            adhoc_types: RefCell::new(vec![]),

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

    fn new_resolved_generics_via_instance(&self, ty: &Type) -> ResolvedGenerics {
        if let Type::GenericInstance(struct_id, generics) = ty {
            let struct_ = self.project.get_struct_by_id(struct_id);
            let map = struct_.generic_ids.iter().zip(generics)
                .map(|(generic_id, type_arg_id)| {
                    (*generic_id, self.make_resolved_generic(type_arg_id, &ResolvedGenerics::default()))
                })
                .collect();
            ResolvedGenerics(vec![map])
        } else {
            ResolvedGenerics::default()
        }
    }

    fn extend_resolved_generics_via_instance(&self, resolved_generics: &ResolvedGenerics, type_id: &TypeId) -> ResolvedGenerics {
        let mut map = HashMap::new();
        if let Type::GenericInstance(struct_id, generics) = self.get_type_by_id(type_id) {
            // For tuples, we shouldn't ever be looking up generics by id since tuples don't have any
            // instance methods aside from those programmatically generated (see `get_or_compile_tuple_method`),
            // and in that generation logic, we use the given realized generics of the tuple items themselves
            // rather than deriving them from resolved_generics.
            if struct_id != self.project.prelude_tuple_struct_id {
                let struct_ = self.project.get_struct_by_id(&struct_id);
                for (generic_type_id, type_id) in struct_.generic_ids.iter().zip(generics) {
                    let resolved = resolved_generics.resolve(&type_id)
                        .map(|resolved| resolved.clone())
                        .unwrap_or_else(|| self.make_resolved_generic(&type_id, resolved_generics));
                    map.insert(*generic_type_id, resolved);
                }
            }
        }
        resolved_generics.extend(map)
    }

    fn get_or_add_adhoc_type(&self, ty: Type) -> TypeId {
        if let Some((idx, _)) = self.adhoc_types.borrow().iter().find_position(|t| *t == &ty) {
            return TypeId(ScopeId::BOGUS, idx);
        }
        let idx = self.adhoc_types.borrow().len();
        self.adhoc_types.borrow_mut().push(ty);
        TypeId(ScopeId::BOGUS, idx)
    }

    fn get_type_by_id(&self, type_id: &TypeId) -> Type {
        if type_id.0 == ScopeId::BOGUS {
            self.adhoc_types.borrow().get(type_id.1).unwrap().clone()
        } else {
            self.project.get_type_by_id(type_id).clone()
        }
    }

    fn type_is_option(&self, type_id: &TypeId) -> Option<TypeId> {
        match self.get_type_by_id(&type_id) {
            Type::GenericInstance(struct_id, generic_ids) if struct_id == self.project.prelude_option_struct_id => Some(generic_ids[0]),
            _ => None
        }
    }

    fn type_is_tuple(&self, type_id: &TypeId) -> Option<Vec<TypeId>> {
        match self.get_type_by_id(&type_id) {
            Type::GenericInstance(struct_id, generic_ids) if struct_id == self.project.prelude_tuple_struct_id => Some(generic_ids),
            _ => None
        }
    }

    fn make_resolved_generic(&self, type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> ResolvedGeneric {
        let llvm_type_name = self.llvm_type_name_by_id(type_id, resolved_generics);

        let type_id = match self.get_type_by_id(type_id) {
            Type::Primitive(_) => *type_id,
            Type::Generic(_, name) => resolved_generics.resolve(type_id)
                .expect(&format!("Could not resolve generic {name} in current scope"))
                .type_id,
            Type::GenericInstance(struct_id, generics) => {
                let mut create_new = false;
                let mut resolved_generic_ids = Vec::with_capacity(generics.len());
                for generic_id in generics {
                    if let Type::Generic(_, name) = self.get_type_by_id(&generic_id) {
                        create_new = true;
                        let resolved_type_id = resolved_generics.resolve(&generic_id)
                            .expect(&format!("Could not resolve generic {name} in current scope"))
                            .type_id;
                        resolved_generic_ids.push(resolved_type_id)
                    } else {
                        resolved_generic_ids.push(generic_id);
                    }
                }
                if create_new {
                    self.get_or_add_adhoc_type(Type::GenericInstance(struct_id, resolved_generic_ids))
                } else {
                    *type_id
                }
            }
            Type::GenericEnumInstance(enum_id, generics, variant_idx) => {
                let mut create_new = false;
                let mut resolved_generic_ids = Vec::with_capacity(generics.len());
                for generic_id in generics {
                    if let Type::Generic(_, name) = self.get_type_by_id(&generic_id) {
                        create_new = true;
                        let resolved_type_id = resolved_generics.resolve(&generic_id)
                            .expect(&format!("Could not resolve generic {name} in current scope"))
                            .type_id;
                        resolved_generic_ids.push(resolved_type_id)
                    } else {
                        resolved_generic_ids.push(generic_id);
                    }
                }
                if create_new {
                    self.get_or_add_adhoc_type(Type::GenericEnumInstance(enum_id, resolved_generic_ids, variant_idx))
                } else {
                    *type_id
                }
            }
            Type::Function(_, _, _, _) |
            Type::Type(_) |
            Type::ModuleAlias => todo!(),
        };

        ResolvedGeneric { type_id, llvm_type_name }
    }

    fn llvm_type_name_by_id(&self, type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> String {
        let ty = self.get_type_by_id(type_id);
        match ty {
            Type::Primitive(PrimitiveType::Unit) => "Unit".into(),
            Type::Primitive(PrimitiveType::Any) => "Any".into(),
            Type::Primitive(PrimitiveType::Int) => "Int".into(),
            Type::Primitive(PrimitiveType::Float) => "Float".into(),
            Type::Primitive(PrimitiveType::Bool) => "Bool".into(),
            Type::Primitive(PrimitiveType::String) => "String".into(),
            Type::Generic(_, name) => {
                resolved_generics.resolve(type_id)
                    .map(|resolved| {
                        if let Type::Generic(_, other_name) = self.get_type_by_id(&resolved.type_id) {
                            if other_name == name { panic!("Self-referential generic type named {}; stackoverflow detected", other_name); }
                        }
                        resolved.llvm_type_name.clone()
                    })
                    .unwrap_or(name.clone())
            }
            Type::GenericInstance(struct_id, generic_ids) => {
                let struct_ = self.project.get_struct_by_id(&struct_id);
                let struct_name = &struct_.name;
                let generic_names = generic_ids.iter().map(|type_id| self.llvm_type_name_by_id(type_id, resolved_generics)).join(",");
                let generic_names = if generic_ids.is_empty() { "".into() } else { format!("<{}>", generic_names) };

                if struct_id == self.project.prelude_tuple_struct_id {
                    let tuple_size = generic_ids.len();
                    format!("{struct_name}{tuple_size}{generic_names}")
                } else {
                    format!("{struct_name}{generic_names}")
                }
            }
            Type::GenericEnumInstance(enum_id, generic_ids, _) => {
                let enum_ = self.project.get_enum_by_id(&enum_id);
                let enum_name = &enum_.name;
                let generic_names = generic_ids.iter().map(|type_id| self.llvm_type_name_by_id(type_id, resolved_generics)).join(",");
                let generic_names = if generic_ids.is_empty() { "".into() } else { format!("<{}>", generic_names) };
                format!("{ENUM_TYPENAME_TAG}{enum_name}{generic_names}")
            }
            Type::Function(parameter_type_ids, num_required_params, is_variadic, return_type_id) => {
                debug_assert!(!is_variadic, "Not yet implemented");

                let arity = num_required_params;

                let ret_type_name = self.llvm_type_name_by_id(&return_type_id, resolved_generics);
                let param_type_names = parameter_type_ids.iter().take(num_required_params).map(|type_id| self.llvm_type_name_by_id(type_id, resolved_generics));
                let mut function_type_args = vec![ret_type_name];
                function_type_args.extend(param_type_names);
                let function_type_args = function_type_args.join(",");

                format!("Function{arity}<{function_type_args}>")
            }
            Type::Type(kind) => match kind {
                TypeKind::Struct(struct_id) => {
                    let struct_ = self.project.get_struct_by_id(&struct_id);
                    let struct_name = &struct_.name;
                    let generic_names = struct_.generic_ids.iter()
                        .map(|type_id| {
                            let Type::Generic(_, generic_name) = self.get_type_by_id(type_id) else { unreachable!("TypeId {:?} represents a type that is not a generic", type_id) };
                            generic_name
                        })
                        .join(",");
                    let generic_names = if struct_.generic_ids.is_empty() { "".into() } else { format!("<{}>", generic_names) };
                    format!("{struct_name}{generic_names}")
                }
                TypeKind::Enum(enum_id) => {
                    let enum_ = self.project.get_enum_by_id(&enum_id);
                    let enum_name = &enum_.name;
                    let generic_names = enum_.generic_ids.iter()
                        .map(|type_id| {
                            let Type::Generic(_, generic_name) = self.get_type_by_id(type_id) else { unreachable!("TypeId {:?} represents a type that is not a generic", type_id) };
                            generic_name
                        })
                        .join(",");
                    let generic_names = if enum_.generic_ids.is_empty() { "".into() } else { format!("<{}>", generic_names) };
                    format!("{ENUM_TYPENAME_TAG}{enum_name}{generic_names}")
                }
            }
            Type::ModuleAlias => todo!()
        }
    }

    fn llvm_enum_variant_type_name(&self, enum_type_name: &String, enum_variant_name: &String) -> String {
        format!("{enum_type_name}.{enum_variant_name}")
    }

    fn llvm_underlying_type_by_id(&self, type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> Option<BasicTypeEnum<'a>> {
        let ty = self.get_type_by_id(type_id);
        let llvm_type = match ty {
            Type::Primitive(PrimitiveType::Unit) => return None,
            Type::Primitive(PrimitiveType::Any) => {
                self.get_or_make_trait_struct_type(&PRELUDE_ANY_TYPE_ID, resolved_generics).0.as_basic_type_enum()
            }
            Type::Primitive(PrimitiveType::Int) => self.i64().as_basic_type_enum(),
            Type::GenericInstance(struct_id, _) if struct_id == self.project.prelude_int_struct_id => self.i64().as_basic_type_enum(),
            Type::Primitive(PrimitiveType::Float) => self.f64().as_basic_type_enum(),
            Type::GenericInstance(struct_id, _) if struct_id == self.project.prelude_float_struct_id => self.f64().as_basic_type_enum(),
            Type::Primitive(PrimitiveType::Bool) => self.bool().as_basic_type_enum(),
            Type::GenericInstance(struct_id, _) if struct_id == self.project.prelude_bool_struct_id => self.bool().as_basic_type_enum(),
            Type::Primitive(PrimitiveType::String) => self.string_type.as_basic_type_enum(),
            Type::Generic(_, name) => {
                return resolved_generics.resolve(type_id)
                    .and_then(|resolved| {
                        if let Some(llvm_ty) = self.main_module.get_struct_type(&resolved.llvm_type_name) {
                            return Some(llvm_ty.into());
                        }

                        if let Type::Generic(_, other_name) = self.get_type_by_id(&resolved.type_id) {
                            if other_name == name {
                                if let Some(llvm_ty) = self.main_module.get_struct_type(&resolved.llvm_type_name) {
                                    return Some(llvm_ty.into());
                                }
                                panic!("Self-referential generic type {}; stackoverflow detected", &name);
                            }
                        }
                        self.llvm_underlying_type_by_id(&resolved.type_id, resolved_generics)
                    });
            }
            Type::GenericInstance(_, generics) => {
                let type_name = self.llvm_type_name_by_id(type_id, resolved_generics);

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
                    let struct_type = self.compile_struct_type_by_type_id(type_id, resolved_generics);
                    debug_assert!(struct_type.get_name().unwrap().to_str().unwrap() == &type_name, "Expected {} to be {}", struct_type.get_name().unwrap().to_str().unwrap(), &type_name);
                    struct_type.as_basic_type_enum()
                }
            }
            Type::GenericEnumInstance(enum_id, _, _) => {
                let enum_type_name = self.llvm_type_name_by_id(type_id, resolved_generics);
                let (enum_llvm_type, _) = self.get_or_compile_enum_type_by_type_id(&enum_id, &enum_type_name, resolved_generics);
                enum_llvm_type.as_basic_type_enum()
            }
            Type::Function(_, _, _, _) => self.make_function_value_type_by_type_id(type_id, resolved_generics).0.as_basic_type_enum(),
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

            if llvm_type_name == "Any" || llvm_type_name.starts_with("Option<") || llvm_type_name.starts_with(ENUM_TYPENAME_TAG) {
                llvm_type
            } else {
                self.ptr(llvm_type).as_basic_type_enum()
            }
        } else {
            llvm_type
        }
    }

    fn get_typeid_by_name(&self, llvm_type_name: &String) -> usize {
        if let Some(typeid) = self.typeids.borrow().get(llvm_type_name) {
            return *typeid;
        }

        self.register_typeid(llvm_type_name)
    }

    fn register_typeid(&self, llvm_type_name: &String) -> usize {
        let mut typeids = self.typeids.borrow_mut();

        if let Some(typeid) = typeids.get(llvm_type_name) {
            unreachable!("Duplicate typeid {} for name {}", typeid, llvm_type_name)
        } else {
            let typeid = typeids.len();
            typeids.insert(llvm_type_name.clone(), typeid);
            typeid
        }
    }

    fn get_typeid_from_value(&self, value: BasicValueEnum<'a>, local: Option<PointerValue<'a>>) -> (IntValue<'a>, /* local: */ Option<PointerValue<'a>>) {
        if value.is_int_value() {
            let value = if value.get_type().into_int_type().get_bit_width() == 1 {
                self.const_i32(RUNTIME_TYPEID_BOOL as u64)
            } else {
                self.const_i32(RUNTIME_TYPEID_INT as u64)
            };
            (value, None)
        } else if value.is_float_value() {
            let value = self.const_i32(RUNTIME_TYPEID_FLOAT as u64);
            (value, None)
        } else if value.is_pointer_value() {
            let typeid_slot = self.builder.build_struct_gep(value.into_pointer_value(), 0, "typeid_slot").unwrap();
            let value = self.builder.build_load(typeid_slot, "typeid").into_int_value();
            (value, None)
        } else if value.get_type().is_struct_type() && value.get_type().into_struct_type().get_name().unwrap().to_str().unwrap().starts_with(ENUM_TYPENAME_TAG) {
            let local = if let Some(local) = local {
                local
            } else {
                let local = self.builder.build_alloca(value.get_type(), "local");
                self.builder.build_store(local, value);
                local
            };
            let value_slot = self.builder.build_struct_gep(local, 0, "").unwrap();
            let value = self.builder.build_load(value_slot, "").into_int_value();
            let value = self.builder.build_right_shift(value, self.const_i64(48), false, "");
            (value, Some(local))
        } else {
            let local = if let Some(local) = local {
                local
            } else {
                let local = self.builder.build_alloca(value.get_type(), "local");
                self.builder.build_store(local, value);
                local
            };
            let typeid_slot = self.builder.build_struct_gep(local, 0, "typeid_slot").unwrap();
            let value = self.builder.build_load(typeid_slot, "typeid").into_int_value();
            (value, Some(local))
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

    fn llvm_initializer_signature(&self, struct_id: &StructId, resolved_generics: &ResolvedGenerics) -> (/* type_name: */ String, /* signature: */ String) {
        let struct_ = self.project.get_struct_by_id(struct_id);

        let llvm_type_name = self.llvm_type_name_by_id(&struct_.self_type_id, resolved_generics);
        let sig = format!("{}.init", &llvm_type_name);
        (llvm_type_name, sig)
    }

    fn llvm_function_type(&mut self, func_id: &FuncId, resolved_generics: &ResolvedGenerics) -> FunctionType<'a> {
        let function = self.project.get_func_by_id(func_id);

        let mut num_optional_params = 0;
        let params = function.params.iter()
            .map(|p| {
                if p.default_value.is_some() {
                    num_optional_params += 1;
                }

                let llvm_type = if p.is_variadic {
                    self.compile_array_type(&p.type_id, resolved_generics).into()
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
        if function.is_closure() {
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

    fn llvm_function_type_by_parts(&self, param_type_ids: &Vec<TypeId>, num_required_params: usize, is_closure: bool, is_variadic: bool, return_type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> FunctionType<'a> {
        debug_assert!(!is_variadic);

        let mut param_types = param_type_ids.iter()
            .take(num_required_params)
            .map(|param_type_id| {
                let Some(llvm_type) = self.llvm_underlying_type_by_id(param_type_id, resolved_generics) else { todo!() };
                self.llvm_ptr_wrap_type_if_needed(llvm_type).into()
            })
            .collect_vec();
        if is_closure {
            param_types.insert(0, self.closure_captures_t().into());
        }

        if return_type_id == &PRELUDE_UNIT_TYPE_ID {
            self.context.void_type().fn_type(param_types.as_slice(), false)
        } else {
            let Some(ret_llvm_type) = self.llvm_underlying_type_by_id(&return_type_id, resolved_generics) else { todo!() };
            let ret_llvm_type = self.llvm_ptr_wrap_type_if_needed(ret_llvm_type);

            self.fn_type(ret_llvm_type, param_types.as_slice())
        }
    }

    fn llvm_initializer_type(&mut self, struct_id: &StructId, resolved_generics: &ResolvedGenerics) -> FunctionType<'a> {
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

    fn _debug_print(&self, fmt_str: &str, value: BasicValueEnum<'a>) {
        let printf = self.main_module.get_function("printf").unwrap();
        let fmt_str_val = self.builder.build_global_string_ptr(fmt_str, "").as_basic_value_enum();
        self.builder.build_call(printf, &[fmt_str_val.into(), value.into()], "").try_as_basic_value().left().unwrap();
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
                        let node_type = self.get_type_by_id(node_type_id);
                        let resolved_generics = self.new_resolved_generics_via_instance(&node_type);
                        let to_string_fn = self.get_or_compile_to_string_method_for_type(node_type_id, &resolved_generics);
                        let str_val = self.builder.build_call(to_string_fn, &[res.into()], "repr").try_as_basic_value().left().unwrap().into_pointer_value();
                        let (len_val, chars_val) = self.destructure_string(str_val);

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

    fn create_closure_captures(&mut self, function: &Function, resolved_generics: &ResolvedGenerics) -> PointerValue<'a> {
        // Create array of captured variables for a closure. This is implemented as an `i64*`, where each `i64` item is an encoded representation
        // of the closed-over value. Variables are known to be captured at compile-time, so when they're initialized they're moved to the heap.
        // When constructing this array, allocate enough memory to hold all known captured variables (each of which will be a pointer), and treat
        // that pointer as an i64 which is stored in this chunk of memory. Upon retrieval, the value will be converted back into the appropriate
        // type, which is also known at compile-time. Using an `i64*` as the captures array helps simplify the model behind the scenes, and makes
        // calling functions/closures simpler.
        //
        // In addition to captured variables, the captures array _also_ includes any captures arrays of any closures that are closed-over within
        // this function (these `i64*` values are encoded as `i64` in the same way as above). Also, it's possible that a closure captures a variable
        // from _outside_ the current function scope. In this case, the containing functions must themselves become closures (if they're not already)
        // and that captured variable must be carried through the call stack. For example:
        //   val a = 1
        //   func outer() {
        //     func inner() { println(a) }
        //   }
        let malloc_size = self.const_i64(((function.captured_vars.len() + function.captured_closures.len()) * 8) as u64);
        let captured_vars_mem = self.malloc(malloc_size, self.closure_captures_t());
        for (idx, captured_var_id) in function.captured_vars.iter().enumerate() {
            let captured_var = self.project.get_var_by_id(captured_var_id);
            let val = if let Some(llvm_var) = self.ctx_stack.last().unwrap().variables.get(&captured_var_id) {
                match llvm_var {
                    LLVMVar::Slot(slot) => self.builder.build_load(*slot, &captured_var.name).into_pointer_value(),
                    LLVMVar::Param(param) => self.builder.build_load(param.into_pointer_value(), &captured_var.name).into_pointer_value(),
                }
            } else {
                self.get_captured_var_slot(captured_var_id, resolved_generics).unwrap()
            };
            let captured_var_value = self.builder.build_ptr_to_int(val, self.i64(), &format!("capture_{}_ptr_as_value", &captured_var.name));
            let slot = unsafe { self.builder.build_gep(captured_vars_mem, &[self.const_i32(idx as u64).into()], &format!("captured_var_{}_slot", &captured_var.name)) };
            self.builder.build_store(slot, captured_var_value);
        }

        for (idx, captured_func_id) in function.captured_closures.iter().enumerate() {
            let idx = idx + function.captured_vars.len();
            let captured_function = self.project.get_func_by_id(captured_func_id);
            let Some(captures_slot) = self.closure_captures.get(&captured_func_id) else { unreachable!("Captured closure {} does not yet have initialized captures", &captured_function.name); };
            let closure_captures = self.builder.build_load(*captures_slot, &format!("captures_{}", &captured_function.name)).into_pointer_value();

            let encoded_closure_captures = self.builder.build_ptr_to_int(closure_captures, self.i64(), "");
            let slot = unsafe { self.builder.build_gep(captured_vars_mem, &[self.const_i32(idx as u64).into()], &format!("captured_var_{}_slot", &captured_function.name)) };
            self.builder.build_store(slot, encoded_closure_captures);
        }

        captured_vars_mem
    }

    fn get_captured_var_slot(&mut self, var_id: &VarId, resolved_generics: &ResolvedGenerics) -> Option<PointerValue<'a>> {
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

    fn get_captures_for_closure(&self, function: &Function) -> BasicValueEnum<'a> {
        let call_is_at_defined_lexical_scope = self.current_fn.1.as_ref()
            .map(|func_id| {
                let current_function = self.project.get_func_by_id(func_id);
                function.id.0 == current_function.fn_scope_id
            })
            .unwrap_or(true);

        // If we're calling a closure and we're at the same lexical scope in which the closure was defined, then we should be able to grab
        // the captures array via its local, which we can assume to have already been created as per the above explanation. For example:
        //   var a = 1
        //   func foo() { a += 1 }
        //   foo()
        //
        // However, since we're using locals to store the captures array, if the call to the closure occurs at a _different_ lexical scope,
        // then that local will not be available to us within this new stack frame. So, functions which contain calls to closure functions
        // must _themselves_ become closures, and must capture the captures array of any closures called therein. For example:
        //   var a = 1
        //   func foo() { a += 1 }
        //   func bar() { foo() }
        let captures = if call_is_at_defined_lexical_scope {
            let Some(captures_slot) = self.closure_captures.get(&function.id) else { unreachable!("Closure {} does not have initialized captures", &function.name); };
            self.builder.build_load(*captures_slot, &format!("captures_{}", &function.name))
        } else {
            let current_function = self.project.get_func_by_id(&self.current_fn.1.expect("We cannot enter this block unless it's present in the above check"));
            let current_function_captures = self.current_fn.0.get_nth_param(0).unwrap().into_pointer_value();
            let Some((mut capture_idx, _)) = current_function.captured_closures.iter().find_position(|func_id| *func_id == &function.id) else { unreachable!() };
            capture_idx += current_function.captured_vars.len();

            let captures_slot = unsafe { self.builder.build_gep(current_function_captures, &[self.const_i32(capture_idx as u64).into()], "") };
            let encoded_captures = self.builder.build_load(captures_slot, "").into_int_value();
            let captures = self.builder.build_int_to_ptr(encoded_captures, self.closure_captures_t(), "");

            captures.into()
        };

        captures
    }

    fn compile_binding_declaration(&mut self, pattern: &BindingPattern, vars: &Vec<VarId>, expr_val: Option<BasicValueEnum<'a>>, resolved_generics: &ResolvedGenerics) {
        let BindingPattern::Variable(token) = pattern else { todo!() };
        let var_name = Token::get_ident_name(token);
        let Some(variable) = vars.iter().find_map(|var_id| {
            let var = self.project.get_var_by_id(var_id);
            if var.name == var_name { Some(var) } else { None }
        }) else { unreachable!() };

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
    }

    fn visit_statement(&mut self, node: &TypedNode, resolved_generics: &ResolvedGenerics) -> Option<BasicValueEnum<'a>> {
        match node {
            node @ TypedNode::If { .. } => self.visit_if_node(node, resolved_generics),
            TypedNode::Match { .. } => self.visit_match_node(node, resolved_generics),
            TypedNode::FuncDeclaration(func_id) => {
                let function = self.project.get_func_by_id(func_id);
                if function.is_closure() {
                    // If a function captures variables, gather those captures into a captures array, and store as a local. This local
                    // is used later on to invoke a function (if the closure is known statically) or to create a runtime function
                    // value (when a function-aliased identifier is referenced in a non-invocation context).
                    let captured_vars_mem = self.create_closure_captures(function, resolved_generics);
                    let captures_name = format!("captures_{}_{}_{}_{}", func_id.0.0.0, func_id.0.1, func_id.1, &function.name);
                    let captured_vars_slot = self.builder.build_alloca(self.closure_captures_t(), &captures_name);
                    self.builder.build_store(captured_vars_slot, captured_vars_mem);
                    self.closure_captures.insert(*func_id, captured_vars_slot);
                }

                None
            }
            TypedNode::TypeDeclaration(struct_id) => {
                let struct_ = self.project.get_struct_by_id(struct_id);

                for func_id in struct_.methods.iter().chain(&struct_.static_methods) {
                    let function = self.project.get_func_by_id(func_id);
                    if function.is_closure() {
                        let captured_vars_mem = self.create_closure_captures(function, resolved_generics);
                        let captures_name = format!("captures_{}_{}_{}_{}", func_id.0.0.0, func_id.0.1, func_id.1, &function.name);
                        let captured_vars_slot = self.builder.build_alloca(self.closure_captures_t(), &captures_name);
                        self.builder.build_store(captured_vars_slot, captured_vars_mem);
                        self.closure_captures.insert(*func_id, captured_vars_slot);
                    }
                }

                None
            }
            TypedNode::EnumDeclaration(_) => None,
            TypedNode::BindingDeclaration { vars, pattern, expr, .. } => {
                let Some(expr) = expr else { todo!() };
                let expr_val = self.visit_expression(expr, resolved_generics);

                self.compile_binding_declaration(pattern, vars, expr_val, resolved_generics);

                None
            }
            TypedNode::ForLoop { binding, binding_var_ids, index_var_id, iterator, body, block_terminator, .. } => {
                let iterator_ty = self.get_type_by_id(iterator.type_id());

                let resolved_generics = self.extend_resolved_generics_via_instance(resolved_generics, iterator.type_id());
                let iterator_val = self.visit_expression(iterator, &resolved_generics).unwrap();

                let Type::GenericInstance(struct_id, _) = &iterator_ty else { unreachable!() };
                let (iter_instance, iter_type_id) = if struct_id == &self.project.prelude_array_struct_id || struct_id == &self.project.prelude_set_struct_id || struct_id == &self.project.prelude_map_struct_id {
                    let (_, func_id) = iterator_ty.find_method_by_name(&self.project, "iterator").unwrap();
                    let iterator_func = self.project.get_func_by_id(func_id);

                    let iterator_method = self.get_or_compile_function(func_id, &resolved_generics);
                    let instance = self.builder.build_call(iterator_method, &[iterator_val.into()], "").try_as_basic_value().left().unwrap();

                    (instance, iterator_func.return_type_id)
                } else {
                    (iterator_val, *iterator.type_id())
                };

                let iter_ty = self.get_type_by_id(&iter_type_id);
                let (_, func_id) = iter_ty.find_method_by_name(&self.project, "next").unwrap();
                let resolved_generics = self.extend_resolved_generics_via_instance(&resolved_generics, &iter_type_id);
                let iter_next_func = self.get_or_compile_function(func_id, &resolved_generics);

                let cond_bb = self.context.append_basic_block(self.current_fn.0, "for_loop_cond");
                let body_bb = self.context.append_basic_block(self.current_fn.0, "for_loop_body");
                let end_bb = self.context.append_basic_block(self.current_fn.0, "for_loop_end");
                self.loop_stack.push((cond_bb, end_bb));
                if let Some(var_id) = index_var_id {
                    let var = self.project.get_var_by_id(var_id);
                    let binding = BindingPattern::Variable(Token::Ident(POSITION_BOGUS, var.name.clone()));
                    let neg_one = self.builder.build_int_neg(self.const_i64(1), "");
                    self.compile_binding_declaration(&binding, &vec![*var_id], Some(neg_one.into()), &resolved_generics);
                }
                let next_local = self.builder.build_alloca(iter_next_func.get_type().get_return_type().unwrap(), "next_local");
                self.builder.build_unconditional_branch(cond_bb);

                self.builder.position_at_end(cond_bb);
                let next_value = self.builder.build_call(iter_next_func, &[iter_instance.into()], "").try_as_basic_value().left().unwrap();
                self.builder.build_store(next_local, next_value);
                let cond = self.option_instance_get_is_set(next_local);
                self.builder.build_conditional_branch(cond, body_bb, end_bb);

                self.builder.position_at_end(body_bb);
                let next_value_val = self.option_instance_get_value(next_local);
                self.compile_binding_declaration(binding, binding_var_ids, Some(next_value_val), &resolved_generics);
                if let Some(var_id) = index_var_id {
                    let LLVMVar::Slot(index_var_slot) = self.ctx_stack.last().unwrap().variables.get(var_id).unwrap() else { unreachable!() };
                    let index_val = self.builder.build_load(*index_var_slot, "").into_int_value();
                    self.builder.build_store(*index_var_slot, self.builder.build_int_add(index_val, self.const_i64(1), ""));
                }
                for node in body {
                    self.visit_statement(node, &resolved_generics);
                }
                if block_terminator.is_none() {
                    self.builder.build_unconditional_branch(cond_bb);
                }

                self.builder.position_at_end(end_bb);
                self.loop_stack.pop();

                None
            }
            TypedNode::WhileLoop { condition, condition_var_id, body, block_terminator, .. } => {
                let loop_cond_block = self.context.append_basic_block(self.current_fn.0, "while_loop_cond");
                let loop_body_block = self.context.append_basic_block(self.current_fn.0, "while_loop_body");
                let loop_end_block = self.context.append_basic_block(self.current_fn.0, "while_loop_end");

                self.loop_stack.push((loop_cond_block, loop_end_block));

                self.builder.build_unconditional_branch(loop_cond_block);
                self.builder.position_at_end(loop_cond_block);

                let condition_type_id = *condition.type_id();
                let cond_is_opt = self.project.type_is_option(&condition_type_id).is_some();
                let cond_val = self.visit_expression(condition, resolved_generics).unwrap();
                let (cond_val, opt_cond_local) = if cond_is_opt {
                    let cond_local = self.builder.build_alloca(cond_val.get_type(), "");
                    self.builder.build_store(cond_local, cond_val);
                    let cond_val = self.option_instance_get_is_set(cond_local);
                    (cond_val, Some(cond_local))
                } else {
                    (cond_val.into_int_value(), None)
                };
                let comp = self.builder.build_int_compare(IntPredicate::EQ, cond_val, self.const_bool(true), "");

                self.builder.build_conditional_branch(comp, loop_body_block, loop_end_block);

                self.builder.position_at_end(loop_body_block);
                if let Some(condition_var_id) = condition_var_id {
                    let expr_val = if let Some(opt_cond_local) = opt_cond_local {
                        self.option_instance_get_value(opt_cond_local)
                    } else {
                        self.const_bool(true).into()
                    };
                    // TODO: While loops should support BindingPattern as conditional_binding rather than just a single var. When that's done, this can go away.
                    let var = self.project.get_var_by_id(condition_var_id);
                    let pat = BindingPattern::Variable(Token::Ident(var.defined_span.as_ref().unwrap().range.start.clone(), var.name.clone()));
                    self.compile_binding_declaration(&pat, &vec![*condition_var_id], Some(expr_val), resolved_generics);
                }
                for node in body {
                    self.visit_statement(node, resolved_generics);
                }
                if block_terminator.is_none() {
                    self.builder.build_unconditional_branch(loop_cond_block);
                }

                self.builder.position_at_end(loop_end_block);
                self.loop_stack.pop();

                None
            }
            TypedNode::Break { .. } => {
                let Some((_loop_start, loop_end)) = self.loop_stack.last() else { unreachable!("A break statement must be contained within a loop") };

                self.builder.build_unconditional_branch(*loop_end);

                None
            }
            TypedNode::Continue { .. } => {
                let Some((loop_start, _loop_end)) = self.loop_stack.last() else { unreachable!("A break statement must be contained within a loop") };

                self.builder.build_unconditional_branch(*loop_start);

                None
            }
            TypedNode::Return { expr, .. } => {
                if let Some(expr) = expr {
                    let return_value = self.visit_expression(expr, resolved_generics).unwrap();
                    self.builder.build_return(Some(&return_value));
                } else {
                    self.builder.build_return(None);
                }

                None
            }
            TypedNode::Import { .. } => None,
            _ => self.visit_expression(node, resolved_generics),
        }
    }

    fn cast_result_if_necessary(&mut self, value: BasicValueEnum<'a>, value_type_id: &TypeId, cast_target_type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> Option<BasicValueEnum<'a>> {
        if self.project.type_is_option(value_type_id).is_none() && self.project.type_is_option(cast_target_type_id).is_some() {
            let arr_val_local = self.builder.build_alloca(value.get_type(), "");
            self.builder.build_store(arr_val_local, value);
            let arr_val = self.builder.build_load(arr_val_local, "");
            Some(self.make_option_instance(cast_target_type_id, arr_val, &resolved_generics).into())
        } else if cast_target_type_id == &PRELUDE_ANY_TYPE_ID && value_type_id != &PRELUDE_ANY_TYPE_ID {
            let resolved_generics = self.extend_resolved_generics_via_instance(resolved_generics, value_type_id);
            let arr_val_local = self.builder.build_alloca(value.get_type(), "");
            self.builder.build_store(arr_val_local, value);
            let arr_val = self.builder.build_load(arr_val_local, "");
            Some(self.make_trait_instance(&PRELUDE_ANY_TYPE_ID, value_type_id, arr_val, &resolved_generics).into())
        } else {
            None
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

                self.cast_result_if_necessary(value, type_id, resolved_type_id, resolved_generics)
                    .or(Some(value))
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
                        } else if self.type_is_option(type_id).is_some() {
                            let local = self.builder.build_alloca(expr_val.get_type(), "");
                            self.builder.build_store(local, expr_val);
                            self.builder.build_not(self.option_instance_get_is_set(local), "").into()
                        } else {
                            unreachable!("`!` unary operator not defined for type {}", self.project.type_repr(type_id))
                        }
                    }
                };

                self.cast_result_if_necessary(value, type_id, resolved_type_id, resolved_generics)
                    .or(Some(value))
            }
            TypedNode::Binary { left, op, right, type_id, resolved_type_id, .. } => {
                let left_type_id = left.as_ref().type_id();
                let right_type_id = right.as_ref().type_id();

                let value = match op {
                    BinaryOp::Add => {
                        if left_type_id == &PRELUDE_STRING_TYPE_ID || right_type_id == &PRELUDE_STRING_TYPE_ID {
                            let string_ty = self.get_type_by_id(&PRELUDE_STRING_TYPE_ID);
                            let (member_idx, func_id) = string_ty.find_method_by_name(self.project, "concat").unwrap();
                            let function = self.project.get_func_by_id(func_id);

                            let string_concat_target = if left_type_id == &PRELUDE_STRING_TYPE_ID {
                                left.clone()
                            } else {
                                let right_ty = self.get_type_by_id(right_type_id);
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
                        if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            let left = self.visit_expression(left, resolved_generics).unwrap();
                            let right = self.visit_expression(right, resolved_generics).unwrap();

                            let value = if op == &BinaryOp::And {
                                self.builder.build_and(left.into_int_value(), right.into_int_value(), "")
                            } else {
                                self.builder.build_or(left.into_int_value(), right.into_int_value(), "")
                            };

                            value.into()
                        } else {
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
                    }
                    BinaryOp::Xor => {
                        let left = self.visit_expression(left, resolved_generics).unwrap();
                        let right = self.visit_expression(right, resolved_generics).unwrap();

                        if left_type_id == &PRELUDE_BOOL_TYPE_ID && right_type_id == &PRELUDE_BOOL_TYPE_ID {
                            self.builder.build_xor(left.into_int_value(), right.into_int_value(), "").as_basic_value_enum()
                        } else if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            self.builder.build_xor(left.into_int_value(), right.into_int_value(), "").as_basic_value_enum()
                        } else {
                            unreachable!("`^` operator not defined between types {} and {}", self.project.type_repr(left_type_id), self.project.type_repr(right_type_id))
                        }
                    }
                    BinaryOp::Coalesce => {
                        let left = self.visit_expression(left, resolved_generics).unwrap();
                        let left_local = self.builder.build_alloca(left.get_type(), "");
                        self.builder.build_store(left_local, left);
                        let cond_val = self.option_instance_get_is_set(left_local);
                        let cmp = self.builder.build_int_compare(IntPredicate::EQ, cond_val, self.const_bool(true), "");

                        let then_bb = self.context.append_basic_block(self.current_fn.0, "then");
                        let else_bb = self.context.append_basic_block(self.current_fn.0, "else");
                        let cont_bb = self.context.append_basic_block(self.current_fn.0, "cont");
                        self.builder.build_conditional_branch(cmp, then_bb, else_bb);

                        self.builder.position_at_end(then_bb);
                        let then_value = self.option_instance_get_value(left_local);
                        let then_bb = self.builder.get_insert_block().unwrap();
                        self.builder.build_unconditional_branch(cont_bb);

                        self.builder.position_at_end(else_bb);
                        let else_value = self.visit_expression(right, resolved_generics).unwrap();
                        let else_bb = self.builder.get_insert_block().unwrap();
                        self.builder.build_unconditional_branch(cont_bb);

                        self.builder.position_at_end(cont_bb);
                        let phi = self.builder.build_phi(else_value.get_type(), "");
                        phi.add_incoming(&[(&then_value, then_bb), (&else_value, else_bb)]);

                        phi.as_basic_value()
                    }
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
                        let left = self.visit_expression(left, resolved_generics).unwrap();
                        let right = self.visit_expression(right, resolved_generics).unwrap();
                        self.compile_eq(op == &BinaryOp::Neq, left_type_id, left, right_type_id, right, resolved_generics).into()
                    }
                    BinaryOp::Pow => {
                        let left_val = self.visit_expression(left, resolved_generics).unwrap();
                        let right_val = self.visit_expression(right, resolved_generics).unwrap();
                        if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            let pow_fn = self.main_module.get_function("llvm.powi.f64.i64").unwrap_or_else(|| {
                                self.main_module.add_function("llvm.powi.f64.i64", self.fn_type(self.f64(), &[self.f64().into(), self.i64().into()]), None)
                            });

                            let left = self.builder.build_signed_int_to_float(left_val.into_int_value(), self.f64(), "");
                            self.builder.build_call(pow_fn, &[left.into(), right_val.into_int_value().into()], "").try_as_basic_value().left().unwrap()
                        } else {
                            let pow_fn = self.main_module.get_function("llvm.pow.f64").unwrap_or_else(|| {
                                self.main_module.add_function("llvm.pow.f64", self.fn_type(self.f64(), &[self.f64().into(), self.f64().into()]), None)
                            });

                            let left;
                            let right;
                            if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_FLOAT_TYPE_ID {
                                left = self.builder.build_signed_int_to_float(left_val.into_int_value(), self.f64(), "");
                                right = right_val.into_float_value()
                            } else if left_type_id == &PRELUDE_FLOAT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                                left = left_val.into_float_value();
                                right = self.builder.build_signed_int_to_float(right_val.into_int_value(), self.f64(), "");
                            } else if left_type_id == &PRELUDE_FLOAT_TYPE_ID && right_type_id == &PRELUDE_FLOAT_TYPE_ID {
                                left = left_val.into_float_value();
                                right = right_val.into_float_value();
                            } else {
                                unreachable!("`**` operator not defined between types {} and {}", self.project.type_repr(left_type_id), self.project.type_repr(right_type_id))
                            };

                            // if a < 0 { -(-a ** b) } else { a ** b }
                            let cond = self.builder.build_float_compare(FloatPredicate::OLT, left, self.const_f64(0.0).into(), "cond");
                            let then_bb = self.context.append_basic_block(self.current_fn.0, "then");
                            let else_bb = self.context.append_basic_block(self.current_fn.0, "else");
                            let cont_bb = self.context.append_basic_block(self.current_fn.0, "cont");
                            self.builder.build_conditional_branch(cond, then_bb, else_bb);

                            self.builder.position_at_end(then_bb);
                            let neg_left = self.builder.build_float_mul(left, self.const_f64(-1.0), "");
                            let then_val = self.builder.build_call(pow_fn, &[neg_left.into(), right.into()], "").try_as_basic_value().left().unwrap().into_float_value();
                            let then_val = self.builder.build_float_mul(then_val, self.const_f64(-1.0), "");
                            self.builder.build_unconditional_branch(cont_bb);
                            let then_bb = self.builder.get_insert_block().unwrap();

                            self.builder.position_at_end(else_bb);
                            let else_val = self.builder.build_call(pow_fn, &[left.into(), right.into()], "").try_as_basic_value().left().unwrap();
                            self.builder.build_unconditional_branch(cont_bb);
                            let else_bb = self.builder.get_insert_block().unwrap();

                            self.builder.position_at_end(cont_bb);
                            let phi = self.builder.build_phi(self.f64(), "");
                            phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
                            phi.as_basic_value()
                        }
                    }
                    BinaryOp::ShiftLeft => {
                        let left = self.visit_expression(left, resolved_generics).unwrap();
                        let right = self.visit_expression(right, resolved_generics).unwrap();
                        if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            self.builder.build_left_shift(left.into_int_value(), right.into_int_value(), "").into()
                        } else {
                            unreachable!("`<<` operator not defined between types {} and {}", self.project.type_repr(left_type_id), self.project.type_repr(right_type_id))
                        }
                    }
                    BinaryOp::ShiftRight => {
                        let left = self.visit_expression(left, resolved_generics).unwrap();
                        let right = self.visit_expression(right, resolved_generics).unwrap();
                        if left_type_id == &PRELUDE_INT_TYPE_ID && right_type_id == &PRELUDE_INT_TYPE_ID {
                            self.builder.build_right_shift(left.into_int_value(), right.into_int_value(), false, "").into()
                        } else {
                            unreachable!("`>>` operator not defined between types {} and {}", self.project.type_repr(left_type_id), self.project.type_repr(right_type_id))
                        }
                    }
                    BinaryOp::AddEq |
                    BinaryOp::SubEq |
                    BinaryOp::MulEq |
                    BinaryOp::DivEq |
                    BinaryOp::ModEq |
                    BinaryOp::AndEq |
                    BinaryOp::OrEq |
                    BinaryOp::CoalesceEq => unreachable!("Handled in ::Assignment"),
                };

                self.cast_result_if_necessary(value, type_id, resolved_type_id, resolved_generics)
                    .or(Some(value))
            }
            TypedNode::Grouped { expr, .. } => self.visit_expression(expr, resolved_generics),
            TypedNode::Array { items, type_id, resolved_type_id, .. } => {
                let Type::GenericInstance(struct_id, generics) = self.get_type_by_id(type_id) else { unreachable!() };
                debug_assert!(struct_id == self.project.prelude_array_struct_id);
                let array_struct = self.project.get_struct_by_id(&struct_id);
                let inner_type_id = generics[0];
                let array_type = Type::Type(TypeKind::Struct(self.project.prelude_array_struct_id));

                let array_with_capacity_llvm_fn = {
                    let array_with_capacity_func_id = array_type.find_static_method_by_name(self.project, "withCapacity").unwrap();
                    let array_with_capacity_fn = self.project.get_func_by_id(array_with_capacity_func_id);

                    let inner_resolved = self.make_resolved_generic(&inner_type_id, resolved_generics);
                    let resolved_generics = resolved_generics.extend_via_pairs(vec![(array_with_capacity_fn.generic_ids[0], inner_resolved)]);
                    self.get_or_compile_function(&array_with_capacity_func_id, &resolved_generics)
                };
                let array_push_llvm_fn = {
                    let (_, array_push_func_id) = array_type.find_method_by_name(self.project, "push").unwrap();
                    let inner_resolved = self.make_resolved_generic(&inner_type_id, resolved_generics);
                    let resolved_generics = resolved_generics.extend_via_pairs(vec![(array_struct.generic_ids[0], inner_resolved)]);
                    self.get_or_compile_function(&array_push_func_id, &resolved_generics)
                };

                let arr_val = self.builder.build_call(array_with_capacity_llvm_fn, &[self.const_i64(items.len() as u64).into()], "").try_as_basic_value().left().unwrap();
                for item in items {
                    let item = self.visit_expression(item, resolved_generics).unwrap();
                    self.builder.build_call(array_push_llvm_fn, &[arr_val.into(), item.into()], "").try_as_basic_value().left();
                }

                self.cast_result_if_necessary(arr_val, type_id, resolved_type_id, resolved_generics)
                    .or(Some(arr_val))
            }
            TypedNode::Tuple { type_id, items, resolved_type_id, .. } => {
                let Type::GenericInstance(struct_id, _) = self.get_type_by_id(type_id) else { unreachable!() };
                debug_assert!(struct_id == self.project.prelude_tuple_struct_id);

                let resolved_generics = self.extend_resolved_generics_via_instance(resolved_generics, type_id);
                let Some(llvm_type) = self.llvm_underlying_type_by_id(type_id, &resolved_generics) else { todo!() };
                let mem = self.malloc(self.sizeof_struct(llvm_type), self.ptr(llvm_type));

                let llvm_type_name = self.llvm_type_name_by_id(type_id, &resolved_generics);
                debug_assert!(llvm_type.into_struct_type().get_name().unwrap().to_str().unwrap() == &llvm_type_name);

                let typeid = self.get_typeid_by_name(&llvm_type_name);
                let typeid_ptr = self.builder.build_struct_gep(mem, 0, "typeid_slot").unwrap();
                self.builder.build_store(typeid_ptr, self.const_i32(typeid as u64));

                for (idx, item) in items.iter().enumerate() {
                    let slot = self.builder.build_struct_gep(mem, (idx + 1) as u32, &format!("tuple_{}_slot", idx)).unwrap();
                    let val = self.visit_expression(item, &resolved_generics).unwrap();
                    self.builder.build_store(slot, val);
                }

                let tuple_val = mem.as_basic_value_enum();

                self.cast_result_if_necessary(tuple_val, type_id, resolved_type_id, &resolved_generics)
                    .or(Some(tuple_val))
            }
            TypedNode::Set { items, type_id, resolved_type_id, .. } => {
                let Type::GenericInstance(struct_id, generics) = self.get_type_by_id(type_id) else { unreachable!() };
                debug_assert!(struct_id == self.project.prelude_set_struct_id);
                let set_struct = self.project.get_struct_by_id(&struct_id);
                let inner_type_id = generics[0];
                let set_type = Type::Type(TypeKind::Struct(self.project.prelude_set_struct_id));

                let set_new_llvm_fn = {
                    let set_new_func_id = set_type.find_static_method_by_name(self.project, "new").unwrap();
                    let set_new_fn = self.project.get_func_by_id(set_new_func_id);

                    let inner_resolved = self.make_resolved_generic(&inner_type_id, resolved_generics);
                    let resolved_generics = resolved_generics.extend_via_pairs(vec![(set_new_fn.generic_ids[0], inner_resolved)]);
                    self.get_or_compile_function(&set_new_func_id, &resolved_generics)
                };
                let set_insert_llvm_fn = {
                    let (_, set_insert_func_id) = set_type.find_method_by_name(self.project, "insert").unwrap();
                    let inner_resolved = self.make_resolved_generic(&inner_type_id, resolved_generics);
                    let resolved_generics = resolved_generics.extend_via_pairs(vec![(set_struct.generic_ids[0], inner_resolved)]);
                    self.get_or_compile_function(&set_insert_func_id, &resolved_generics)
                };

                let set_val = self.builder.build_call(set_new_llvm_fn, &[self.const_i64(0).into(), self.const_i16(1).into()], "").try_as_basic_value().left().unwrap();
                for val in items {
                    let val = self.visit_expression(val, resolved_generics).unwrap();
                    self.builder.build_call(set_insert_llvm_fn, &[set_val.into(), val.into()], "").try_as_basic_value().left();
                }

                self.cast_result_if_necessary(set_val, type_id, resolved_type_id, resolved_generics)
                    .or(Some(set_val))
            }
            TypedNode::Map { items, type_id, resolved_type_id, .. } => {
                let Type::GenericInstance(struct_id, generics) = self.get_type_by_id(type_id) else { unreachable!() };
                debug_assert!(struct_id == self.project.prelude_map_struct_id);
                let map_struct = self.project.get_struct_by_id(&struct_id);
                let key_type_id = generics[0];
                let val_type_id = generics[1];
                let map_type = Type::Type(TypeKind::Struct(self.project.prelude_map_struct_id));

                let map_new_llvm_fn = {
                    let map_new_func_id = map_type.find_static_method_by_name(self.project, "new").unwrap();
                    let map_new_fn = self.project.get_func_by_id(map_new_func_id);

                    let resolved_generics = resolved_generics.extend_via_pairs(vec![
                        (map_new_fn.generic_ids[0], self.make_resolved_generic(&key_type_id, resolved_generics)),
                        (map_new_fn.generic_ids[1], self.make_resolved_generic(&val_type_id, resolved_generics)),
                    ]);
                    self.get_or_compile_function(&map_new_func_id, &resolved_generics)
                };
                let map_insert_llvm_fn = {
                    let (_, map_insert_func_id) = map_type.find_method_by_name(self.project, "insert").unwrap();
                    let resolved_generics = resolved_generics.extend_via_pairs(vec![
                        (map_struct.generic_ids[0], self.make_resolved_generic(&key_type_id, resolved_generics)),
                        (map_struct.generic_ids[1], self.make_resolved_generic(&val_type_id, resolved_generics)),
                    ]);
                    self.get_or_compile_function(&map_insert_func_id, &resolved_generics)
                };

                let map_val = self.builder.build_call(map_new_llvm_fn, &[self.const_i64(0).into(), self.const_i16(1).into()], "").try_as_basic_value().left().unwrap();
                for (key, val) in items {
                    let key = self.visit_expression(key, resolved_generics).unwrap();
                    let val = self.visit_expression(val, resolved_generics).unwrap();
                    self.builder.build_call(map_insert_llvm_fn, &[map_val.into(), key.into(), val.into()], "").try_as_basic_value().left();
                }

                self.cast_result_if_necessary(map_val, type_id, resolved_type_id, resolved_generics)
                    .or(Some(map_val))
            }
            TypedNode::Identifier { var_id, resolved_type_id, .. } => {
                let variable = self.project.get_var_by_id(var_id);

                let value = match variable.alias {
                    VariableAlias::None => {
                        // If the variable is captured, then the underlying model is different and needs to be handled specially.
                        // Captured variables are moved to the heap upon initialization (see TypedNode::BindingDeclaration), and
                        // their backing type uses pointer indirection. If we're currently in a function which closes over the
                        // variable, we handle it here...
                        if let Some(ptr) = self.get_captured_var_slot(var_id, resolved_generics) {
                            self.builder.build_load(ptr, &format!("decoded_captured_var_{}", &variable.name))
                        } else {
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
                    }
                    VariableAlias::Function(func_id) => {
                        // todo: cache value to not create duplicate?
                        let function = self.project.get_func_by_id(&func_id);
                        let captures = if function.is_closure() {
                            let captures = self.get_captures_for_closure(function);
                            Some(captures.into_pointer_value())
                        } else {
                            None
                        };

                        self.make_function_value(&func_id, resolved_type_id, captures, resolved_generics)
                    }
                    VariableAlias::Type(_) => todo!()
                };

                self.cast_result_if_necessary(value, &variable.type_id, resolved_type_id, &resolved_generics)
                    .or(Some(value))
            }
            TypedNode::NoneValue { resolved_type_id, .. } => {
                Some(self.make_none_option_instance(resolved_type_id, resolved_generics).into())
            }
            TypedNode::Invocation { target, arguments, type_arg_ids, type_id, resolved_type_id, .. } => {
                let mut method_self_arg = None;
                let mut args = Vec::with_capacity(arguments.len());

                let params_data;
                let mut is_opt_safe_accessor = false;
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

                                let realized_generics = type_arg_ids.iter().map(|type_id| self.make_resolved_generic(type_id, resolved_generics)).collect();
                                new_resolved_generics = resolved_generics.extend_via_func_call(function, &realized_generics);

                                if let Some(dec) = function.decorators.iter().find(|dec| dec.name == "Intrinsic") {
                                    let TypedNode::Literal { value: TypedLiteral::String(intrinsic_name), .. } = &dec.args[0] else { unreachable!("@Intrinsic requires exactly 1 String argument") };
                                    return self.compile_intrinsic_invocation(type_arg_ids, &new_resolved_generics, intrinsic_name, Some(&**target), arguments, resolved_type_id);
                                }

                                if function.is_closure() {
                                    let captures = self.get_captures_for_closure(function);
                                    args.push(captures.into());
                                }

                                self.get_or_compile_function(func_id, &new_resolved_generics).into()
                            }
                            VariableAlias::Type(TypeKind::Enum(_)) => unreachable!("Cannot invoke an enum directly"),
                            VariableAlias::Type(TypeKind::Struct(struct_id)) => {
                                let struct_ = self.project.get_struct_by_id(struct_id);
                                params_data = struct_.fields.iter().map(|f| (f.type_id, f.default_value.is_some())).collect_vec();

                                let realized_generics = type_arg_ids.iter().map(|type_id| self.make_resolved_generic(type_id, resolved_generics)).collect();

                                new_resolved_generics = resolved_generics.extend_via_struct(struct_, &realized_generics);
                                self.get_or_compile_type_initializer(struct_id, &new_resolved_generics).into()
                            }
                        }
                    }
                    TypedNode::Accessor { target, kind, member_idx, is_opt_safe, .. } if kind != &AccessorKind::Field => {
                        let mut target_type_id = resolved_generics.resolve_if_generic(&target.type_id(), &self.project)
                            .map(|resolved| resolved.type_id)
                            .unwrap_or(*target.type_id());

                        if *is_opt_safe {
                            if let Some(inner_type_id) = self.project.type_is_option(&target_type_id) {
                                target_type_id = inner_type_id;
                                is_opt_safe_accessor = true;
                            }
                        }

                        let target_ty = self.get_type_by_id(&target_type_id);

                        match kind {
                            AccessorKind::Field => unreachable!("Field accessor nodes should be handled in the catchall case below"),
                            AccessorKind::Method if self.type_is_tuple(&target_type_id).is_some() => {
                                debug_assert!(*member_idx == METHOD_IDX_TOSTRING || *member_idx == METHOD_IDX_HASH, "Tuples don't have any methods aside from toString/hash");
                                params_data = vec![(target_type_id, false)];

                                let target = self.visit_expression(target, &resolved_generics).unwrap();
                                method_self_arg = Some((0, target));

                                self.get_or_compile_tuple_method(&target_type_id, &resolved_generics, member_idx).into()
                            }
                            AccessorKind::Method if self.type_is_option(&target_type_id).is_some() => {
                                let Some(inner_type_id) = self.type_is_option(&target_type_id) else { unreachable!() };

                                let func_id = self.get_type_by_id(&inner_type_id).get_method(self.project, *member_idx).unwrap();
                                let function = self.project.get_func_by_id(&func_id);
                                params_data = function.params.iter().skip(1).map(|p| (p.type_id, p.default_value.is_some())).collect_vec();

                                let target = self.visit_expression(target, &resolved_generics).unwrap();
                                method_self_arg = Some((0, target));

                                self.get_or_compile_option_method(&resolved_generics, &inner_type_id, member_idx).into()
                            }
                            AccessorKind::Method if self.project.type_is_trait(&target_type_id) => {
                                debug_assert!(!*is_opt_safe, "What does it even mean to have an option-safe accessor for a trait method at this point?");
                                debug_assert!(arguments.iter().all(|a| a.is_some()), "Trait methods with default-valued parameters not yet supported");

                                let instance = self.visit_expression(target, &resolved_generics).unwrap();
                                let instance_local = self.builder.build_alloca(instance.get_type(), "instance_local");
                                self.builder.build_store(instance_local, instance);

                                let value_slot = self.builder.build_struct_gep(instance_local, 1, "value_slot").unwrap();
                                let underlying_value = self.builder.build_load(value_slot, "underlying_value");
                                method_self_arg = Some((0, underlying_value));

                                let vtable_slot = self.builder.build_struct_gep(instance_local, 0, "vtable_slot").unwrap();
                                let vtable_value = self.builder.build_load(vtable_slot, "vtable").into_pointer_value();
                                let method_slot = self.builder.build_struct_gep(vtable_value, (*member_idx + 1) as u32, "method_slot").unwrap();
                                let method_val = self.builder.build_load(method_slot, "method").into_pointer_value();

                                let fn_ptr_type = match *member_idx {
                                    METHOD_IDX_TOSTRING => {
                                        params_data = vec![(target_type_id, false)];

                                        let tostring_fn_type = self.fn_type(self.ptr(self.string_type), &[self.i64().into()]);
                                        tostring_fn_type.ptr_type(AddressSpace::Generic)
                                    }
                                    METHOD_IDX_HASH => {
                                        params_data = vec![(target_type_id, false)];

                                        let hash_fn_type = self.fn_type(self.i64(), &[self.i64().into()]);
                                        hash_fn_type.ptr_type(AddressSpace::Generic)
                                    }
                                    _ => unimplemented!("For now, the only trait is `Any`, and only the `toString`/`hash` methods are implemented")
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

                                if function.is_closure() {
                                    let captures = self.get_captures_for_closure(function);
                                    args.push(captures.into());
                                }

                                let target = self.visit_expression(target, &resolved_generics).unwrap();
                                method_self_arg = Some((if function.is_closure() { 1 } else { 0 }, target));

                                let realized_generics = type_arg_ids.iter().map(|type_id| self.make_resolved_generic(type_id, resolved_generics)).collect();
                                new_resolved_generics = self.extend_resolved_generics_via_instance(resolved_generics, &target_type_id);
                                new_resolved_generics = new_resolved_generics.extend_via_func_call(function, &realized_generics);
                                self.get_or_compile_function(&func_id, &new_resolved_generics).into()
                            }
                            AccessorKind::StaticMethod => {
                                let func_id = target_ty.get_static_method(self.project, *member_idx).unwrap();
                                let function = self.project.get_func_by_id(&func_id);
                                params_data = function.params.iter().map(|p| (p.type_id, p.default_value.is_some())).collect_vec();

                                let realized_generics = type_arg_ids.iter().map(|type_id| self.make_resolved_generic(type_id, resolved_generics)).collect();
                                new_resolved_generics = resolved_generics.extend_via_func_call(function, &realized_generics);

                                if let Some(dec) = function.decorators.iter().find(|dec| dec.name == "Intrinsic") {
                                    let TypedNode::Literal { value: TypedLiteral::String(intrinsic_name), .. } = &dec.args[0] else { unreachable!("@Intrinsic requires exactly 1 String argument") };
                                    return self.compile_intrinsic_invocation(type_arg_ids, &new_resolved_generics, intrinsic_name, None, arguments, resolved_type_id);
                                }

                                if function.is_closure() {
                                    let captures = self.get_captures_for_closure(function);
                                    args.push(captures.into());
                                }

                                self.get_or_compile_function(&func_id, &new_resolved_generics).into()
                            }
                            AccessorKind::EnumVariant => {
                                let Type::Type(TypeKind::Enum(enum_id)) = self.get_type_by_id(&target_type_id) else { unreachable!() };
                                let enum_ = self.project.get_enum_by_id(&enum_id);
                                let variant = &enum_.variants[*member_idx];
                                let EnumVariantKind::Container(func_id) = &variant.kind else { unreachable!() };
                                let function = self.project.get_func_by_id(func_id);
                                params_data = function.params.iter().map(|p| (p.type_id, p.default_value.is_some())).collect_vec();

                                let enum_type_name = self.llvm_type_name_by_id(&target_type_id, &resolved_generics);
                                self.get_or_compile_tagged_union_enum_variant_function(&enum_id, &enum_type_name, *member_idx, resolved_generics).into()
                            }
                        }
                    }
                    _ => {
                        // NB: If we're in this case then we know we don't have to handle optional parameters, since
                        // function values do not have optional parameters. As such, this block returns and skips
                        // the post-`match` logic.

                        new_resolved_generics = resolved_generics.clone();

                        let fn_obj = self.visit_expression(target, resolved_generics).unwrap().into_pointer_value();

                        let captures_slot = self.builder.build_struct_gep(fn_obj, 0, "captures_slot").unwrap();
                        let captures = self.builder.build_load(captures_slot, "captures").into_pointer_value();

                        let fn_ptr_slot = self.builder.build_struct_gep(fn_obj, 1, "fn_ptr_slot").unwrap();
                        let fn_ptr = self.builder.build_load(fn_ptr_slot, "fn_ptr").into_pointer_value();

                        // When calling a function value, we need to perform different logic depending on whether the function is a closure
                        // or not. This cannot be known at compile time for dynamic values, so instead it's based on whether the `captures`
                        // value in the function object is `NULL` or not. If it is, then the function can be called via the `fn_ptr` field;
                        // if it's not NULL, then the `fn_ptr` first needs to be re-cast to its original signature (since when it was stored
                        // in the object, the `captures` parameter was removed via a cast), and then the `captures` field can be passed in.
                        let cond = self.builder.build_int_compare(
                            IntPredicate::EQ,
                            self.builder.build_ptr_to_int(captures, self.i64(), ""),
                            self.const_i64(0),
                            "cond",
                        );
                        let then_bb = self.context.append_basic_block(self.current_fn.0, "then_bb");
                        let else_bb = self.context.append_basic_block(self.current_fn.0, "else_bb");
                        let cont_bb = self.context.append_basic_block(self.current_fn.0, "cont_bb");
                        self.builder.build_conditional_branch(cond, then_bb, else_bb);

                        // If the function value is not a closure, call it normally.
                        self.builder.position_at_end(then_bb);
                        debug_assert!(args.is_empty(), "Just a sanity check to make sure that we haven't added arguments prior to this point");
                        let mut args = Vec::with_capacity(arguments.len());
                        for arg_node in arguments {
                            let arg_node = arg_node.as_ref().unwrap();
                            let arg_value = self.visit_expression(arg_node, &new_resolved_generics).expect("Unit cannot be a valid argument value");
                            args.push(arg_value.into());
                        }
                        let callable = CallableValue::try_from(fn_ptr).unwrap();
                        let then_value = self.builder.build_call(callable, args.as_slice(), "").try_as_basic_value().left();
                        self.builder.build_unconditional_branch(cont_bb);
                        let then_bb = self.builder.get_insert_block().unwrap();

                        // If the function value is a closure, cast it and then call it.
                        self.builder.position_at_end(else_bb);
                        let mut args = Vec::with_capacity(arguments.len());
                        args.push(captures.into());
                        for arg_node in arguments {
                            let arg_node = arg_node.as_ref().unwrap();
                            let arg_value = self.visit_expression(arg_node, &new_resolved_generics).expect("Unit cannot be a valid argument value");
                            args.push(arg_value.into());
                        }
                        let orig_fn_type = fn_ptr.get_type().get_element_type().into_function_type();
                        let mut cast_fn_type_params = orig_fn_type.get_param_types();
                        cast_fn_type_params.insert(0, self.closure_captures_t().into());
                        let cast_fn_type_params = cast_fn_type_params.into_iter().map(|t| t.into()).collect_vec();
                        let cast_fn_type = if let Some(return_type) = orig_fn_type.get_return_type() {
                            return_type.fn_type(&cast_fn_type_params.as_slice(), false)
                        } else {
                            self.context.void_type().fn_type(&cast_fn_type_params.as_slice(), false)
                        };
                        let cast_fn_type_ptr = cast_fn_type.ptr_type(AddressSpace::Generic);

                        let fn_ptr = self.builder.build_pointer_cast(fn_ptr, cast_fn_type_ptr, "");
                        let callable = CallableValue::try_from(fn_ptr).unwrap();
                        let else_value = self.builder.build_call(callable, args.as_slice(), "").try_as_basic_value().left();
                        self.builder.build_unconditional_branch(cont_bb);
                        let else_bb = self.builder.get_insert_block().unwrap();

                        self.builder.position_at_end(cont_bb);
                        let value = match (then_value, else_value) {
                            (Some(then_value), Some(else_value)) => {
                                let phi = self.builder.build_phi(then_value.get_type(), "");
                                phi.add_incoming(&[(&then_value, then_bb), (&else_value, else_bb)]);
                                Some(phi.as_basic_value())
                            }
                            (None, None) => None,
                            _ => unreachable!()
                        };

                        return if let Some(value) = value {
                            self.cast_result_if_necessary(value, &type_id, resolved_type_id, &resolved_generics)
                                .or(Some(value))
                        } else {
                            value
                        };
                    }
                };

                let opt_safe_blocks = if is_opt_safe_accessor {
                    let Some((method_self_arg_idx, method_self_arg_value)) = method_self_arg else { unreachable!("How did we end up performing an optional safe invocation without a self argument?") };

                    let self_local = self.builder.build_alloca(method_self_arg_value.get_type(), "");
                    self.builder.build_store(self_local, method_self_arg_value);
                    let cond_val = self.option_instance_get_is_set(self_local);

                    let then_bb = self.context.append_basic_block(self.current_fn.0, "then");
                    let else_bb = self.context.append_basic_block(self.current_fn.0, "else");
                    let cont_bb = self.context.append_basic_block(self.current_fn.0, "cont");
                    self.builder.build_conditional_branch(cond_val, then_bb, else_bb);

                    self.builder.position_at_end(then_bb);
                    let self_arg_value = self.option_instance_get_value(self_local);

                    method_self_arg = Some((method_self_arg_idx, self_arg_value));

                    Some((else_bb, cont_bb))
                } else {
                    None
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

                if let Some((idx, arg)) = method_self_arg {
                    args.insert(idx, arg.into());
                }

                let value = self.builder.build_call(llvm_fn_val, args.as_slice(), "").try_as_basic_value().left();
                let value = if let Some((else_bb, cont_bb)) = opt_safe_blocks {
                    if let Some(value) = value {
                        let then_value = self.make_option_instance(type_id, value, resolved_generics);
                        let then_bb = self.builder.get_insert_block().unwrap();
                        self.builder.build_unconditional_branch(cont_bb);

                        self.builder.position_at_end(else_bb);
                        let else_value = self.make_none_option_instance(type_id, resolved_generics);
                        let else_bb = self.builder.get_insert_block().unwrap();
                        self.builder.build_unconditional_branch(cont_bb);

                        self.builder.position_at_end(cont_bb);
                        let Some(llvm_type) = self.llvm_underlying_type_by_id(&type_id, resolved_generics) else { todo!() };
                        let llvm_type = self.llvm_ptr_wrap_type_if_needed(llvm_type);
                        let phi = self.builder.build_phi(llvm_type, "");
                        phi.add_incoming(&[(&then_value, then_bb), (&else_value, else_bb)]);

                        Some(phi.as_basic_value())
                    } else {
                        self.builder.build_unconditional_branch(cont_bb);

                        self.builder.position_at_end(else_bb);
                        self.builder.build_unconditional_branch(cont_bb);

                        self.builder.position_at_end(cont_bb);
                        None
                    }
                } else {
                    value
                };

                if let Some(value) = value {
                    self.cast_result_if_necessary(value, &type_id, resolved_type_id, &resolved_generics)
                        .or(Some(value))
                } else {
                    value
                }
            }
            TypedNode::Accessor { target, kind, member_idx, is_opt_safe, type_id, resolved_type_id, .. } => {
                let mut target_type_id = *target.type_id();

                if let Type::Type(TypeKind::Enum(enum_id)) = self.get_type_by_id(&target_type_id) {
                    let enum_type_name = self.llvm_type_name_by_id(&target_type_id, resolved_generics);
                    let instance = self.construct_const_enum_variant(&enum_id, &enum_type_name, *member_idx, resolved_generics);

                    return self.cast_result_if_necessary(instance, &type_id, resolved_type_id, &resolved_generics)
                        .or(Some(instance));
                }

                let target = self.visit_expression(target, resolved_generics).unwrap();
                let opt_inner_type_id = self.type_is_option(&target_type_id);
                let (target_value, opt_safe_blocks) = if *is_opt_safe && opt_inner_type_id.is_some() {
                    target_type_id = opt_inner_type_id.unwrap();

                    let target_local = self.builder.build_alloca(target.get_type(), "");
                    self.builder.build_store(target_local, target);
                    let cond_val = self.option_instance_get_is_set(target_local);

                    let then_bb = self.context.append_basic_block(self.current_fn.0, "then");
                    let else_bb = self.context.append_basic_block(self.current_fn.0, "else");
                    let cont_bb = self.context.append_basic_block(self.current_fn.0, "cont");
                    self.builder.build_conditional_branch(cond_val, then_bb, else_bb);

                    self.builder.position_at_end(then_bb);
                    let target_value = self.option_instance_get_value(target_local);
                    (target_value.into_pointer_value(), Some((else_bb, cont_bb)))
                } else {
                    (target.into_pointer_value(), None)
                };

                let target_ty = self.get_type_by_id(&target_type_id);

                let value = match kind {
                    AccessorKind::Field => {
                        let field = target_ty.get_field(self.project, *member_idx).unwrap();
                        let field_slot = self.builder.build_struct_gep(target_value, (*member_idx + 1) as u32, &field.name).unwrap();
                        self.builder.build_load(field_slot, "")
                    }
                    AccessorKind::Method |
                    AccessorKind::StaticMethod |
                    AccessorKind::EnumVariant => todo!()
                };

                let value = if let Some((else_bb, cont_bb)) = opt_safe_blocks {
                    let then_value = self.make_option_instance(type_id, value, resolved_generics);
                    let then_bb = self.builder.get_insert_block().unwrap();
                    self.builder.build_unconditional_branch(cont_bb);

                    self.builder.position_at_end(else_bb);
                    let else_value = self.make_none_option_instance(type_id, resolved_generics);
                    let else_bb = self.builder.get_insert_block().unwrap();
                    self.builder.build_unconditional_branch(cont_bb);

                    self.builder.position_at_end(cont_bb);
                    let Some(llvm_type) = self.llvm_underlying_type_by_id(&type_id, resolved_generics) else { todo!() };
                    let llvm_type = self.llvm_ptr_wrap_type_if_needed(llvm_type);
                    let phi = self.builder.build_phi(llvm_type, "");
                    phi.add_incoming(&[(&then_value, then_bb), (&else_value, else_bb)]);

                    phi.as_basic_value()
                } else {
                    value
                };

                self.cast_result_if_necessary(value, &type_id, resolved_type_id, &resolved_generics)
                    .or(Some(value))
            }
            TypedNode::Indexing { target, index, type_id, resolved_type_id, .. } => {
                let target_type_id = target.as_ref().type_id();
                let target_ty = self.get_type_by_id(target_type_id);

                let empty_generics = vec![];
                let (target_struct_id, target_generics) = if let Type::GenericInstance(target_struct_id, target_generics) = &target_ty {
                    (target_struct_id, target_generics)
                } else if target_type_id == &PRELUDE_STRING_TYPE_ID {
                    (&self.project.prelude_string_struct_id, &empty_generics)
                } else {
                    unreachable!("All indexable types are struct instances")
                };

                match index {
                    IndexingMode::Index(idx_expr) if target_struct_id == &self.project.prelude_tuple_struct_id => {
                        let TypedNode::Literal { value: TypedLiteral::Int(idx), .. } = &**idx_expr else { unreachable!() };
                        let tuple_ptr = self.visit_expression(target, resolved_generics).unwrap().into_pointer_value();

                        let slot = self.builder.build_struct_gep(tuple_ptr, (*idx + 1) as u32, &format!("tuple_item_{}_slot", idx)).unwrap();
                        let val = self.builder.build_load(slot, &format!("tuple_item_{}", idx));

                        self.cast_result_if_necessary(val, &type_id, resolved_type_id, &resolved_generics)
                            .or(Some(val))
                    }
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
                let function = self.project.get_func_by_id(func_id);

                let captures = if function.is_closure() {
                    Some(self.create_closure_captures(function, resolved_generics))
                } else {
                    None
                };

                Some(self.make_function_value(func_id, resolved_type_id, captures, resolved_generics))
            }
            TypedNode::Assignment { kind, expr, .. } => {
                match kind {
                    AssignmentKind::Identifier { var_id } => {
                        let expr_val = self.visit_expression(expr, resolved_generics).unwrap();
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

                        Some(expr_val)
                    }
                    AssignmentKind::Accessor { target, kind, member_idx } => {
                        let target_type_id = target.type_id();
                        let target = self.visit_expression(target, resolved_generics).unwrap();

                        let expr_val = self.visit_expression(expr, resolved_generics).unwrap();

                        match kind {
                            AccessorKind::Field => {
                                let target_ty = self.get_type_by_id(target_type_id);
                                let field = target_ty.get_field(self.project, *member_idx).unwrap();
                                let field_slot = self.builder.build_struct_gep(target.into_pointer_value(), (*member_idx + 1) as u32, &field.name).unwrap();
                                self.builder.build_store(field_slot, expr_val);
                            }
                            AccessorKind::Method | AccessorKind::StaticMethod | AccessorKind::EnumVariant => unreachable!("Cannot assign to methods or variants"),
                        }

                        Some(expr_val)
                    }
                    AssignmentKind::Indexing { target, index } => {
                        let target_ty = self.get_type_by_id(target.type_id());
                        let Type::GenericInstance(struct_id, target_generics) = &target_ty  else { unreachable!() };
                        debug_assert!(struct_id == &self.project.prelude_array_struct_id);
                        let array_inner_type_id = &target_generics[0];
                        let (array_set_member_idx, array_set_func_id) = target_ty.find_method_by_name(self.project, "set").unwrap();
                        let array_set_function = self.project.get_func_by_id(array_set_func_id);

                        return self.visit_expression(&TypedNode::Invocation {
                            target: Box::new(TypedNode::Accessor {
                                target: target.clone(),
                                kind: AccessorKind::Method,
                                is_opt_safe: false,
                                member_idx: array_set_member_idx,
                                member_span: target.span(),
                                type_id: array_set_function.fn_type_id,
                                type_arg_ids: vec![],
                                resolved_type_id: array_set_function.fn_type_id,
                            }),
                            arguments: vec![
                                Some(*index.clone()), Some(*expr.clone()),
                            ],
                            type_arg_ids: vec![*array_inner_type_id],
                            type_id: *array_inner_type_id,
                            resolved_type_id: *array_inner_type_id,
                        }, resolved_generics);
                    }
                }
            }
            node @ TypedNode::If { .. } => self.visit_if_node(node, resolved_generics),
            node @ TypedNode::Match { .. } => self.visit_match_node(node, resolved_generics),
            _ => unreachable!("Node {:?} is not an expression and should have been handled in visit_statement", node)
        }
    }

    fn compile_eq(&mut self, negate: bool, left_type_id: &TypeId, left: BasicValueEnum<'a>, right_type_id: &TypeId, right: BasicValueEnum<'a>, resolved_generics: &ResolvedGenerics) -> IntValue<'a> {
        let comp_op_int = if negate { IntPredicate::NE } else { IntPredicate::EQ };
        let comp_op_float = if negate { FloatPredicate::ONE } else { FloatPredicate::OEQ };

        let (left_type_id, left_type_name) = resolved_generics.resolve_if_generic(left_type_id, &self.project)
            .map(|resolved| (resolved.type_id, resolved.llvm_type_name.clone()))
            .unwrap_or_else(|| {
                (*left_type_id, self.llvm_type_name_by_id(left_type_id, resolved_generics))
            });
        let (right_type_id, right_type_name) = resolved_generics.resolve_if_generic(right_type_id, &self.project)
            .map(|resolved| (resolved.type_id, resolved.llvm_type_name.clone()))
            .unwrap_or_else(|| {
                (*right_type_id, self.llvm_type_name_by_id(right_type_id, resolved_generics))
            });

        if left_type_id == PRELUDE_INT_TYPE_ID && right_type_id == PRELUDE_INT_TYPE_ID {
            self.builder.build_int_compare(comp_op_int, left.into_int_value(), right.into_int_value(), "").into()
        } else if left_type_id == PRELUDE_BOOL_TYPE_ID && right_type_id == PRELUDE_BOOL_TYPE_ID {
            self.builder.build_int_compare(comp_op_int, left.into_int_value(), right.into_int_value(), "").into()
        } else if left_type_id == PRELUDE_INT_TYPE_ID && right_type_id == PRELUDE_FLOAT_TYPE_ID {
            let left = self.builder.build_signed_int_to_float(left.into_int_value(), self.f64(), "");
            self.builder.build_float_compare(comp_op_float, left, right.into_float_value(), "").into()
        } else if left_type_id == PRELUDE_FLOAT_TYPE_ID && right_type_id == PRELUDE_INT_TYPE_ID {
            let right = self.builder.build_signed_int_to_float(right.into_int_value(), self.f64(), "");
            self.builder.build_float_compare(comp_op_float, left.into_float_value(), right, "").into()
        } else if left_type_id == PRELUDE_FLOAT_TYPE_ID && right_type_id == PRELUDE_FLOAT_TYPE_ID {
            self.builder.build_float_compare(comp_op_float, left.into_float_value(), right.into_float_value(), "").into()
        } else {
            let left_typeid = self.get_typeid_by_name(&left_type_name);
            let right_typeid = self.get_typeid_by_name(&right_type_name);

            if left_typeid != right_typeid {
                self.const_bool(false).into()
            } else {
                let resolved_generics = self.extend_resolved_generics_via_instance(resolved_generics, &left_type_id);
                let eq_method = self.get_or_compile_eq_method_for_type(&left_type_id, &resolved_generics);
                let eq_result = self.builder.build_call(eq_method, &[left.into(), right.into()], "").try_as_basic_value().left().unwrap().into_int_value();
                if negate {
                    self.builder.build_not(eq_result, "").into()
                } else {
                    eq_result.into()
                }
            }
        }
    }

    fn construct_string(&mut self, len_val: IntValue<'a>, str_val: PointerValue<'a>) -> BasicValueEnum<'a> {
        let string_initializer = self.get_or_compile_type_initializer(&self.project.prelude_string_struct_id, &ResolvedGenerics::default());
        self.builder.build_call(string_initializer, &[len_val.into(), str_val.into(), self.const_i16(0).into()], "").try_as_basic_value().left().unwrap()
    }

    fn destructure_string(&self, instance: PointerValue<'a>) -> (IntValue<'a>, PointerValue<'a>) {
        let len_val_ptr = self.builder.build_struct_gep(instance.const_cast(self.ptr(self.string_type)), 1, "").unwrap();
        let len_val = self.builder.build_load(len_val_ptr, "str.length").into_int_value();
        let chars_val_ptr = self.builder.build_struct_gep(instance.const_cast(self.ptr(self.string_type)), 2, "").unwrap();
        let chars_val = self.builder.build_load(chars_val_ptr, "str.chars").into_pointer_value();

        (len_val, chars_val)
    }

    fn visit_if_node(&mut self, if_node: &TypedNode, resolved_generics: &ResolvedGenerics) -> Option<BasicValueEnum<'a>> {
        let TypedNode::If { is_statement, condition, condition_binding, if_block, if_block_terminator, else_block, else_block_terminator, type_id, resolved_type_id, .. } = if_node else { unreachable!() };

        let then_bb = self.context.append_basic_block(self.current_fn.0, "then_block");
        let else_bb = self.context.append_basic_block(self.current_fn.0, "else_block");
        let end_bb = self.context.append_basic_block(self.current_fn.0, "if_end");

        let condition_type_id = *condition.type_id();
        let cond_is_opt = self.type_is_option(&condition_type_id).is_some();

        let cond_val = self.visit_expression(&condition, resolved_generics).unwrap();
        let (cond_val, opt_cond_local) = if cond_is_opt {
            let cond_local = self.builder.build_alloca(cond_val.get_type(), "");
            self.builder.build_store(cond_local, cond_val);
            let cond_val = self.option_instance_get_is_set(cond_local);
            (cond_val, Some(cond_local))
        } else {
            (cond_val.into_int_value(), None)
        };

        let cmp = self.builder.build_int_compare(IntPredicate::EQ, cond_val, self.const_bool(true), "");
        self.builder.build_conditional_branch(cmp, then_bb, else_bb);

        self.builder.position_at_end(then_bb);
        if let Some((condition_binding, condition_binding_var_ids)) = condition_binding {
            let expr_val = if let Some(opt_cond_local) = opt_cond_local {
                self.option_instance_get_value(opt_cond_local)
            } else {
                self.const_bool(true).into()
            };
            self.compile_binding_declaration(condition_binding, condition_binding_var_ids, Some(expr_val), resolved_generics);
        }
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
        if if_block_terminator.is_none() {
            self.builder.build_unconditional_branch(end_bb);
        }

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
        if else_block_terminator.is_none() {
            self.builder.build_unconditional_branch(end_bb);
        }

        self.builder.position_at_end(end_bb);

        if *is_statement {
            None
        } else {
            let Some(llvm_type) = self.llvm_underlying_type_by_id(type_id, resolved_generics) else { todo!() };
            let llvm_type = self.llvm_ptr_wrap_type_if_needed(llvm_type);

            debug_assert!(!(if_block_terminator.is_some() && else_block_terminator.is_some()), "In an if-expression, we cannot have both branches terminate");

            let phi = self.builder.build_phi(llvm_type, "");

            if if_block_terminator.is_some() {
                let else_block_value = else_block_value.unwrap_or_else(|| self.make_none_option_instance(type_id, &resolved_generics).into());

                phi.add_incoming(&[(&else_block_value, else_bb)]);
            } else if else_block_terminator.is_some() {
                let if_block_value = if_block_value.unwrap_or_else(|| self.make_none_option_instance(type_id, &resolved_generics).into());

                phi.add_incoming(&[(&if_block_value, then_bb)]);
            } else {
                let if_block_value = if_block_value.unwrap_or_else(|| self.make_none_option_instance(type_id, &resolved_generics).into());
                let else_block_value = else_block_value.unwrap_or_else(|| self.make_none_option_instance(type_id, &resolved_generics).into());

                phi.add_incoming(&[(&if_block_value, then_bb), (&else_block_value, else_bb)]);
            };

            let phi_value = phi.as_basic_value();

            if resolved_type_id == &PRELUDE_ANY_TYPE_ID {
                let resolved_generics = resolved_generics.clone();
                return Some(self.make_trait_instance(&PRELUDE_ANY_TYPE_ID, &type_id, phi_value, &resolved_generics).into());
            } else {
                Some(phi_value)
            }
        }
    }

    fn visit_match_node(&mut self, if_node: &TypedNode, resolved_generics: &ResolvedGenerics) -> Option<BasicValueEnum<'a>> {
        let TypedNode::Match { is_statement, target, cases, type_id, .. } = if_node else { unreachable!() };

        let target_type_id = target.type_id();
        let resolved_generics = self.extend_resolved_generics_via_instance(resolved_generics, target_type_id);
        let target_value = self.visit_expression(target, &resolved_generics).unwrap();
        let target_local = self.builder.build_alloca(target_value.get_type(), "match_target_local");
        self.builder.build_store(target_local, target_value);

        let match_target_is_option_type = self.type_is_option(target_type_id).is_some();
        let (data_is_set, data) = if match_target_is_option_type {
            (self.option_instance_get_is_set(target_local), self.option_instance_get_value(target_local))
        } else {
            (self.const_bool(true), target_value)
        };

        let end_bb = self.context.append_basic_block(self.current_fn.0, "match_end");

        let result_slot = if !*is_statement {
            let Some(llvm_type) = self.llvm_underlying_type_by_id(type_id, &resolved_generics) else { todo!() };
            let llvm_type = self.llvm_ptr_wrap_type_if_needed(llvm_type);

            let result = self.builder.build_alloca(llvm_type, "match_result");
            self.builder.build_store(result, llvm_type.const_zero());
            Some(result)
        } else {
            None
        };

        let mut seen_none_case = false;
        let mut seen_wildcard_case = false;
        for case in cases {
            match &case.kind {
                TypedMatchCaseKind::None => {
                    seen_none_case = true;
                    let is_none_bb = self.context.append_basic_block(self.current_fn.0, "is_none");
                    let next_case_bb = self.context.append_basic_block(self.current_fn.0, "next_case");
                    self.builder.build_conditional_branch(data_is_set, next_case_bb, is_none_bb);

                    self.builder.position_at_end(is_none_bb);
                    if let Some(var_id) = &case.case_binding {
                        let expr_val = target_value;
                        let var = self.project.get_var_by_id(var_id);
                        let pat = BindingPattern::Variable(Token::Ident(var.defined_span.as_ref().unwrap().range.start.clone(), var.name.clone()));
                        self.compile_binding_declaration(&pat, &vec![*var_id], Some(expr_val), &resolved_generics);
                    }

                    let mut case_value = None;
                    let case_body_len = case.body.len();
                    for (idx, node) in case.body.iter().enumerate() {
                        if idx == case_body_len - 1 {
                            case_value = self.visit_statement(node, &resolved_generics);
                        } else {
                            self.visit_statement(node, &resolved_generics);
                        }
                    }
                    if case.block_terminator.is_none() {
                        if let Some(result_slot) = result_slot {
                            let case_value = case_value.expect("If we're able to treat the match as an expression, then the resulting value exists");
                            self.builder.build_store(result_slot, case_value);
                        }
                        self.builder.build_unconditional_branch(end_bb);
                    }

                    self.builder.position_at_end(next_case_bb);
                }
                TypedMatchCaseKind::Wildcard(_) => {
                    seen_wildcard_case = true;
                    if let Some(var_id) = &case.case_binding {
                        let expr_val = if match_target_is_option_type && seen_none_case { data } else { target_value };
                        let var = self.project.get_var_by_id(var_id);
                        let pat = BindingPattern::Variable(Token::Ident(var.defined_span.as_ref().unwrap().range.start.clone(), var.name.clone()));
                        self.compile_binding_declaration(&pat, &vec![*var_id], Some(expr_val), &resolved_generics);
                    }

                    let mut case_value = None;
                    let case_body_len = case.body.len();
                    for (idx, node) in case.body.iter().enumerate() {
                        if idx == case_body_len - 1 {
                            case_value = self.visit_statement(node, &resolved_generics);
                        } else {
                            self.visit_statement(node, &resolved_generics);
                        }
                    }
                    if case.block_terminator.is_none() {
                        if let Some(result_slot) = result_slot {
                            let case_value = case_value.expect("If we're able to treat the match as an expression, then the resulting value exists");
                            self.builder.build_store(result_slot, case_value);
                        }
                        self.builder.build_unconditional_branch(end_bb);
                    }
                }
                TypedMatchCaseKind::Type(type_id, args) => {
                    debug_assert!(args.iter().all(|a| matches!(a, TypedMatchCaseArgument::Pattern(_, _))), "Literal args not yet implemented");

                    let next_case_bb = self.context.append_basic_block(self.current_fn.0, "next_case");

                    if match_target_is_option_type && !seen_none_case {
                        let cont_bb = self.context.append_basic_block(self.current_fn.0, "cont");

                        self.builder.build_conditional_branch(data_is_set, cont_bb, next_case_bb);
                        self.builder.position_at_end(cont_bb);
                    }
                    let case_type_name = self.llvm_type_name_by_id(type_id, &resolved_generics);
                    let case_typeid = if let Type::GenericEnumInstance(enum_id, _, variant_idx) = self.get_type_by_id(type_id) {
                        let enum_ = self.project.get_enum_by_id(&enum_id);
                        let variant = &enum_.variants[variant_idx.unwrap()];
                        let enum_variant_name = self.llvm_enum_variant_type_name(&case_type_name, &variant.name);
                        self.get_typeid_by_name(&enum_variant_name)
                    } else {
                        self.get_typeid_by_name(&case_type_name)
                    };
                    let case_typeid_val = self.const_i64(case_typeid as u64);
                    let (typeid_val, intermediate_local) = self.get_typeid_from_value(data, None);

                    let is_same_type_bb = self.context.append_basic_block(self.current_fn.0, "is_same_type");
                    let cond = self.builder.build_int_compare(IntPredicate::EQ, typeid_val, case_typeid_val, "typeid_matches");
                    self.builder.build_conditional_branch(cond, is_same_type_bb, next_case_bb);

                    self.builder.position_at_end(is_same_type_bb);
                    if let Some(var_id) = &case.case_binding {
                        let expr_val = data;
                        let var = self.project.get_var_by_id(var_id);
                        let pat = BindingPattern::Variable(Token::Ident(var.defined_span.as_ref().unwrap().range.start.clone(), var.name.clone()));
                        self.compile_binding_declaration(&pat, &vec![*var_id], Some(expr_val), &resolved_generics);
                    }

                    if !args.is_empty() {
                        if let Type::GenericEnumInstance(enum_id, _, variant_idx) = self.get_type_by_id(&type_id) {
                            let variant_idx = variant_idx.expect("Match cases cannot be top-level enum types; they must be variants");
                            let enum_ = self.project.get_enum_by_id(&enum_id);
                            let enum_type_name = self.llvm_type_name_by_id(&type_id, &resolved_generics);
                            let variant = &enum_.variants[variant_idx];
                            let EnumVariantKind::Container(func_id) = &variant.kind else { unreachable!("Only tagged union variants can be destructured") };
                            let params = &self.project.get_func_by_id(func_id).params;
                            let enum_data = self.extract_tagged_union_enum_variant_data(intermediate_local.unwrap(), &enum_id, &enum_type_name, variant_idx, &resolved_generics);

                            for (idx, (arg, param)) in args.iter().zip(params).enumerate() {
                                let TypedMatchCaseArgument::Pattern(pattern, var_ids) = arg else { todo!() };

                                let slot = self.builder.build_struct_gep(enum_data, idx as u32, &format!("{}_slot", &param.name)).unwrap();
                                let value = self.builder.build_load(slot, &format!("{}_value", &param.name));
                                self.compile_binding_declaration(pattern, var_ids, Some(value), &resolved_generics);
                            }
                        } else {
                            todo!()
                        };
                    }

                    let mut case_value = None;
                    let case_body_len = case.body.len();
                    for (idx, node) in case.body.iter().enumerate() {
                        if idx == case_body_len - 1 {
                            case_value = self.visit_statement(node, &resolved_generics);
                        } else {
                            self.visit_statement(node, &resolved_generics);
                        }
                    }
                    if case.block_terminator.is_none() {
                        if let Some(result_slot) = result_slot {
                            let case_value = case_value.expect("If we're able to treat the match as an expression, then the resulting value exists");
                            self.builder.build_store(result_slot, case_value);
                        }
                        self.builder.build_unconditional_branch(end_bb);
                    }

                    self.builder.position_at_end(next_case_bb);
                }
                TypedMatchCaseKind::Constant(constant_node_type_id, constant_node) => {
                    let next_case_bb = self.context.append_basic_block(self.current_fn.0, "next_case");

                    let target_type_id = if let Some(inner_type_id) = self.type_is_option(target_type_id) {
                        if !seen_none_case {
                            let cont_bb = self.context.append_basic_block(self.current_fn.0, "cont");

                            self.builder.build_conditional_branch(data_is_set, cont_bb, next_case_bb);
                            self.builder.position_at_end(cont_bb);
                        }

                        inner_type_id
                    } else {
                        *target_type_id
                    };

                    let is_eq_bb = self.context.append_basic_block(self.current_fn.0, "is_eq");
                    let constant_value = self.visit_expression(constant_node, &resolved_generics).unwrap();
                    let eq = self.compile_eq(false, &target_type_id, data, constant_node_type_id, constant_value, &resolved_generics);
                    self.builder.build_conditional_branch(eq, is_eq_bb, next_case_bb);

                    self.builder.position_at_end(is_eq_bb);
                    if let Some(var_id) = &case.case_binding {
                        let expr_val = data;
                        let var = self.project.get_var_by_id(var_id);
                        let pat = BindingPattern::Variable(Token::Ident(var.defined_span.as_ref().unwrap().range.start.clone(), var.name.clone()));
                        self.compile_binding_declaration(&pat, &vec![*var_id], Some(expr_val), &resolved_generics);
                    }

                    let mut case_value = None;
                    let case_body_len = case.body.len();
                    for (idx, node) in case.body.iter().enumerate() {
                        if idx == case_body_len - 1 {
                            case_value = self.visit_statement(node, &resolved_generics);
                        } else {
                            self.visit_statement(node, &resolved_generics);
                        }
                    }
                    if case.block_terminator.is_none() {
                        if let Some(result_slot) = result_slot {
                            let case_value = case_value.expect("If we're able to treat the match as an expression, then the resulting value exists");
                            self.builder.build_store(result_slot, case_value);
                        }
                        self.builder.build_unconditional_branch(end_bb);
                    }

                    self.builder.position_at_end(next_case_bb);
                }
            }
        }

        if !seen_wildcard_case {
            self.builder.build_unconditional_branch(end_bb);
        }

        self.builder.position_at_end(end_bb);

        if let Some(result_slot) = result_slot {
            let result = self.builder.build_load(result_slot, "match_result_final");
            Some(result)
        } else {
            None
        }
    }

    fn make_option_instance<V: BasicValue<'a>>(&mut self, outer_type_id: &TypeId, value: V, resolved_generics: &ResolvedGenerics) -> StructValue<'a> {
        let Type::GenericInstance(struct_id, generics) = self.get_type_by_id(outer_type_id) else { unreachable!() };
        debug_assert!(struct_id == self.project.prelude_option_struct_id);

        let inner_type_id = resolved_generics.resolve_if_generic(&generics[0], &self.project)
            .map(|resolved| resolved.type_id)
            .unwrap_or(generics[0]);
        let opt_struct = self.project.get_struct_by_id(&self.project.prelude_option_struct_id);
        let inner_resolved = self.make_resolved_generic(&inner_type_id, resolved_generics);
        let resolved_generics = resolved_generics.extend_via_pairs(vec![(opt_struct.generic_ids[0], inner_resolved)]);
        let Some(llvm_type) = self.llvm_underlying_type_by_id(outer_type_id, &resolved_generics) else { todo!() };

        let llvm_type_name = self.llvm_type_name_by_id(outer_type_id, &resolved_generics);
        debug_assert!(llvm_type.into_struct_type().get_name().unwrap().to_str().unwrap() == &llvm_type_name);
        let runtime_typeid = self.get_typeid_by_name(&llvm_type_name);

        let instance_ptr = self.builder.build_alloca(llvm_type, "opt_instance_ptr");
        let typeid_slot = self.builder.build_struct_gep(instance_ptr, 0, "typeid_slot").unwrap();
        self.builder.build_store(typeid_slot, self.const_i32(runtime_typeid as u64));
        let is_set_slot = self.builder.build_struct_gep(instance_ptr, 1, "is_set_slot").unwrap();
        self.builder.build_store(is_set_slot, self.const_bool(true));
        let value_slot = self.builder.build_struct_gep(instance_ptr, 2, "value_slot").unwrap();
        self.builder.build_store(value_slot, value.as_basic_value_enum());

        self.builder.build_load(instance_ptr, "opt_instance").into_struct_value()
    }

    fn option_instance_get_is_set(&self, option_instance_ptr: PointerValue<'a>) -> IntValue<'a> {
        let is_set_slot = self.builder.build_struct_gep(option_instance_ptr, 1, "is_set_slot").unwrap();
        self.builder.build_load(is_set_slot, "is_set").into_int_value()
    }

    fn option_instance_get_value(&self, option_instance_ptr: PointerValue<'a>) -> BasicValueEnum<'a> {
        let value_slot = self.builder.build_struct_gep(option_instance_ptr, 2, "value_slot").unwrap();
        self.builder.build_load(value_slot, "value_set")
    }

    fn make_none_option_instance(&mut self, outer_type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> StructValue<'a> {
        let Type::GenericInstance(struct_id, generics) = self.get_type_by_id(outer_type_id) else { unreachable!() };
        debug_assert!(struct_id == self.project.prelude_option_struct_id);
        let inner_type_id = resolved_generics.resolve_if_generic(&generics[0], &self.project)
            .map(|resolved| resolved.type_id)
            .unwrap_or(generics[0]);

        let opt_struct = self.project.get_struct_by_id(&self.project.prelude_option_struct_id);
        let inner_resolved = self.make_resolved_generic(&inner_type_id, resolved_generics);
        let resolved_generics = resolved_generics.extend_via_pairs(vec![(opt_struct.generic_ids[0], inner_resolved)]);
        let Some(llvm_type) = self.llvm_underlying_type_by_id(outer_type_id, &resolved_generics) else { todo!() };

        llvm_type.into_struct_type().const_zero()
    }

    fn make_trait_instance<V: BasicValue<'a>>(&mut self, trait_type_id: &TypeId, value_type_id: &TypeId, value: V, resolved_generics: &ResolvedGenerics) -> StructValue<'a> {
        debug_assert!(trait_type_id == &PRELUDE_ANY_TYPE_ID, "When other trait implementations come along, this implementation will need to be made generic");

        let trait_type_name = self.llvm_type_name_by_id(trait_type_id, resolved_generics);
        let (trait_type, vtable_struct_type) = self.get_or_make_trait_struct_type(trait_type_id, resolved_generics);

        let trait_instance = self.builder.build_alloca(trait_type, "trait_instance_ptr");

        let (value_type_id, value_type_name) = resolved_generics.resolve_if_generic(value_type_id, &self.project)
            .map(|resolved| (resolved.type_id, resolved.llvm_type_name.clone()))
            .unwrap_or_else(|| {
                let value_type_name = self.llvm_type_name_by_id(value_type_id, resolved_generics);
                (*value_type_id, value_type_name)
            });

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
                let value = if value_type_id == PRELUDE_INT_TYPE_ID {
                    raw_value.as_basic_value_enum()
                } else if value_type_id == PRELUDE_FLOAT_TYPE_ID {
                    self.builder.build_cast(InstructionOpcode::BitCast, raw_value, self.f64(), "").as_basic_value_enum()
                } else if value_type_id == PRELUDE_BOOL_TYPE_ID {
                    self.builder.build_cast(InstructionOpcode::Trunc, raw_value, self.bool(), "").as_basic_value_enum()
                } else {
                    let Some(value_type) = self.llvm_underlying_type_by_id(&value_type_id, resolved_generics) else { todo!() };
                    let value_type = self.llvm_ptr_wrap_type_if_needed(value_type);

                    if self.llvm_type_name_by_id(&value_type_id, resolved_generics).starts_with(ENUM_TYPENAME_TAG) {
                        let local = self.builder.build_alloca(value_type, "enum_instance_local");
                        self.builder.build_store(
                            self.builder.build_struct_gep(local, 0, "").unwrap(),
                            raw_value,
                        );
                        self.builder.build_load(local, "")
                    } else if self.type_is_option(&value_type_id).is_some() {
                        let ptr = self.builder.build_int_to_ptr(raw_value, self.ptr(value_type), "self");
                        self.builder.build_load(ptr, "self").as_basic_value_enum()
                    } else {
                        self.builder.build_int_to_ptr(raw_value, value_type.into_pointer_type(), "self").as_basic_value_enum()
                    }
                };

                let tostring_llvm_fn = self.get_or_compile_to_string_method_for_type(&value_type_id, resolved_generics);
                let tostring_ret = self.builder.build_call(tostring_llvm_fn, &[value.into()], "").try_as_basic_value().left().unwrap();
                self.builder.build_return(Some(&tostring_ret));

                self.builder.position_at_end(prev_bb);

                wrapper_llvm_fn.as_global_value().as_pointer_value()
            };

            let hash_fn_ptr = {
                let prev_bb = self.builder.get_insert_block().unwrap();

                let hash_fn_type = self.fn_type(self.i64(), &[self.i64().into()]);
                let wrapper_llvm_fn = self.main_module.add_function(&format!("{value_type_name}@Any#hash(ValWrapper):Int"), hash_fn_type, None);
                let block = self.context.append_basic_block(wrapper_llvm_fn, "");
                self.builder.position_at_end(block);

                let raw_value = wrapper_llvm_fn.get_nth_param(0).unwrap().into_int_value();
                let value = if value_type_id == PRELUDE_INT_TYPE_ID {
                    raw_value.as_basic_value_enum()
                } else if value_type_id == PRELUDE_FLOAT_TYPE_ID {
                    self.builder.build_cast(InstructionOpcode::BitCast, raw_value, self.f64(), "").as_basic_value_enum()
                } else if value_type_id == PRELUDE_BOOL_TYPE_ID {
                    self.builder.build_cast(InstructionOpcode::Trunc, raw_value, self.bool(), "").as_basic_value_enum()
                } else {
                    let Some(value_type) = self.llvm_underlying_type_by_id(&value_type_id, resolved_generics) else { todo!() };
                    let value_type = self.llvm_ptr_wrap_type_if_needed(value_type);

                    if self.llvm_type_name_by_id(&value_type_id, resolved_generics).starts_with(ENUM_TYPENAME_TAG) {
                        let local = self.builder.build_alloca(value_type, "enum_instance_local");
                        self.builder.build_store(
                            self.builder.build_struct_gep(local, 0, "").unwrap(),
                            raw_value,
                        );
                        self.builder.build_load(local, "")
                    } else if self.type_is_option(&value_type_id).is_some() || self.llvm_type_name_by_id(&value_type_id, resolved_generics).starts_with(ENUM_TYPENAME_TAG) {
                        let ptr = self.builder.build_int_to_ptr(raw_value, self.ptr(value_type), "self");
                        self.builder.build_load(ptr, "self").as_basic_value_enum()
                    } else {
                        self.builder.build_int_to_ptr(raw_value, value_type.into_pointer_type(), "self").as_basic_value_enum()
                    }
                };

                let hash_llvm_fn = self.get_or_compile_hash_method_for_type(&value_type_id, resolved_generics);
                let hash_ret = self.builder.build_call(hash_llvm_fn, &[value.into()], "").try_as_basic_value().left().unwrap();
                self.builder.build_return(Some(&hash_ret));

                self.builder.position_at_end(prev_bb);

                wrapper_llvm_fn.as_global_value().as_pointer_value()
            };

            global.set_initializer(&self.context.const_struct(&[
                self.const_i32(value_type_id.1 as u64).into(),
                tostring_fn_ptr.into(),
                hash_fn_ptr.into(),
            ], false));
            global
        };
        let vtable_slot = self.builder.build_struct_gep(trait_instance, 0, "vtable_slot").unwrap();
        self.builder.build_store(vtable_slot, vtable_global.as_pointer_value());

        let value_slot = self.builder.build_struct_gep(trait_instance, 1, "value_slot").unwrap();
        if value_type_id == PRELUDE_INT_TYPE_ID {
            self.builder.build_store(value_slot, value);
        } else if value_type_id == PRELUDE_FLOAT_TYPE_ID {
            let value = self.builder.build_cast(InstructionOpcode::BitCast, value, self.i64(), "float_as_value");
            self.builder.build_store(value_slot, value);
        } else if value_type_id == PRELUDE_BOOL_TYPE_ID {
            let value = self.builder.build_int_cast(value.as_basic_value_enum().into_int_value(), self.i64(), "bool_as_value");
            self.builder.build_store(value_slot, value);
        } else if self.llvm_type_name_by_id(&value_type_id, resolved_generics).starts_with(ENUM_TYPENAME_TAG) {
            let local = self.builder.build_alloca(value.as_basic_value_enum().get_type(), "enum_instance_local");
            self.builder.build_store(local, value);
            let enum_instance_value = self.builder.build_load(self.builder.build_struct_gep(local, 0, "").unwrap(), "");
            self.builder.build_store(value_slot, enum_instance_value);
        } else {
            // Otherwise, value should be treated a pointer
            let ptr = if self.type_is_option(&value_type_id).is_some() {
                let option_struct_llvm_type = value.as_basic_value_enum().get_type();
                let value_local = self.builder.build_alloca(option_struct_llvm_type, "");
                self.builder.build_store(value_local, value);

                let mem = self.malloc(self.sizeof_struct(option_struct_llvm_type), self.ptr(option_struct_llvm_type));
                let typeid_slot = self.builder.build_struct_gep(mem, 0, "typeid_slot").unwrap();
                self.builder.build_store(
                    typeid_slot,
                    self.builder.build_load(self.builder.build_struct_gep(value_local, 0, "").unwrap(), "typeid_orig"),
                );
                let is_set_slot = self.builder.build_struct_gep(mem, 1, "is_set_slot").unwrap();
                self.builder.build_store(
                    is_set_slot,
                    self.builder.build_load(self.builder.build_struct_gep(value_local, 1, "").unwrap(), "is_set_orig"),
                );
                let value_slot = self.builder.build_struct_gep(mem, 2, "value_slot").unwrap();
                self.builder.build_store(
                    value_slot,
                    self.builder.build_load(self.builder.build_struct_gep(value_local, 2, "").unwrap(), "value_orig"),
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
            let pointer_type_id = if let Some(inner_type_id) = self.type_is_option(pointer_type_id) {
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

                let Type::GenericInstance(_, generics) = self.get_type_by_id(instance_node.type_id()) else { unreachable!() };
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
                let arg_value_len_ptr = self.builder.build_struct_gep(arg_value, 1, "len_slot").unwrap();
                let arg_value_len = self.builder.build_load(arg_value_len_ptr, "len");
                let arg_value_chars_ptr = self.builder.build_struct_gep(arg_value, 2, "chars_slot").unwrap();
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
            "byte_as_int" => { // Instance method
                let instance_node = implicit_argument.expect("Byte#asInt is an instance method and will have an implicit argument");
                let i8_val = self.visit_expression(instance_node, resolved_generics).unwrap().into_int_value();
                let i64_val = self.builder.build_int_cast(i8_val, self.i64(), "");
                i64_val.as_basic_value_enum()
            }
            _ => unimplemented!("Unimplemented intrinsic '{}'", name),
        };

        if self.type_is_option(resolved_type_id).is_some() {
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

            let hash_fn_type = self.fn_type(self.i64(), &[self.i64().into()]);
            let hash_fn_ptr_type = hash_fn_type.ptr_type(AddressSpace::Generic);

            self.context.struct_type(&[
                self.i32().into(), // type_id
                tostring_fn_ptr_type.into(), // toString_wrapper fn pointer
                hash_fn_ptr_type.into(), // hash_wrapper fn pointer
            ], false)
        } else {
            unimplemented!("No traits other than `Any` are implemented yet")
        }
    }

    fn get_or_compile_to_string_method_for_type(&mut self, type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        if let Some(inner_type_id) = self.type_is_option(type_id) {
            self.get_or_compile_option_method(resolved_generics, &inner_type_id, &METHOD_IDX_TOSTRING)
        } else if self.type_is_tuple(type_id).is_some() {
            self.get_or_compile_tuple_method(&type_id, resolved_generics, &METHOD_IDX_TOSTRING)
        } else {
            let value_ty = self.get_type_by_id(type_id);
            let (_, tostring_func_id) = value_ty.find_method_by_name(self.project, "toString").unwrap();
            self.get_or_compile_function(tostring_func_id, resolved_generics)
        }
    }

    fn get_or_compile_hash_method_for_type(&mut self, type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        if let Some(inner_type_id) = self.type_is_option(type_id) {
            self.get_or_compile_option_method(resolved_generics, &inner_type_id, &METHOD_IDX_HASH)
        } else if self.type_is_tuple(type_id).is_some() {
            self.get_or_compile_tuple_method(&type_id, resolved_generics, &METHOD_IDX_HASH)
        } else {
            let value_ty = self.get_type_by_id(type_id);
            let (_, hash_func_id) = value_ty.find_method_by_name(self.project, "hash").unwrap();
            self.get_or_compile_function(hash_func_id, resolved_generics)
        }
    }

    fn get_or_compile_eq_method_for_type(&mut self, type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        if let Some(inner_type_id) = self.type_is_option(type_id) {
            self.get_or_compile_option_method(resolved_generics, &inner_type_id, &METHOD_IDX_EQ)
        } else if self.type_is_tuple(type_id).is_some() {
            self.get_or_compile_tuple_method(&type_id, resolved_generics, &METHOD_IDX_EQ)
        } else {
            let value_ty = self.get_type_by_id(type_id);
            let (_, eq_func_id) = value_ty.find_method_by_name(self.project, "eq").unwrap();
            self.get_or_compile_function(eq_func_id, resolved_generics)
        }
    }

    fn get_or_compile_function(&mut self, func_id: &FuncId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let fn_sig = self.llvm_function_signature(func_id, &resolved_generics);
        let llvm_fn = if let Some(function_val) = self.main_module.get_function(&fn_sig) {
            function_val
        } else {
            let function = self.project.get_func_by_id(func_id);
            let function_val = if function.body.is_empty() && matches!(function.kind, FunctionKind::Method(_)) {
                match function.name.as_str() {
                    "toString" => self.compile_to_string_method(&func_id, &resolved_generics),
                    "hash" => self.compile_hash_method(&func_id, &resolved_generics),
                    "eq" => self.compile_eq_method(&func_id, &resolved_generics),
                    _ => unreachable!("Method '{}' without body", &function.name)
                }
            } else {
                self.compile_function(&func_id, &resolved_generics)
            };
            debug_assert!(function_val.get_name().to_str().unwrap() == &fn_sig);
            function_val
        };

        llvm_fn
    }

    fn get_or_compile_option_method(&mut self, resolved_generics: &ResolvedGenerics, inner_type_id: &TypeId, member_idx: &usize) -> FunctionValue<'a> {
        let resolved_generics = self.extend_resolved_generics_via_instance(resolved_generics, inner_type_id);

        match *member_idx {
            METHOD_IDX_TOSTRING => self.get_or_compile_option_tostring_method(inner_type_id, &resolved_generics),
            METHOD_IDX_HASH => self.get_or_compile_option_hash_method(inner_type_id, &resolved_generics),
            METHOD_IDX_EQ => self.get_or_compile_option_eq_method(inner_type_id, &resolved_generics),
            _ => unreachable!("Method idx {} for option instance", *member_idx),
        }
    }

    fn get_or_compile_option_tostring_method(&mut self, inner_type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let type_name = format!("Option<{}>", self.llvm_type_name_by_id(inner_type_id, &resolved_generics));
        let fn_sig = format!("{}#toString({}):String", &type_name, &type_name);

        if let Some(llvm_fn) = self.main_module.get_function(&fn_sig) {
            return llvm_fn;
        }

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

        let is_set_val = self.option_instance_get_is_set(self_var);
        let cond = self.builder.build_int_compare(IntPredicate::NE, is_set_val, self.const_bool(true), "cond");
        self.builder.build_conditional_branch(cond, is_none_block, is_some_block);

        self.builder.position_at_end(is_none_block);
        let is_none_value = self.main_module.get_global("NONE_STRING_VALUE").unwrap_or_else(|| {
            let global = self.main_module.add_global(self.string_type, None, "NONE_STRING_VALUE");
            global.set_constant(true);
            let str = self.builder.build_global_string_ptr("None", "").as_basic_value_enum();
            global.set_initializer(&self.context.const_struct(&[self.const_i32(RUNTIME_TYPEID_STRING as u64).into(), self.const_i64(4).into(), str], false));
            global
        }).as_pointer_value();
        self.builder.build_unconditional_branch(end_block);

        self.builder.position_at_end(is_some_block);
        let value_val = self.option_instance_get_value(self_var);
        let inner_tostring_llvm_fn = self.get_or_compile_to_string_method_for_type(inner_type_id, resolved_generics);
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

    fn get_or_compile_option_hash_method(&mut self, inner_type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let type_name = format!("Option<{}>", self.llvm_type_name_by_id(inner_type_id, &resolved_generics));
        let fn_sig = format!("{}#hash({}):Int", &type_name, &type_name);

        if let Some(llvm_fn) = self.main_module.get_function(&fn_sig) {
            return llvm_fn;
        }

        let fn_type = self.fn_type(self.i64(), &[self.main_module.get_struct_type(&type_name).unwrap().into()]);
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

        let is_set_val = self.option_instance_get_is_set(self_var);
        let cond = self.builder.build_int_compare(IntPredicate::NE, is_set_val, self.const_bool(true), "cond");
        self.builder.build_conditional_branch(cond, is_none_block, is_some_block);

        self.builder.position_at_end(is_none_block);
        let is_none_value = self.const_i64(0);
        self.builder.build_unconditional_branch(end_block);

        self.builder.position_at_end(is_some_block);
        let value_val = self.option_instance_get_value(self_var);
        let inner_type_hash_fn = self.get_or_compile_hash_method_for_type(inner_type_id, resolved_generics);
        let is_some_value = self.builder.build_call(inner_type_hash_fn, &[value_val.into()], "").try_as_basic_value().left().unwrap().into_int_value();
        self.builder.build_unconditional_branch(end_block);

        self.builder.position_at_end(end_block);

        let phi = self.builder.build_phi(self.i64(), "");
        phi.add_incoming(&[(&is_none_value, is_none_block), (&is_some_value, is_some_block)]);
        self.builder.build_return(Some(&phi.as_basic_value()));

        self.ctx_stack.pop();
        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn get_or_compile_option_eq_method(&mut self, inner_type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let type_name = format!("Option<{}>", self.llvm_type_name_by_id(inner_type_id, &resolved_generics));
        let fn_sig = format!("{}#eq({},{}):Bool", &type_name, &type_name, &type_name);

        if let Some(llvm_fn) = self.main_module.get_function(&fn_sig) {
            return llvm_fn;
        }

        let llvm_type = self.main_module.get_struct_type(&type_name).unwrap();
        let fn_type = self.fn_type(self.bool(), &[llvm_type.into(), llvm_type.into()]);
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
        let other_ = llvm_fn.get_nth_param(1).unwrap();
        let other_var = self.builder.build_alloca(other_.get_type(), "other");
        self.builder.build_store(other_var, other_);

        let self_is_set_val = self.option_instance_get_is_set(self_var);
        let other_is_set_val = self.option_instance_get_is_set(other_var);
        let both_are_set = self.builder.build_and(self_is_set_val, other_is_set_val, "both_are_set");

        let both_set_bb = self.context.append_basic_block(llvm_fn, "both_are_set_bb");
        let else_bb = self.context.append_basic_block(llvm_fn, "else");
        let end_bb = self.context.append_basic_block(llvm_fn, "end");

        self.builder.build_conditional_branch(both_are_set, both_set_bb, else_bb);

        self.builder.position_at_end(both_set_bb);
        let self_val = self.option_instance_get_value(self_var);
        let other_val = self.option_instance_get_value(other_var);
        let both_set_value = self.compile_eq(false, inner_type_id, self_val, inner_type_id, other_val, resolved_generics);
        let both_set_bb = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(end_bb);

        self.builder.position_at_end(else_bb);
        let else_value = self.builder.build_and(
            self.builder.build_not(self_is_set_val, ""),
            self.builder.build_not(other_is_set_val, ""),
            "both_are_unset",
        );
        self.builder.build_unconditional_branch(end_bb);

        self.builder.position_at_end(end_bb);

        let phi = self.builder.build_phi(self.bool(), "");
        phi.add_incoming(&[(&both_set_value, both_set_bb), (&else_value, else_bb)]);
        self.builder.build_return(Some(&phi.as_basic_value()));

        self.ctx_stack.pop();
        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn get_or_compile_tuple_method(&mut self, type_id: &TypeId, resolved_generics: &ResolvedGenerics, member_idx: &usize) -> FunctionValue<'a> {
        let Type::GenericInstance(struct_id, generic_ids) = self.get_type_by_id(type_id) else { unreachable!(); };
        debug_assert!(struct_id == self.project.prelude_tuple_struct_id);

        let resolved_generics = self.extend_resolved_generics_via_instance(resolved_generics, type_id);

        match *member_idx {
            METHOD_IDX_TOSTRING => self.get_or_compile_tuple_tostring_method(type_id, &generic_ids, &resolved_generics),
            METHOD_IDX_HASH => self.get_or_compile_tuple_hash_method(type_id, &generic_ids, &resolved_generics),
            METHOD_IDX_EQ => self.get_or_compile_tuple_eq_method(type_id, &generic_ids, &resolved_generics),
            _ => unreachable!("Method idx {} for option instance", *member_idx),
        }
    }

    fn get_or_compile_tuple_tostring_method(&mut self, type_id: &TypeId, generic_ids: &Vec<TypeId>, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let type_name = self.llvm_type_name_by_id(type_id, &resolved_generics);
        let fn_sig = format!("{}#toString({}):String", &type_name, &type_name);

        if let Some(llvm_fn) = self.main_module.get_function(&fn_sig) {
            return llvm_fn;
        }

        let Some(llvm_type) = self.llvm_underlying_type_by_id(type_id, &resolved_generics) else { todo!() };
        let fn_type = self.fn_type(self.ptr(self.string_type), &[self.ptr(llvm_type).into()]);
        let llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, None);
        self.ctx_stack.push(CompilerContext { variables: HashMap::new() });

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let self_val = llvm_fn.get_nth_param(0).unwrap().into_pointer_value();
        let data_spec = generic_ids.iter().enumerate().map(|(idx, generic_id)| {
            let name = format!("tuple_item_{}", idx);
            let item_val_slot = self.builder.build_struct_gep(self_val, (idx + 1) as u32, &format!("{name}_slot")).unwrap();

            (*generic_id, name, item_val_slot)
        });
        let ret_val = self.generate_tostring_logic_for_structured_data(&"".to_string(), false, &data_spec.collect(), resolved_generics);
        self.builder.build_return(Some(&ret_val));

        self.ctx_stack.pop();
        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn get_or_compile_tuple_hash_method(&mut self, type_id: &TypeId, generic_ids: &Vec<TypeId>, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let type_name = self.llvm_type_name_by_id(type_id, &resolved_generics);
        let fn_sig = format!("{}#hash({}):Int", &type_name, &type_name);

        if let Some(llvm_fn) = self.main_module.get_function(&fn_sig) {
            return llvm_fn;
        }

        let Some(llvm_type) = self.llvm_underlying_type_by_id(type_id, &resolved_generics) else { todo!() };
        let fn_type = self.fn_type(self.i64(), &[self.ptr(llvm_type).into()]);
        let llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, None);
        self.ctx_stack.push(CompilerContext { variables: HashMap::new() });

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let self_val = llvm_fn.get_nth_param(0).unwrap().into_pointer_value();
        let data_spec = generic_ids.iter().enumerate().map(|(idx, generic_id)| {
            let name = format!("tuple_item_{}", idx);
            let item_val_slot = self.builder.build_struct_gep(self_val, (idx + 1) as u32, &format!("{name}_slot")).unwrap();

            (*generic_id, name, item_val_slot)
        });
        let ret_val = self.generate_hash_logic_for_structured_data(&data_spec.collect(), resolved_generics);
        self.builder.build_return(Some(&ret_val));

        self.ctx_stack.pop();
        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn get_or_compile_tuple_eq_method(&mut self, type_id: &TypeId, generic_ids: &Vec<TypeId>, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let type_name = self.llvm_type_name_by_id(type_id, &resolved_generics);
        let fn_sig = format!("{}#eq({},{}):Bool", &type_name, &type_name, &type_name);

        if let Some(llvm_fn) = self.main_module.get_function(&fn_sig) {
            return llvm_fn;
        }

        let Some(llvm_type) = self.llvm_underlying_type_by_id(type_id, &resolved_generics) else { todo!() };
        let fn_type = self.fn_type(self.bool(), &[self.ptr(llvm_type).into(), self.ptr(llvm_type).into()]);
        let llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, None);
        self.ctx_stack.push(CompilerContext { variables: HashMap::new() });

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let self_val = llvm_fn.get_nth_param(0).unwrap().into_pointer_value();
        let other_val = llvm_fn.get_nth_param(1).unwrap().into_pointer_value();
        let data_spec = generic_ids.iter().enumerate().map(|(idx, generic_id)| {
            let name = format!("tuple_item_{}", idx);
            let item_val_slot = self.builder.build_struct_gep(self_val, (idx + 1) as u32, &format!("{name}_slot")).unwrap();
            let other_item_val_slot = self.builder.build_struct_gep(other_val, (idx + 1) as u32, &format!("other_item_{}_slot", idx)).unwrap();

            (*generic_id, name, item_val_slot, other_item_val_slot)
        });
        let ret_val = self.generate_eq_logic_for_structured_data(&data_spec.collect(), resolved_generics);
        self.builder.build_return(Some(&ret_val));

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
        if function.is_closure() {
            params_iter.next().unwrap().set_name("captures");
        }

        let mut default_value_param_idx = 0;
        for (idx, param) in function.params.iter().enumerate() {
            params_iter.next().unwrap().set_name(&param.name);
            let idx = if function.is_closure() { idx + 1 } else { idx };
            let llvm_param = llvm_fn.get_nth_param(idx as u32).unwrap();
            let llvm_param_type = llvm_param.get_type();
            let variable = if let Some(default_value_node) = &param.default_value {
                let param_local = self.builder.build_alloca(llvm_param_type, &param.name);

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
                let var = self.project.get_var_by_id(&param.var_id);
                if var.is_captured {
                    let ptr_type = llvm_param_type.ptr_type(AddressSpace::Generic);
                    let heap_mem = self.malloc(self.const_i64(8), ptr_type);
                    self.builder.build_store(heap_mem, llvm_param);

                    let param_local = self.builder.build_alloca(ptr_type, &param.name);
                    self.builder.build_store(param_local, heap_mem);
                    LLVMVar::Slot(param_local)
                } else {
                    LLVMVar::Param(llvm_param)
                }
            };
            self.ctx_stack.last_mut().unwrap().variables.insert(param.var_id, variable);
        }

        let num_nodes = function.body.len();
        for (idx, node) in function.body.iter().enumerate() {
            let res = self.visit_statement(node, resolved_generics);
            if idx == num_nodes - 1 {
                if has_return_value {
                    if node.is_returning_terminator() {
                        break;
                    }

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

    // This is a helper function used to create adhoc array type instances for variadic function parameters
    fn compile_array_type(&mut self, inner_type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> StructType<'a> {
        let inner_type_name = self.llvm_type_name_by_id(inner_type_id, resolved_generics);
        let type_name = format!("Array<{inner_type_name}>");

        if let Some(llvm_type) = self.main_module.get_struct_type(&type_name) {
            return llvm_type;
        }

        let llvm_type = self.context.opaque_struct_type(&type_name);
        self.register_typeid(&type_name);
        let struct_ = self.project.get_struct_by_id(&self.project.prelude_array_struct_id);
        let inner_resolved = self.make_resolved_generic(&inner_type_id, resolved_generics);
        let resolved_generics = resolved_generics.extend_via_struct(struct_, &vec![inner_resolved]);

        let mut field_types = Vec::with_capacity(struct_.fields.len() + 1);
        field_types.push(self.i32().into());
        for field in &struct_.fields {
            let Some(field_llvm_type) = self.llvm_underlying_type_by_id(&field.type_id, &resolved_generics) else { todo!() };
            field_types.push(self.llvm_ptr_wrap_type_if_needed(field_llvm_type).into());
        }

        llvm_type.set_body(field_types.as_slice(), false);

        llvm_type
    }

    fn compile_struct_type_by_type_id(&self, type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> StructType<'a> {
        let ty = self.get_type_by_id(type_id);
        let Type::GenericInstance(struct_id, generics) = ty else { todo!() };
        if struct_id == self.project.prelude_option_struct_id {
            return self.compile_option_struct_type_by_type_id(type_id, resolved_generics);
        }
        if struct_id == self.project.prelude_tuple_struct_id {
            return self.compile_tuple_struct_type_by_type_id(type_id, resolved_generics);
        }

        let struct_ = self.project.get_struct_by_id(&struct_id);
        let type_name = self.llvm_type_name_by_id(type_id, resolved_generics);
        let llvm_type = self.context.opaque_struct_type(&type_name);
        self.register_typeid(&type_name);

        let realized_generics = generics.iter().map(|type_id| {
            self.make_resolved_generic(type_id, resolved_generics)
        }).collect();
        let resolved_generics = resolved_generics.extend_via_struct(struct_, &realized_generics);

        let struct_ = self.project.get_struct_by_id(&struct_id);

        let mut field_types = Vec::with_capacity(struct_.fields.len() + 1);
        field_types.push(self.i32().into());
        for field in &struct_.fields {
            let Some(field_llvm_type) = self.llvm_underlying_type_by_id(&field.type_id, &resolved_generics) else { todo!() };
            field_types.push(self.llvm_ptr_wrap_type_if_needed(field_llvm_type).into())
        }
        llvm_type.set_body(field_types.as_slice(), false);

        llvm_type
    }

    fn get_or_compile_enum_type_by_type_id(&self, enum_id: &EnumId, enum_type_name: &String, resolved_generics: &ResolvedGenerics) -> (StructType<'a>, Vec<Option<StructType<'a>>>) {
        let enum_ = self.project.get_enum_by_id(&enum_id);
        debug_assert!(enum_.generic_ids.is_empty(), "generic enums not yet implemented");

        if let Some(enum_llvm_type) = self.main_module.get_struct_type(&enum_type_name) {
            let enum_variant_llvm_types = enum_.variants.iter()
                .map(|v| {
                    let enum_variant_type_name = format!("{}.{}", enum_type_name, &v.name);
                    self.main_module.get_struct_type(&enum_variant_type_name)
                })
                .collect();
            return (enum_llvm_type, enum_variant_llvm_types);
        }

        let enum_llvm_type = self.context.opaque_struct_type(&enum_type_name);
        enum_llvm_type.set_body(&[self.i64().into()], false);
        self.register_typeid(&enum_type_name);

        let mut enum_variant_llvm_types = Vec::with_capacity(enum_.variants.len());
        for variant in &enum_.variants {
            let enum_variant_type_name = format!("{}.{}", enum_type_name, &variant.name);
            self.register_typeid(&enum_variant_type_name);

            let enum_variant_llvm_type = if let EnumVariantKind::Container(func_id) = &variant.kind {
                let enum_variant_llvm_type = self.context.opaque_struct_type(&enum_variant_type_name);

                let container_function = self.project.get_func_by_id(func_id);
                let mut field_types = Vec::with_capacity(container_function.params.len());
                for param in &container_function.params {
                    let Some(field_llvm_type) = self.llvm_underlying_type_by_id(&param.type_id, &resolved_generics) else { todo!() };
                    field_types.push(self.llvm_ptr_wrap_type_if_needed(field_llvm_type).into())
                }
                enum_variant_llvm_type.set_body(field_types.as_slice(), false);

                Some(enum_variant_llvm_type)
            } else {
                None
            };

            enum_variant_llvm_types.push(enum_variant_llvm_type);
        }

        (enum_llvm_type, enum_variant_llvm_types)
    }

    fn construct_const_enum_variant(&self, enum_id: &EnumId, enum_type_name: &String, variant_idx: usize, resolved_generics: &ResolvedGenerics) -> BasicValueEnum<'a> {
        let enum_ = self.project.get_enum_by_id(&enum_id);
        let (enum_llvm_type, _) = self.get_or_compile_enum_type_by_type_id(&enum_id, &enum_type_name, &resolved_generics);
        let variant = &enum_.variants[variant_idx];

        let enum_variant_type_name = self.llvm_enum_variant_type_name(&enum_type_name, &variant.name);
        let variant_typeid = self.get_typeid_by_name(&enum_variant_type_name);

        // An enum instance is contained in 64 bits, and is represented as an unsigned 64-bit integer. The top 16 bits represent the runtime typeid
        // which, for constant enum variants, is enough to distinguish between them. For container variants (aka tagged unions) this 64-bit integer
        // is a pointer to the contained data; since pointers can be represented in 48 bits on modern architectures, so pointer is actually derived
        // via `value & 0xFFFF000000000000`, and the typeid is `value >> 48`.
        //
        // NB: This only works if the typeid values are representable in 16 bits.
        debug_assert!(variant_typeid <= (u16::MAX as usize));
        let variant_value = (variant_typeid as u64) << 48;

        let instance_ptr = self.builder.build_alloca(enum_llvm_type, &format!("{}_instance_ptr", &enum_variant_type_name));
        let value_slot = self.builder.build_struct_gep(instance_ptr, 0, &format!("{}_value_slot", &enum_variant_type_name)).unwrap();
        self.builder.build_store(value_slot, self.const_i64(variant_value));

        self.builder.build_load(instance_ptr, &format!("{}_instance", &enum_variant_type_name))
    }

    fn get_or_compile_tagged_union_enum_variant_function(&mut self, enum_id: &EnumId, enum_type_name: &String, variant_idx: usize, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let enum_ = self.project.get_enum_by_id(&enum_id);
        let (enum_llvm_type, variant_llvm_types) = self.get_or_compile_enum_type_by_type_id(&enum_id, &enum_type_name, &resolved_generics);
        let variant = &enum_.variants[variant_idx];
        let EnumVariantKind::Container(func_id) = &variant.kind else { unreachable!() };
        let enum_variant_type_name = self.llvm_enum_variant_type_name(&enum_type_name, &variant.name);

        let variant_fn_sig = self.llvm_function_signature(func_id, resolved_generics);
        if let Some(llvm_func) = self.main_module.get_function(&variant_fn_sig) {
            return llvm_func;
        }

        let variant_func = self.project.get_func_by_id(func_id);
        let fn_type = self.llvm_function_type(func_id, &resolved_generics);
        let llvm_fn = self.main_module.add_function(&variant_fn_sig, fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, Some(*func_id));
        self.ctx_stack.push(CompilerContext { variables: HashMap::new() });

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let variant_llvm_type = variant_llvm_types[variant_idx].expect(&format!("Variant {} is a tagged union and must have had a struct type defined", &variant.name));
        let variant_typeid = self.get_typeid_by_name(&enum_variant_type_name);

        let mem = self.malloc(self.sizeof_struct(variant_llvm_type), self.ptr(variant_llvm_type));

        let mut params_iter = llvm_fn.get_param_iter();
        for (idx, param) in variant_func.params.iter().enumerate() {
            let llvm_param = params_iter.next().unwrap();
            llvm_param.set_name(&param.name);

            let field_slot = self.builder.build_struct_gep(mem, idx as u32, &format!("{}_field_{}_slot", &variant.name, &param.name)).unwrap();
            self.builder.build_store(field_slot, llvm_param);
        }

        // An enum instance is contained in 64 bits, and is represented as an unsigned 64-bit integer. The top 16 bits represent the runtime typeid
        // which, for constant enum variants, is enough to distinguish between them. For container variants (aka tagged unions) this 64-bit integer
        // is a pointer to the contained data; since pointers can be represented in 48 bits on modern architectures, so pointer is actually derived
        // via `value & 0xFFFF000000000000`, and the typeid is `value >> 48`.
        //
        // NB: This only works if the typeid values are representable in 16 bits.
        debug_assert!(variant_typeid <= (u16::MAX as usize));
        let variant_value = (variant_typeid as u64) << 48;
        let ptr_as_int = self.builder.build_ptr_to_int(mem, self.i64(), "");
        let ptr_as_int_masked = self.builder.build_and(ptr_as_int, self.const_i64(0x0000FFFFFFFFFFFF), "");
        let variant_value = self.builder.build_or(self.const_i64(variant_value), ptr_as_int_masked, "");

        let instance_ptr = self.builder.build_alloca(enum_llvm_type, &format!("{}_instance_ptr", &enum_variant_type_name));
        let value_slot = self.builder.build_struct_gep(instance_ptr, 0, &format!("{}_value_slot", &enum_variant_type_name)).unwrap();
        self.builder.build_store(value_slot, variant_value);

        let ret = self.builder.build_load(instance_ptr, &format!("{}_instance", &enum_variant_type_name));
        self.builder.build_return(Some(&ret));

        self.ctx_stack.pop();
        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn extract_tagged_union_enum_variant_data(&self, local_value: PointerValue<'a>, enum_id: &EnumId, enum_type_name: &String, variant_idx: usize, resolved_generics: &ResolvedGenerics) -> PointerValue<'a> {
        let underlying_int_value_slot = self.builder.build_struct_gep(local_value, 0, "value_slot").unwrap();
        let underlying_int_value = self.builder.build_load(underlying_int_value_slot, "value").into_int_value();
        let masked_value = self.builder.build_and(underlying_int_value, self.const_i64(0x0000FFFFFFFFFFFF), "");

        let (_, variant_llvm_types) = self.get_or_compile_enum_type_by_type_id(enum_id, enum_type_name, resolved_generics);
        let variant_llvm_type = variant_llvm_types[variant_idx].expect(&format!("Variant {} is a tagged union and must have had a struct type defined", variant_idx));

        self.builder.build_int_to_ptr(masked_value, self.ptr(variant_llvm_type), "")
    }

    fn compile_option_struct_type_by_type_id(&self, type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> StructType<'a> {
        let ty = self.get_type_by_id(type_id);
        let Type::GenericInstance(struct_id, generics) = ty else { todo!() };
        debug_assert!(struct_id == self.project.prelude_option_struct_id);

        let type_name = self.llvm_type_name_by_id(type_id, resolved_generics);
        if let Some(llvm_type) = self.main_module.get_struct_type(&type_name) {
            return llvm_type;
        }
        let llvm_type = self.context.opaque_struct_type(&type_name);
        self.register_typeid(&type_name);

        let Some(inner_llvm_type) = self.llvm_underlying_type_by_id(&generics[0], resolved_generics) else { todo!() };
        let inner_llvm_type = self.llvm_ptr_wrap_type_if_needed(inner_llvm_type);

        llvm_type.set_body(&[self.i32().into(), self.bool().into(), inner_llvm_type.into()], false);

        llvm_type
    }

    fn compile_tuple_struct_type_by_type_id(&self, type_id: &TypeId, resolved_generics: &ResolvedGenerics) -> StructType<'a> {
        let ty = self.get_type_by_id(type_id);
        let Type::GenericInstance(struct_id, generics) = ty else { todo!() };
        debug_assert!(struct_id == self.project.prelude_tuple_struct_id);

        let type_name = self.llvm_type_name_by_id(type_id, resolved_generics);
        if let Some(llvm_type) = self.main_module.get_struct_type(&type_name) {
            return llvm_type;
        }
        let llvm_type = self.context.opaque_struct_type(&type_name);
        self.register_typeid(&type_name);

        let mut field_types = Vec::with_capacity(generics.len() + 1);
        field_types.push(self.i32().into());
        for generic_id in generics {
            let Some(inner_llvm_type) = self.llvm_underlying_type_by_id(&generic_id, resolved_generics) else { todo!() };
            field_types.push(self.llvm_ptr_wrap_type_if_needed(inner_llvm_type));
        }
        llvm_type.set_body(field_types.as_slice(), false);

        llvm_type
    }

    fn get_or_compile_type_initializer(&mut self, struct_id: &StructId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let (_, init_fn_sig) = self.llvm_initializer_signature(struct_id, &resolved_generics);
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
        let (type_name, initializer_sig) = self.llvm_initializer_signature(struct_id, resolved_generics);

        let fn_type = self.llvm_initializer_type(struct_id, resolved_generics);
        let llvm_fn = self.main_module.add_function(&initializer_sig, fn_type, None);
        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, None);

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let Some(llvm_struct_type) = self.llvm_underlying_type_by_id(&struct_.self_type_id, resolved_generics) else { todo!() };
        let mem = self.malloc(self.sizeof_struct(llvm_struct_type), self.ptr(llvm_struct_type));

        let typeid = self.get_typeid_by_name(&type_name);
        let typeid_ptr = self.builder.build_struct_gep(mem, 0, "typeid_slot").unwrap();
        self.builder.build_store(typeid_ptr, self.const_i32(typeid as u64));

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
                let field_resolved_generics = self.extend_resolved_generics_via_instance(resolved_generics, &field.type_id);
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
            let field_ptr = self.builder.build_struct_gep(mem, (idx + 1) as u32, &format!("{}_slot", &field.name)).unwrap();
            self.builder.build_store(field_ptr, field_value);
        }

        self.builder.build_return(Some(&mem.as_basic_value_enum()));

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn generate_tostring_logic_for_structured_data(&mut self, prefix: &String, display_names: bool, data_spec: &Vec<(TypeId, String, PointerValue<'a>)>, resolved_generics: &ResolvedGenerics) -> BasicValueEnum<'a> {
        let named_items_fmt_str = data_spec.iter()
            .map(|(type_id, name, _)| {
                let val_fmt = if type_id == &PRELUDE_STRING_TYPE_ID { "\"%s\"" } else { "%s" };
                if display_names {
                    format!("{name}: {val_fmt}")
                } else {
                    val_fmt.to_string()
                }
            })
            .join(", ");
        let to_string_fmt = format!("{prefix}({named_items_fmt_str})");
        let fmt_str_val = self.builder.build_global_string_ptr(&to_string_fmt, "").as_basic_value_enum();
        let mut len_val = self.const_i64(to_string_fmt.replace("%s", "").len() as u64);

        let mut snprintf_args = Vec::with_capacity(data_spec.len());
        for (type_id, name, slot) in data_spec {
            let value = self.builder.build_load(*slot, &name);

            let resolved_generics = self.extend_resolved_generics_via_instance(resolved_generics, &type_id);
            let field_type_id = resolved_generics.resolve_if_generic(&type_id, &self.project)
                .map(|resolved| resolved.type_id)
                .unwrap_or(*type_id);
            let tostring_fn_val = self.get_or_compile_to_string_method_for_type(&field_type_id, &resolved_generics);
            let tostring_val = self.builder.build_call(tostring_fn_val, &[value.into()], &format!("{}_to_string", &name)).try_as_basic_value().left().unwrap().into_pointer_value();

            let (field_tostring_len, field_tostring_chars) = self.destructure_string(tostring_val);
            snprintf_args.push(field_tostring_chars.into());
            len_val = self.builder.build_int_add(len_val, field_tostring_len, "");
        }

        let len_plus_1 = self.builder.build_int_add::<IntValue<'a>>(len_val.into(), self.const_i64(1).into(), "len_plus_1");
        let str_val = self.malloc(len_plus_1, self.ptr(self.i8()));
        let snprintf_len_val = self.builder.build_int_cast(len_plus_1, self.i32(), "");
        let mut args = vec![str_val.into(), snprintf_len_val.into(), fmt_str_val.into()];
        args.append(&mut snprintf_args);
        self.builder.build_call(self.snprintf, args.as_slice(), "").try_as_basic_value().left().unwrap();

        self.construct_string(len_val, str_val)
    }

    fn compile_to_string_method(&mut self, func_id: &FuncId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let function = self.project.get_func_by_id(func_id);
        let FunctionKind::Method(type_id) = &function.kind else { unreachable!() };
        if self.project.get_enum_by_type_id(type_id).is_some() {
            return self.compile_enum_to_string_method(type_id, func_id, resolved_generics);
        }

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

        let fn_sig = self.llvm_function_signature(func_id, resolved_generics);
        let fn_type = self.llvm_function_type(func_id, resolved_generics);
        let llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);
        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, Some(*func_id));

        let mut params_iter = llvm_fn.get_param_iter();
        params_iter.next().unwrap().set_name("self");
        let llvm_self_param = llvm_fn.get_nth_param(0).unwrap().into_pointer_value();

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let data_spec = struct_.fields.iter().enumerate().map(|(idx, field)| {
            let field_slot = self.builder.build_struct_gep(llvm_self_param, (idx + 1) as u32, &format!("{}_slot", &field.name)).unwrap();

            (field.type_id, field.name.clone(), field_slot)
        });
        let ret_val = self.generate_tostring_logic_for_structured_data(&struct_.name, true, &data_spec.collect(), resolved_generics);

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
        bool_true_str_global.set_initializer(&self.context.const_struct(&[self.const_i32(RUNTIME_TYPEID_STRING as u64).into(), self.const_i64(4).into(), true_str], false));
        let bool_false_str_global = self.main_module.add_global(self.string_type, None, "BOOL_FALSE_STR");
        bool_false_str_global.set_constant(true);
        let false_str = self.builder.build_global_string_ptr("false", "").as_basic_value_enum();
        bool_false_str_global.set_initializer(&self.context.const_struct(&[self.const_i32(RUNTIME_TYPEID_STRING as u64).into(), self.const_i64(5).into(), false_str], false));

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

    fn compile_enum_to_string_method(&mut self, type_id: &TypeId, func_id: &FuncId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let Type::GenericEnumInstance(enum_id, _, _) = self.get_type_by_id(type_id) else { unreachable!() };
        let enum_ = self.project.get_enum_by_id(&enum_id);
        let enum_type_name = self.llvm_type_name_by_id(type_id, resolved_generics);

        let fn_sig = self.llvm_function_signature(func_id, resolved_generics);
        let fn_type = self.llvm_function_type(func_id, resolved_generics);
        let llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);
        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, Some(*func_id));

        let mut params_iter = llvm_fn.get_param_iter();
        params_iter.next().unwrap().set_name("self");
        let llvm_self_param = llvm_fn.get_nth_param(0).unwrap();

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let (typeid_val, intermediate_local) = self.get_typeid_from_value(llvm_self_param, None);

        let mut variant_cases = Vec::with_capacity(enum_.variants.len());
        for (idx, variant) in enum_.variants.iter().enumerate() {
            let block = self.context.append_basic_block(self.current_fn.0, &format!("{}_case", &variant.name));
            self.builder.position_at_end(block);
            let name = format!("{}.{}", &enum_.name, &variant.name);

            let ret_val = if let EnumVariantKind::Container(func_id) = &variant.kind {
                let local = intermediate_local.expect("When extracting the typeid from an enum variant value, there is an intermediate value introduced");
                let data = self.extract_tagged_union_enum_variant_data(local, &enum_id, &enum_type_name, idx, resolved_generics);

                let function = self.project.get_func_by_id(func_id);
                let data_spec = function.params.iter().enumerate().map(|(idx, param)| {
                    let slot = self.builder.build_struct_gep(data, idx as u32, &format!("{}_variant_{}_value", &name, &param.name)).unwrap();

                    (param.type_id, param.name.clone(), slot)
                });
                self.generate_tostring_logic_for_structured_data(&name, true, &data_spec.collect(), resolved_generics)
            } else {
                let str_val = self.builder.build_global_string_ptr(&name, "").as_pointer_value();
                let len_val = self.const_i64(name.len() as u64);
                self.construct_string(len_val, str_val)
            };

            self.builder.build_return(Some(&ret_val));

            let enum_variant_type_name = format!("{}.{}", &enum_type_name, &variant.name);
            let variant_typeid = self.get_typeid_by_name(&enum_variant_type_name);
            let switch_case_val = self.const_i32(variant_typeid as u64);

            variant_cases.push((switch_case_val, block));
        }

        let unreachable_block = self.context.append_basic_block(self.current_fn.0, "unreachable");
        self.builder.position_at_end(unreachable_block);
        self.builder.build_unreachable();

        self.builder.position_at_end(block);
        self.builder.build_switch(typeid_val, unreachable_block, variant_cases.as_slice());

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn generate_hash_logic_for_structured_data(&mut self, data_spec: &Vec<(TypeId, String, PointerValue<'a>)>, resolved_generics: &ResolvedGenerics) -> IntValue<'a> {
        let result = self.builder.build_alloca(self.i64(), "result");
        self.builder.build_store(result, self.const_i64(1));

        for (type_id, name, slot) in data_spec {
            let value = self.builder.build_load(*slot, &name);

            let resolved_generics = self.extend_resolved_generics_via_instance(resolved_generics, &type_id);
            let field_type_id = resolved_generics.resolve_if_generic(&type_id, &self.project)
                .map(|resolved| resolved.type_id)
                .unwrap_or(*type_id);
            let hash_fn = self.get_or_compile_hash_method_for_type(&field_type_id, &resolved_generics);
            let hash = self.builder.build_call(hash_fn, &[value.into()], "").try_as_basic_value().left().unwrap().into_int_value();

            let new_result = self.builder.build_int_add(
                self.builder.build_load(result, "result").into_int_value(),
                self.builder.build_int_mul(self.const_i64(31), hash, ""),
                "",
            );
            self.builder.build_store(result, new_result);
        }

        self.builder.build_load(result, "final_result").into_int_value()
    }

    fn compile_hash_method(&mut self, func_id: &FuncId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let fn_sig = self.llvm_function_signature(func_id, resolved_generics);
        let function = self.project.get_func_by_id(func_id);
        let FunctionKind::Method(type_id) = &function.kind else { unreachable!() };
        if self.project.get_enum_by_type_id(type_id).is_some() {
            return self.compile_enum_hash_method(type_id, func_id, resolved_generics);
        }

        let Some((struct_, _)) = self.project.get_struct_by_type_id(type_id) else { todo!() };

        if struct_.id == self.project.prelude_int_struct_id {
            return self.compile_int_hash_method(func_id);
        } else if struct_.id == self.project.prelude_float_struct_id {
            return self.compile_float_hash_method(func_id);
        } else if struct_.id == self.project.prelude_bool_struct_id {
            return self.compile_bool_hash_method(func_id);
        }

        let fn_type = self.llvm_function_type(func_id, resolved_generics);
        let llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);
        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, Some(*func_id));

        let mut params_iter = llvm_fn.get_param_iter();
        params_iter.next().unwrap().set_name("self");
        let llvm_self_param = llvm_fn.get_nth_param(0).unwrap().into_pointer_value();

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let data_spec = struct_.fields.iter().enumerate().map(|(idx, field)| {
            let slot = self.builder.build_struct_gep(llvm_self_param, (idx + 1) as u32, &format!("{}_slot", &field.name)).unwrap();

            (field.type_id, field.name.clone(), slot)
        });
        let result = self.generate_hash_logic_for_structured_data(&data_spec.collect(), resolved_generics);
        self.builder.build_return(Some(&result));

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn compile_int_hash_method(&mut self, func_id: &FuncId) -> FunctionValue<'a> {
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

    fn compile_float_hash_method(&mut self, func_id: &FuncId) -> FunctionValue<'a> {
        let llvm_fn_sig = self.llvm_function_signature(func_id, &ResolvedGenerics::default());
        let llvm_fn_type = self.llvm_function_type(func_id, &ResolvedGenerics::default());
        let llvm_fn = self.main_module.add_function(&llvm_fn_sig, llvm_fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, Some(*func_id));

        llvm_fn.get_param_iter().next().unwrap().set_name("self");
        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);
        let self_val = llvm_fn.get_nth_param(0).unwrap().into_float_value();
        let self_as_int = self.builder.build_cast(InstructionOpcode::BitCast, self_val, self.i64(), "").as_basic_value_enum();
        self.builder.build_return(Some(&self_as_int));

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn compile_bool_hash_method(&mut self, func_id: &FuncId) -> FunctionValue<'a> {
        let llvm_fn_sig = self.llvm_function_signature(func_id, &ResolvedGenerics::default());
        let llvm_fn_type = self.llvm_function_type(func_id, &ResolvedGenerics::default());
        let llvm_fn = self.main_module.add_function(&llvm_fn_sig, llvm_fn_type, None);

        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, Some(*func_id));

        llvm_fn.get_param_iter().next().unwrap().set_name("self");
        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);
        let self_val = llvm_fn.get_nth_param(0).unwrap().into_int_value();
        let self_as_int = self.builder.build_int_cast(self_val, self.i64(), "");
        self.builder.build_return(Some(&self_as_int));

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn compile_enum_hash_method(&mut self, type_id: &TypeId, func_id: &FuncId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let Type::GenericEnumInstance(enum_id, _, _) = self.get_type_by_id(type_id) else { unreachable!() };
        let enum_ = self.project.get_enum_by_id(&enum_id);
        let enum_type_name = self.llvm_type_name_by_id(type_id, resolved_generics);

        let fn_sig = self.llvm_function_signature(func_id, resolved_generics);
        let fn_type = self.llvm_function_type(func_id, resolved_generics);
        let llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);
        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, Some(*func_id));

        let mut params_iter = llvm_fn.get_param_iter();
        params_iter.next().unwrap().set_name("self");
        let llvm_self_param = llvm_fn.get_nth_param(0).unwrap();

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let (typeid_val, intermediate_local) = self.get_typeid_from_value(llvm_self_param, None);

        let mut variant_cases = Vec::with_capacity(enum_.variants.len());
        for (idx, variant) in enum_.variants.iter().enumerate() {
            let enum_variant_type_name = format!("{}.{}", &enum_type_name, &variant.name);
            let variant_typeid = self.get_typeid_by_name(&enum_variant_type_name);
            let switch_case_val = self.const_i32(variant_typeid as u64);

            let block = self.context.append_basic_block(self.current_fn.0, &format!("{}_case", &variant.name));
            self.builder.position_at_end(block);

            let ret_val = if let EnumVariantKind::Container(func_id) = &variant.kind {
                let local = intermediate_local.expect("When extracting the typeid from an enum variant value, there is an intermediate value introduced");
                let data = self.extract_tagged_union_enum_variant_data(local, &enum_id, &enum_type_name, idx, resolved_generics);

                let function = self.project.get_func_by_id(func_id);
                let data_spec = function.params.iter().enumerate().map(|(idx, param)| {
                    let slot = self.builder.build_struct_gep(data, idx as u32, &format!("{}_variant_{}_value", &enum_variant_type_name, &param.name)).unwrap();

                    (param.type_id, param.name.clone(), slot)
                });
                self.generate_hash_logic_for_structured_data(&data_spec.collect(), resolved_generics)
            } else {
                self.const_i64(variant_typeid as u64)
            };

            self.builder.build_return(Some(&ret_val));

            variant_cases.push((switch_case_val, block));
        }

        let unreachable_block = self.context.append_basic_block(self.current_fn.0, "unreachable");
        self.builder.position_at_end(unreachable_block);
        self.builder.build_unreachable();

        self.builder.position_at_end(block);
        self.builder.build_switch(typeid_val, unreachable_block, variant_cases.as_slice());

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn generate_eq_logic_for_structured_data(&mut self, data_spec: &Vec<(TypeId, String, PointerValue<'a>, PointerValue<'a>)>, resolved_generics: &ResolvedGenerics) -> IntValue<'a> {
        let result = self.builder.build_alloca(self.bool(), "result");
        self.builder.build_store(result, self.const_bool(true));

        let end_bb = self.context.append_basic_block(self.current_fn.0, "eq_end");

        for (type_id, name, self_val_slot, other_val_slot) in data_spec {
            let self_item_val = self.builder.build_load(*self_val_slot, &format!("self_{}", &name));
            let other_item_val = self.builder.build_load(*other_val_slot, &format!("other_{}", &name));

            let resolved_generics = self.extend_resolved_generics_via_instance(resolved_generics, &type_id);
            let item_type_id = resolved_generics.resolve_if_generic(&type_id, &self.project)
                .map(|resolved| resolved.type_id)
                .unwrap_or(*type_id);
            let neq = self.compile_eq(true, &item_type_id, self_item_val, &item_type_id, other_item_val, &resolved_generics);

            let then_bb = self.context.append_basic_block(self.current_fn.0, &format!("if_{}_neq", &name));
            let else_bb = self.context.append_basic_block(self.current_fn.0, &format!("if_{}_eq", &name));
            self.builder.build_conditional_branch(neq, then_bb, else_bb);

            self.builder.position_at_end(then_bb);
            self.builder.build_store(result, self.const_bool(false));
            self.builder.build_unconditional_branch(end_bb);

            self.builder.position_at_end(else_bb);
        }

        self.builder.build_unconditional_branch(end_bb);

        self.builder.position_at_end(end_bb);
        self.builder.build_load(result, "result").into_int_value()
    }

    fn compile_eq_method(&mut self, func_id: &FuncId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let fn_sig = self.llvm_function_signature(func_id, resolved_generics);
        let function = self.project.get_func_by_id(func_id);
        let FunctionKind::Method(type_id) = &function.kind else { unreachable!() };
        if self.project.get_enum_by_type_id(type_id).is_some() {
            return self.compile_enum_eq_method(type_id, func_id, resolved_generics);
        }
        let Some((struct_, _)) = self.project.get_struct_by_type_id(type_id) else { todo!() };

        if struct_.id == self.project.prelude_int_struct_id {
            return self.compile_int_hash_method(func_id);
        } else if struct_.id == self.project.prelude_float_struct_id {
            return self.compile_float_hash_method(func_id);
        } else if struct_.id == self.project.prelude_bool_struct_id {
            return self.compile_bool_hash_method(func_id);
        }

        let fn_type = self.llvm_function_type(func_id, resolved_generics);
        let llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);
        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, Some(*func_id));

        let mut params_iter = llvm_fn.get_param_iter();
        params_iter.next().unwrap().set_name("self");
        params_iter.next().unwrap().set_name("other");
        let llvm_self_param = llvm_fn.get_nth_param(0).unwrap().into_pointer_value();
        let llvm_other_param = llvm_fn.get_nth_param(1).unwrap().into_pointer_value();

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let data_spec = struct_.fields.iter().enumerate().map(|(idx, field)| {
            let self_field_slot = self.builder.build_struct_gep(llvm_self_param, (idx + 1) as u32, &format!("self_{}_slot", &field.name)).unwrap();
            let other_field_slot = self.builder.build_struct_gep(llvm_other_param, (idx + 1) as u32, &format!("other_{}_slot", &field.name)).unwrap();

            (field.type_id, field.name.clone(), self_field_slot, other_field_slot)
        });
        let result = self.generate_eq_logic_for_structured_data(&data_spec.collect(), resolved_generics);
        self.builder.build_return(Some(&result));

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn compile_enum_eq_method(&mut self, type_id: &TypeId, func_id: &FuncId, resolved_generics: &ResolvedGenerics) -> FunctionValue<'a> {
        let Type::GenericEnumInstance(enum_id, _, _) = self.get_type_by_id(type_id) else { unreachable!() };
        let enum_ = self.project.get_enum_by_id(&enum_id);
        let enum_type_name = self.llvm_type_name_by_id(type_id, resolved_generics);

        let fn_sig = self.llvm_function_signature(func_id, resolved_generics);
        let fn_type = self.llvm_function_type(func_id, resolved_generics);
        let llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);
        let prev_bb = self.builder.get_insert_block().unwrap();
        let prev_fn = self.current_fn;
        self.current_fn = (llvm_fn, Some(*func_id));

        let mut params_iter = llvm_fn.get_param_iter();
        params_iter.next().unwrap().set_name("self");
        params_iter.next().unwrap().set_name("other");
        let llvm_self_param = llvm_fn.get_nth_param(0).unwrap();
        let llvm_other_param = llvm_fn.get_nth_param(1).unwrap();

        let block = self.context.append_basic_block(llvm_fn, "");
        self.builder.position_at_end(block);

        let (self_typeid_val, self_intermediate_local) = self.get_typeid_from_value(llvm_self_param, None);
        let (other_typeid_val, other_intermediate_local) = self.get_typeid_from_value(llvm_other_param, None);

        let mut variant_cases = Vec::with_capacity(enum_.variants.len());
        for (idx, variant) in enum_.variants.iter().enumerate() {
            let enum_variant_type_name = format!("{}.{}", &enum_type_name, &variant.name);
            let variant_typeid = self.get_typeid_by_name(&enum_variant_type_name);
            let switch_case_val = self.const_i32(variant_typeid as u64);

            let block = self.context.append_basic_block(self.current_fn.0, &format!("{}_case", &variant.name));
            self.builder.position_at_end(block);

            let variant_typeid_val = self.const_i64(variant_typeid as u64);
            let neq = self.builder.build_int_compare(IntPredicate::NE, variant_typeid_val, other_typeid_val, "");

            let neq_bb = self.context.append_basic_block(self.current_fn.0, "typeids_neq_bb");
            let eq_bb = self.context.append_basic_block(self.current_fn.0, "typeids_eq_bb");
            self.builder.build_conditional_branch(neq, neq_bb, eq_bb);

            self.builder.position_at_end(neq_bb);
            self.builder.build_return(Some(&self.const_bool(false)));

            self.builder.position_at_end(eq_bb);
            let ret_val = if let EnumVariantKind::Container(func_id) = &variant.kind {
                let self_local = self_intermediate_local.expect("When extracting the typeid from an enum variant value, there is an intermediate value introduced");
                let other_local = other_intermediate_local.expect("When extracting the typeid from an enum variant value, there is an intermediate value introduced");
                let self_data = self.extract_tagged_union_enum_variant_data(self_local, &enum_id, &enum_type_name, idx, resolved_generics);
                let other_data = self.extract_tagged_union_enum_variant_data(other_local, &enum_id, &enum_type_name, idx, resolved_generics);

                let function = self.project.get_func_by_id(func_id);
                let data_spec = function.params.iter().enumerate().map(|(idx, param)| {
                    let self_slot = self.builder.build_struct_gep(self_data, idx as u32, &format!("{}_variant_{}_value", &enum_variant_type_name, &param.name)).unwrap();
                    let other_slot = self.builder.build_struct_gep(other_data, idx as u32, &format!("{}_variant_{}_value", &enum_variant_type_name, &param.name)).unwrap();

                    (param.type_id, param.name.clone(), self_slot, other_slot)
                });
                self.generate_eq_logic_for_structured_data(&data_spec.collect(), resolved_generics)
            } else {
                self.const_bool(true)
            };

            self.builder.build_return(Some(&ret_val));

            variant_cases.push((switch_case_val, block));
        }

        let unreachable_block = self.context.append_basic_block(self.current_fn.0, "unreachable");
        self.builder.position_at_end(unreachable_block);
        self.builder.build_unreachable();

        self.builder.position_at_end(block);
        self.builder.build_switch(self_typeid_val, unreachable_block, variant_cases.as_slice());

        self.current_fn = prev_fn;
        self.builder.position_at_end(prev_bb);

        llvm_fn
    }

    fn make_function_value(&mut self, func_id: &FuncId, target_type_id: &TypeId, captures_arr: Option<PointerValue<'a>>, resolved_generics: &ResolvedGenerics) -> BasicValueEnum<'a> {
        let Type::Function(target_param_type_ids, target_num_required_params, _, target_return_type_id) = self.get_type_by_id(target_type_id) else { unreachable!() };
        let target_arity = target_num_required_params;
        debug_assert!(target_param_type_ids.len() == target_arity);

        let function = self.project.get_func_by_id(func_id);
        let is_closure = function.is_closure();

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
                let fn_sig = self.llvm_function_signature_by_parts(&wrapper_fn_name, None, &vec![], &wrapper_params, &target_return_type_id, resolved_generics);
                let fn_type = self.llvm_function_type_by_parts(&target_param_type_ids, target_arity, is_closure, false, &target_return_type_id, &resolved_generics);
                let wrapper_llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);

                let prev_bb = self.builder.get_insert_block().unwrap();
                let prev_fn = self.current_fn;
                self.current_fn = (wrapper_llvm_fn, None);
                self.ctx_stack.push(CompilerContext { variables: HashMap::new() });

                let block = self.context.append_basic_block(wrapper_llvm_fn, "");
                self.builder.position_at_end(block);

                let range_end = if is_closure { num_params + 1 } else { num_params };
                let args = (0..range_end).map(|i| wrapper_llvm_fn.get_nth_param(i as u32).unwrap().into()).collect_vec();
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
            let fn_sig = self.llvm_function_signature_by_parts(&wrapper_fn_name, None, &vec![], &wrapper_params, &target_return_type_id, resolved_generics);
            let fn_type = self.llvm_function_type_by_parts(&target_param_type_ids, target_arity, is_closure, false, &target_return_type_id, &resolved_generics);
            let wrapper_llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);

            let prev_bb = self.builder.get_insert_block().unwrap();
            let prev_fn = self.current_fn;
            self.current_fn = (wrapper_llvm_fn, None);
            self.ctx_stack.push(CompilerContext { variables: HashMap::new() });

            let block = self.context.append_basic_block(wrapper_llvm_fn, "");
            self.builder.position_at_end(block);

            let range_end = if is_closure { target_arity + 1 } else { target_arity };
            let mut args = (0..range_end).map(|i| wrapper_llvm_fn.get_nth_param(i as u32).unwrap()).collect_vec();
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
            let fn_sig = self.llvm_function_signature_by_parts(&wrapper_fn_name, None, &vec![], &wrapper_params, &target_return_type_id, resolved_generics);
            let fn_type = self.llvm_function_type_by_parts(&target_param_type_ids, target_arity, is_closure, false, &target_return_type_id, &resolved_generics);
            let wrapper_llvm_fn = self.main_module.add_function(&fn_sig, fn_type, None);

            let prev_bb = self.builder.get_insert_block().unwrap();
            let prev_fn = self.current_fn;
            self.current_fn = (wrapper_llvm_fn, None);
            self.ctx_stack.push(CompilerContext { variables: HashMap::new() });

            let block = self.context.append_basic_block(wrapper_llvm_fn, "");
            self.builder.position_at_end(block);

            let range_end = if is_closure { num_params + 1 } else { num_params };
            let mut args = (0..range_end).map(|i| wrapper_llvm_fn.get_nth_param(i as u32).unwrap()).collect_vec();
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
        let func_ty = self.get_type_by_id(func_type_id);
        let Type::Function(param_type_ids, num_required_params, is_variadic, return_type_id) = func_ty else { unreachable!() };

        let fn_value_type_name = self.llvm_type_name_by_id(func_type_id, resolved_generics);

        // The `fn_ptr` type is always treated as if it's a non-closure function. If it is a closure function, it will be determined
        // dynamically at runtime by comparing the `captures` field with `NULL`, and the `fn_ptr` will be cast into the proper type.
        let llvm_fn_type = self.llvm_function_type_by_parts(&param_type_ids, num_required_params, false, is_variadic, &return_type_id, resolved_generics);
        let fn_ptr_type = llvm_fn_type.ptr_type(AddressSpace::Generic);

        if let Some(llvm_type) = self.main_module.get_struct_type(&fn_value_type_name) {
            (llvm_type, fn_ptr_type)
        } else {
            let fn_val_type = self.context.opaque_struct_type(&fn_value_type_name);
            self.register_typeid(&fn_value_type_name);
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
    fn test_tuples() {
        run_test_file("tuples.abra");
    }

    #[test]
    fn test_maps() {
        run_test_file("maps.abra");
    }

    #[test]
    fn test_sets() {
        run_test_file("sets.abra");
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
    fn test_enums() {
        run_test_file("enums.abra");
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
    fn test_match() {
        run_test_file("match.abra");
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
