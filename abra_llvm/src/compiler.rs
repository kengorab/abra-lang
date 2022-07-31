use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::iter::repeat;
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType, IntType, PointerType, StructType};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallableValue, FloatValue, FunctionValue, InstructionOpcode, IntValue, PointerValue};
use itertools::{Either, Itertools};
use abra_core::common::typed_ast_visitor::TypedAstVisitor;
use abra_core::lexer::tokens::Token;
use abra_core::parser::ast::{BinaryOp, BindingPattern, IndexingMode, UnaryOp};
use abra_core::typechecker::typechecker::TypedModule;
use abra_core::typechecker::typed_ast::{AssignmentTargetKind, TypedAccessorNode, TypedArrayNode, TypedAssignmentNode, TypedAstNode, TypedBinaryNode, TypedBindingDeclNode, TypedEnumDeclNode, TypedForLoopNode, TypedFunctionDeclNode, TypedGroupedNode, TypedIdentifierNode, TypedIfNode, TypedImportNode, TypedIndexingNode, TypedInstantiationNode, TypedInvocationNode, TypedLambdaNode, TypedLiteralNode, TypedMapNode, TypedMatchNode, TypedReturnNode, TypedSetNode, TypedTupleNode, TypedTypeDeclField, TypedTypeDeclNode, TypedUnaryNode, TypedWhileLoopNode};
use abra_core::typechecker::types::{FnType, Type};

const ENTRY_FN_NAME: &str = "__mod_entry";

// Cached externally-defined functions
const FN_PRINTF: &str = "printf";
const FN_SNPRINTF: &str = "snprintf";
const FN_POWF64: &str = "llvm.pow.f64";
const FN_MALLOC: &str = "GC_malloc";
const FN_DOUBLE_TO_VALUE_T: &str = "double_to_value_t";
const FN_VALUE_T_TO_DOUBLE: &str = "value_t_to_double";
const FN_STRING_ALLOC: &str = "string_alloc";
const FN_STRING_CONCAT: &str = "string_concat";
const FN_STRING_GET: &str = "string_get";
const FN_STRING_RANGE: &str = "string_range";
const FN_STRING_SPLIT: &str = "string_split";
const FN_TUPLE_ALLOC: &str = "tuple_alloc";
const FN_ARRAY_ALLOC: &str = "array_alloc";
const FN_ARRAY_INSERT: &str = "array_insert";
const FN_ARRAY_GET: &str = "array_get";
const FN_ARRAY_RANGE: &str = "array_range";
const FN_ARRAY_SPLIT: &str = "array_split";
const FN_TUPLE_GET: &str = "tuple_get";
const FN_TUPLE_SET: &str = "tuple_set";
const FN_MAP_ALLOC: &str = "map_alloc";
const FN_MAP_INSERT: &str = "map_insert";
const FN_MAP_GET: &str = "map_get";
const FN_SET_ALLOC: &str = "set_alloc";
const FN_SET_INSERT: &str = "set_insert";
const FN_FUNCTION_ALLOC: &str = "function_alloc";
const FN_FUNCTION_BIND: &str = "function_bind";
const FN_FUNCTION_CALL: &str = "function_call";
const FN_VTABLE_ALLOC_ENTRY: &str = "vtable_alloc_entry";
const FN_VTABLE_LOOKUP: &str = "vtable_lookup";
const FN_VALUE_TO_STRING: &str = "value_to_string";
const FN_VALUE_EQ: &str = "value_eq";
const FN_VALUE_HASH: &str = "value_hash";

// Cached externally-defined types
const TYPE_OBJ_HEADER: &str = "obj_header_t";
const TYPE_HASHMAP: &str = "hashmap_t";
const TYPE_STRING: &str = "String";
const TYPE_ARRAY: &str = "Array";
const TYPE_MAP: &str = "Map";
const TYPE_SET: &str = "Set";
const TYPE_FUNCTION: &str = "Function";

// NaN-tagging constants
// IMPORTANT! These must stay in sync with the constants in `rt.h`
const MASK_NAN: u64 = 0x7ffc000000000000;
const MASK_INT: u64 = MASK_NAN | 0x0002000000000000;
const MASK_OBJ: u64 = MASK_NAN | 0x8000000000000000;

const VAL_NONE: u64  = MASK_NAN | 0x0001000000000000;
const VAL_FALSE: u64 = MASK_NAN | 0x0001000000000001;
const VAL_TRUE: u64  = MASK_NAN | 0x0001000000000002;

const PAYLOAD_MASK_INT: u64 = 0x00000000ffffffff;
const PAYLOAD_MASK_OBJ: u64 = 0x0000ffffffffffff;

#[derive(Debug)]
struct Variable<'ctx> {
    is_captured: bool,
    local_ptr: PointerValue<'ctx>,
}

#[derive(Debug)]
struct ClosureContext<'ctx> {
    env: PointerValue<'ctx>,
    captured_variables: HashMap<String, usize>,
}

#[derive(Debug)]
struct TypeSpec<'ctx> {
    typ: Type,
    type_id: IntValue<'ctx>,
    struct_type: StructType<'ctx>,
    ctor_fn: FunctionValue<'ctx>
}

#[derive(Debug)]
struct EnumSpec<'ctx> {
    typ: Type,
    base_struct_type: StructType<'ctx>,
    type_id: IntValue<'ctx>,
    variants: Vec<EnumVariantSpec<'ctx>>,
}

#[derive(Debug)]
struct EnumVariantSpec<'ctx> {
    variant_idx: usize,
    struct_type: StructType<'ctx>,
    val: Either<PointerValue<'ctx>, BasicValueEnum<'ctx>>,
}

#[derive(Debug)]
struct Scope<'ctx> {
    name: String,
    fn_depth: usize,
    fns: HashMap<String, FunctionValue<'ctx>>,
    variables: HashMap<String, Variable<'ctx>>,
    closure_context: Option<ClosureContext<'ctx>>,
    types: HashMap<String, TypeSpec<'ctx>>,
    enums: HashMap<String, EnumSpec<'ctx>>,
}

pub struct Compiler<'a, 'ctx> {
    module_name: &'a str,
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    cur_fn: FunctionValue<'ctx>,
    scopes: Vec<Scope<'ctx>>,
    cur_loop: Option<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
}

type CompilerError = ();

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile_module(context: &'ctx Context, typed_module: TypedModule) -> Result<Module<'ctx>, CompilerError> {
        let module_name = "__main";
        let builder = context.create_builder();
        let module = context.create_module(module_name);

        let entry_fn_type = context.void_type().fn_type(&[], false);
        let entry_fn = module.add_function(ENTRY_FN_NAME, entry_fn_type, None);

        let root_scope = Scope {
            name: "$root".to_string(),
            fn_depth: 0,
            fns: HashMap::new(),
            variables: HashMap::new(),
            closure_context: None,
            types: HashMap::new(),
            enums: HashMap::new(),
        };

        let mut compiler = Compiler {
            module_name,
            context: &context,
            builder: &builder,
            module: &module,
            cur_fn: entry_fn,
            scopes: vec![root_scope],
            cur_loop: None,
        };

        compiler.init();

        let mut last_item = context.i64_type().const_zero().as_basic_value_enum();
        let mut last_item_type = Type::Unit;
        for node in typed_module.typed_nodes {
            last_item_type = node.get_type();
            last_item = compiler.visit(node)?;
        }

        compiler.finalize(&last_item_type, last_item);

        // module.print_to_stderr();
        Ok(module)
    }

    fn incr_next_type_id(&self) -> IntValue<'ctx> {
        let next_type_id = self.module.get_global("next_type_id").unwrap();
        let type_id = self.builder.build_load(next_type_id.as_pointer_value(), "").into_int_value();
        self.builder.build_store(
            next_type_id.as_pointer_value(),
            self.builder.build_int_add(
                type_id,
                self.context.i32_type().const_int(1, false),
                ""
            )
        );

        type_id
    }

    fn gen_llvm_fn_type(&self, has_return: bool, num_args: usize) -> FunctionType<'ctx> {
        let mut args = Vec::new();
        args.push(self.value_t_ptr().into());
        args.push(self.context.i8_type().into());
        args.append(&mut repeat(self.value_t().into()).take(num_args).collect_vec());

        if has_return {
            self.value_t().fn_type(args.as_slice(), false)
        } else {
            self.context.void_type().fn_type(args.as_slice(), false)
        }
    }

    fn init_prelude(&mut self) {
        let println = self.module.add_function("prelude__println", self.gen_llvm_fn_type(false, 1), None);
        self.current_scope_mut().fns.insert("println".to_string(), println);
        let print = self.module.add_function("prelude__print", self.gen_llvm_fn_type(false, 1), None);
        self.current_scope_mut().fns.insert("print".to_string(), print);

        self.init_int_type();
        self.init_float_type();
        self.init_bool_type();
        self.init_string_type();
        self.init_array_type();
        self.init_tuple_type();
        self.init_map_type();
        self.init_set_type();
        self.init_function_type();
    }

    fn is_builtin_type(typ: &Type) -> bool {
        match typ {
            Type::Int | Type::Float | Type::Bool | Type::String | Type::Array(_) | Type::Tuple(_) | Type::Map(_, _) | Type::Set(_) => true,
            _ => false,
        }
    }

    fn init_builtin_type(&self, type_id_name: &str, methods: &[(&str, FunctionType<'ctx>)]) {
        // Increment the type_id counter
        let type_id = self.incr_next_type_id();
        let new_type_id = self.module.add_global(self.context.i32_type(), None, type_id_name);
        self.builder.build_store(new_type_id.as_pointer_value(), type_id);

        // Allocate vtable space for the type's methods
        let type_id = self.builder.build_load(new_type_id.as_pointer_value(), "").into_int_value();
        let num_methods = self.context.i32_type().const_int(methods.len() as u64, false);
        let vtable_entry = self.builder.build_call(self.cached_fn(FN_VTABLE_ALLOC_ENTRY), &[type_id.into(), num_methods.into()], "").try_as_basic_value().left().unwrap().into_pointer_value();

        // Store the methods in the vtable
        for (idx, (method_name, method_type)) in methods.iter().enumerate() {
            let slot = unsafe { self.builder.build_gep(vtable_entry, &[self.context.i32_type().const_int(idx as u64, false)], "") };
            let fn_ptr = self.module.add_function(method_name, method_type.clone(), None).as_global_value().as_pointer_value();
            self.builder.build_store(slot, self.builder.build_cast(InstructionOpcode::PtrToInt, fn_ptr, self.context.i64_type(), ""));
        }
    }

    fn init_int_type(&self)  {
        self.init_builtin_type("type_id_Int", &[
            ("prelude__Int__toString", self.gen_llvm_fn_type(true, 1)),
            ("prelude__Int__abs", self.gen_llvm_fn_type(true, 1)),
        ]);
    }

    fn init_float_type(&self) {
        self.init_builtin_type("type_id_Float", &[
            ("prelude__Float__toString", self.gen_llvm_fn_type(true, 1)),
            ("prelude__Float__floor", self.gen_llvm_fn_type(true, 1)),
        ]);
    }

    fn init_bool_type(&self) {
        self.init_builtin_type("type_id_Bool", &[
            ("prelude__Bool__toString", self.gen_llvm_fn_type(true, 1)),
        ]);
    }

    fn init_string_type(&self) {
        self.init_builtin_type("type_id_String", &[
            ("prelude__String__toString", self.gen_llvm_fn_type(true, 1)),
            ("prelude__String__toLower", self.gen_llvm_fn_type(true, 1)),
            ("prelude__String__toUpper", self.gen_llvm_fn_type(true, 1)),
        ]);
    }

    fn init_array_type(&self) {
        self.init_builtin_type("type_id_Array", &[
            ("prelude__Array__toString", self.gen_llvm_fn_type(true, 1)),
            ("prelude__Array__isEmpty", self.gen_llvm_fn_type(true, 1)),
            ("prelude__Array__enumerate", self.gen_llvm_fn_type(true, 1)),
        ]);
    }

    fn init_tuple_type(&self) {
        self.init_builtin_type("type_id_Tuple", &[
            ("prelude__Tuple__toString", self.gen_llvm_fn_type(true, 1)),
        ]);
    }

    fn init_map_type(&self) {
        self.init_builtin_type("type_id_Map", &[
            ("prelude__Map__toString", self.gen_llvm_fn_type(true, 1)),
            ("prelude__Map__isEmpty", self.gen_llvm_fn_type(true, 1)),
            ("prelude__Map__enumerate", self.gen_llvm_fn_type(true, 1)),
        ]);
    }

    fn init_set_type(&self) {
        self.init_builtin_type("type_id_Set", &[
            ("prelude__Set__toString", self.gen_llvm_fn_type(true, 1)),
            ("prelude__Set__isEmpty", self.gen_llvm_fn_type(true, 1)),
            ("prelude__Set__enumerate", self.gen_llvm_fn_type(true, 1)),
        ]);
    }

    fn init_function_type(&self) {
        self.init_builtin_type("type_id_Function", &[
            ("prelude__Function__toString", self.gen_llvm_fn_type(true, 1)),
        ]);
    }

    fn init(&mut self) {
        self.module.add_global(self.context.i32_type(), None, "next_type_id");

        let value_t = self.value_t();
        let str = self.str_type();
        let void = self.context.void_type();
        let i8 = self.context.i8_type();
        let i32 = self.context.i32_type();
        let i64 = self.context.i64_type();
        let f64 = self.context.f64_type();
        let bool = self.context.bool_type();

        self.module.add_function(FN_MALLOC, str.fn_type(&[i64.into()], false), None);
        self.module.add_function(FN_PRINTF, i64.fn_type(&[str.into()], true), None);
        self.module.add_function(FN_SNPRINTF, i64.fn_type(&[str.into(), i32.into(), str.into()], true), None);
        self.module.add_function(FN_POWF64, f64.fn_type(&[f64.into(), f64.into()], false), None);
        self.module.add_function(FN_DOUBLE_TO_VALUE_T, value_t.fn_type(&[f64.into()], false), None);
        self.module.add_function(FN_VALUE_T_TO_DOUBLE, f64.fn_type(&[value_t.into()], false), None);
        self.module.add_function(FN_STRING_ALLOC, value_t.fn_type(&[i32.into(), str.into()], false), None);
        self.module.add_function(FN_STRING_CONCAT, value_t.fn_type(&[value_t.into(), value_t.into()], false), None);
        self.module.add_function(FN_STRING_GET, value_t.fn_type(&[value_t.into(), i32.into()], false), None);
        self.module.add_function(FN_STRING_RANGE, value_t.fn_type(&[value_t.into(), value_t.into(), value_t.into()], false), None);
        self.module.add_function(FN_STRING_SPLIT, value_t.fn_type(&[value_t.into(), i32.into()], false), None);
        self.module.add_function(FN_TUPLE_ALLOC, value_t.fn_type(&[i32.into()], true), None);
        self.module.add_function(FN_ARRAY_ALLOC, value_t.fn_type(&[i32.into()], false), None);
        self.module.add_function(FN_ARRAY_INSERT, void.fn_type(&[value_t.into(), i32.into(), value_t.into()], false), None);
        self.module.add_function(FN_ARRAY_GET, value_t.fn_type(&[value_t.into(), i32.into()], false), None);
        self.module.add_function(FN_ARRAY_RANGE, value_t.fn_type(&[value_t.into(), value_t.into(), value_t.into()], false), None);
        self.module.add_function(FN_ARRAY_SPLIT, value_t.fn_type(&[value_t.into(), i32.into()], false), None);
        self.module.add_function(FN_TUPLE_GET, value_t.fn_type(&[value_t.into(), i32.into()], false), None);
        self.module.add_function(FN_TUPLE_SET, void.fn_type(&[value_t.into(), i32.into(), value_t.into()], false), None);
        self.module.add_function(FN_MAP_ALLOC, value_t.fn_type(&[i32.into()], false), None);
        self.module.add_function(FN_MAP_INSERT, void.fn_type(&[value_t.into(), value_t.into(), value_t.into()], false), None);
        self.module.add_function(FN_MAP_GET, value_t.fn_type(&[value_t.into(), value_t.into()], false), None);
        self.module.add_function(FN_SET_ALLOC, value_t.fn_type(&[i32.into()], false), None);
        self.module.add_function(FN_SET_INSERT, void.fn_type(&[value_t.into(), value_t.into()], false), None);
        self.module.add_function(FN_FUNCTION_ALLOC, value_t.fn_type(&[str.into(), value_t.into(), value_t.ptr_type(AddressSpace::Generic).into()], false), None);
        self.module.add_function(FN_FUNCTION_BIND, value_t.fn_type(&[value_t.into(), value_t.into()], false), None);
        self.module.add_function(FN_FUNCTION_CALL, value_t.fn_type(&[value_t.into(), bool.into(), i8.into(), i8.into()], true), None);
        self.module.add_function(FN_VTABLE_ALLOC_ENTRY, i64.ptr_type(AddressSpace::Generic).fn_type(&[i32.into(), i32.into()], false), None);
        self.module.add_function(FN_VTABLE_LOOKUP, value_t.fn_type(&[value_t.into(), i32.into()], false), None);
        self.module.add_function(FN_VALUE_TO_STRING, value_t.fn_type(&[value_t.into()], false), None);
        self.module.add_function(FN_VALUE_EQ, bool.fn_type(&[value_t.into(), value_t.into()], false), None);
        self.module.add_function(FN_VALUE_HASH, i32.fn_type(&[value_t.into()], false), None);

        let obj_header_t = self.context.opaque_struct_type(TYPE_OBJ_HEADER);
        obj_header_t.set_body(&[i32.into() /* type_id */], false);
        let hashmap_t = self.context.opaque_struct_type(TYPE_HASHMAP);
        hashmap_t.set_body(&[i32.into(), i32.into() /* size */], false);

        self.context.opaque_struct_type(TYPE_STRING).set_body(
            &[
                obj_header_t.into(), // obj_header_t h
                i32.into(), // int32_t length
                str, // char* chars
            ],
            false,
        );
        self.context.opaque_struct_type(TYPE_ARRAY).set_body(
            &[
                obj_header_t.into(), // obj_header_t h
                i32.into(), // int32_t length
                i32.into(), // int32_t capacity
                value_t.ptr_type(AddressSpace::Generic).into(), // value_t* items
            ],
            false,
        );
        self.context.opaque_struct_type(TYPE_MAP).set_body(
            &[
                obj_header_t.into(), // obj_header_t h
                hashmap_t.into(), // hashmap_t hash
            ],
            false,
        );
        self.context.opaque_struct_type(TYPE_SET).set_body(
            &[
                obj_header_t.into(), // obj_header_t h
                hashmap_t.into(), // hashmap_t hash
            ],
            false,
        );
        self.context.opaque_struct_type(TYPE_FUNCTION).set_body(
            &[
                obj_header_t.into(), // obj_header_t h
                str.into(), // char* name
                value_t.into(), // value_t fn_ptr
                value_t.ptr_type(AddressSpace::Generic).into(), // value_t* env
                i32.into(), // uint32_t id
                value_t.into(), // value_t bound_self
            ],
            false,
        );

        // Prelude initialization function
        let init_fn_type = void.fn_type(&[], false);
        let init_fn = self.module.add_function("$init", init_fn_type, None);
        let init_fn_bb = self.context.append_basic_block(init_fn, "init_fn_bb");
        self.builder.position_at_end(init_fn_bb);
        self.init_prelude();
        self.builder.build_return(None);

        // Begin emitting code in the entry function, starting with `$init()`
        let entry_fn_bb = self.context.append_basic_block(self.cur_fn, "entry_fn_bb");
        self.builder.position_at_end(entry_fn_bb);
        self.builder.build_call(init_fn, &[], "");
    }

    fn finalize(&self,  last_item_type: &Type, last_item: BasicValueEnum<'ctx>) {
        if *last_item_type != Type::Unit {
            let res_val = self.builder.build_call(self.cached_fn(FN_VALUE_TO_STRING), &[last_item.into()], "").try_as_basic_value().left().unwrap().into_int_value();
            let res_val = self.emit_extract_nan_tagged_obj(res_val);
            let res_str = self.builder.build_cast(InstructionOpcode::BitCast, res_val, self.cached_type(TYPE_STRING).ptr_type(AddressSpace::Generic), "").into_pointer_value();
            let res_str_chars = self.builder.build_struct_gep(res_str, 2, "").unwrap();

            self.builder.build_call(
                self.cached_fn(FN_PRINTF),
                &[
                    self.builder.build_global_string_ptr("%s\n", "fmt").as_basic_value_enum().into(),
                    self.builder.build_load(res_str_chars, "").into()
                ],
                "",
            );
        }

        self.builder.build_return(None);
    }

    #[inline]
    fn cached_fn(&self, name: &str) -> FunctionValue<'ctx> {
        self.module.get_function(name).expect(&format!("Internal error: expected function '{}' to be defined", name))
    }

    #[inline]
    fn cached_type(&self, name: &str) -> StructType<'ctx> {
        self.module.get_struct_type(name).expect(&format!("Internal error: expected type '{}' to be defined", name))
    }

    #[allow(dead_code)]
    fn println_debug(&self, tag: &str, value: BasicValueEnum<'ctx>) {
        let res_val = self.builder.build_call(self.cached_fn(FN_VALUE_TO_STRING), &[value.into()], "").try_as_basic_value().left().unwrap().into_int_value();
        let res_val = self.emit_extract_nan_tagged_obj(res_val);
        let res_str = self.builder.build_cast(InstructionOpcode::BitCast, res_val, self.cached_type(TYPE_STRING).ptr_type(AddressSpace::Generic), "").into_pointer_value();
        let res_str_chars = self.builder.build_struct_gep(res_str, 2, "").unwrap();

        self.builder.build_call(
            self.cached_fn(FN_PRINTF),
            &[
                self.builder.build_global_string_ptr(&format!("{}: %s\n", tag), "fmt").as_basic_value_enum().into(),
                self.builder.build_load(res_str_chars, "").into()
            ],
            "",
        );
    }

    fn current_scope(&self) -> &Scope<'ctx> {
        self.scopes.last().expect("There should always be at least 1 scope")
    }

    fn current_scope_mut(&mut self) -> &mut Scope<'ctx> {
        self.scopes.last_mut().expect("There should always be at least 1 scope")
    }

    fn begin_new_fn_scope(&mut self, name: String, closure_context: Option<ClosureContext<'ctx>>) {
        let current_fn_depth = self.current_scope().fn_depth;
        let fn_depth = current_fn_depth + 1;
        self.scopes.push(Scope { name, fn_depth, fns: HashMap::new(), variables: HashMap::new(), closure_context, types: HashMap::new(), enums: HashMap::new() })
    }

    fn begin_new_scope(&mut self, name: String) {
        let fn_depth = self.current_scope().fn_depth;
        self.scopes.push(Scope { name, fn_depth, fns: HashMap::new(), variables: HashMap::new(), closure_context: None, types: HashMap::new(), enums: HashMap::new() })
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn str_type(&self) -> BasicTypeEnum<'ctx> {
        self.context.i8_type().ptr_type(AddressSpace::Generic).as_basic_type_enum()
    }

    fn alloc_string_obj(&self, length_val: IntValue<'ctx>, chars_val: PointerValue<'ctx>) -> IntValue<'ctx> {
        self.builder.build_call(self.cached_fn(FN_STRING_ALLOC), &[length_val.into(), chars_val.into()], "").try_as_basic_value().left().unwrap().into_int_value()
    }

    fn alloc_const_string_obj<S: AsRef<str>>(&self, s: S) -> IntValue<'ctx> {
        let str = s.as_ref();
        let len = str.len();

        self.alloc_string_obj(
            self.context.i32_type().const_int(len as u64, false),
            self.builder.build_global_string_ptr(str, "").as_pointer_value(),
        )
    }

    fn alloc_tuple_obj(&self, mut items: Vec<BasicMetadataValueEnum<'ctx>>) -> IntValue<'ctx> {
        let mut args = Vec::with_capacity(items.len() + 1);
        args.push(self.context.i32_type().const_int(items.len() as u64, false).into());
        args.append(&mut items);

        self.builder.build_call(self.cached_fn(FN_TUPLE_ALLOC), args.as_slice(), "").try_as_basic_value().left().unwrap().into_int_value()
    }

    fn alloc_array_obj_with_length(&self, length_val: IntValue<'ctx>) -> IntValue<'ctx> {
        self.builder.build_call(self.cached_fn(FN_ARRAY_ALLOC), &[length_val.into()], "").try_as_basic_value().left().unwrap().into_int_value()
    }

    fn array_obj_insert(&self, array_val: IntValue<'ctx>, index_val: IntValue<'ctx>, item_val: IntValue<'ctx>) {
        self.builder.build_call(self.cached_fn(FN_ARRAY_INSERT), &[array_val.into(), index_val.into(), item_val.into()], "");
    }

    fn alloc_map_obj(&self, entries: Vec<(BasicValueEnum<'ctx>, BasicValueEnum<'ctx>)>) -> IntValue<'ctx> {
        let length_val = self.context.i32_type().const_int(entries.len() as u64, false);

        let map = self.builder.build_call(self.cached_fn(FN_MAP_ALLOC), &[length_val.into()], "").try_as_basic_value().left().unwrap().into_int_value();
        for (key, value) in entries {
            self.builder.build_call(
                self.cached_fn(FN_MAP_INSERT),
                &[map.into(), key.into(), value.into()],
                ""
            );
        }

        map
    }

    fn alloc_set_obj(&self, values: Vec<BasicValueEnum<'ctx>>) -> IntValue<'ctx> {
        let length_val = self.context.i32_type().const_int(values.len() as u64, false);

        let set = self.builder.build_call(self.cached_fn(FN_SET_ALLOC), &[length_val.into()], "").try_as_basic_value().left().unwrap().into_int_value();
        for value in values {
            self.builder.build_call(
                self.cached_fn(FN_SET_INSERT),
                &[set.into(), value.into()],
                ""
            );
        }

        set
    }

    fn value_t(&self) -> IntType<'ctx> {
        self.context.i64_type()
    }

    fn value_t_ptr(&self) -> PointerType<'ctx> {
        self.value_t().ptr_type(AddressSpace::Generic)
    }

    fn emit_nan_tagged_int_const(&self, int: i32) -> IntValue<'ctx> {
        self.emit_nan_tagged_int(self.context.i64_type().const_int(int as u64, false))
    }

    fn emit_nan_tagged_int(&self, int_val: IntValue<'ctx>) -> IntValue<'ctx> {
        let mask = self.context.i64_type().const_int(MASK_INT, false);
        let int_val = self.builder.build_int_cast(int_val, self.context.i64_type(), "");
        self.builder.build_or(mask, int_val, "")
    }

    fn emit_nan_tagged_float_const(&self, float: f64) -> IntValue<'ctx> {
        self.emit_nan_tagged_float(self.context.f64_type().const_float(float))
    }

    fn emit_nan_tagged_float(&self, float_val: FloatValue<'ctx>) -> IntValue<'ctx> {
        self.builder.build_call(
            self.cached_fn(FN_DOUBLE_TO_VALUE_T),
            &[float_val.into()],
            ""
        ).try_as_basic_value().left().unwrap().into_int_value()
    }

    fn emit_nan_tagged_bool_const(&self, b: bool) -> IntValue<'ctx> {
        self.context.i64_type().const_int(if b { VAL_TRUE } else { VAL_FALSE }, false)
    }

    fn emit_nan_tagged_bool(&self, bool_val: IntValue<'ctx>) -> IntValue<'ctx> {
        self.builder.build_int_add(
            self.context.i64_type().const_int(VAL_FALSE, false),
            self.builder.build_int_z_extend(bool_val, self.context.i64_type(), ""),
            "",
        )
    }

    fn emit_nan_tagged_obj(&self, ptr: PointerValue<'ctx>) -> IntValue<'ctx> {
        let ptr = self.builder.build_cast(InstructionOpcode::PtrToInt, ptr, self.context.i64_type(), "");
        let mask = self.context.i64_type().const_int(MASK_OBJ, false);
        self.builder.build_or(mask, ptr.into_int_value(), "")
    }

    fn emit_extract_nan_tagged_int(&self, value: IntValue<'ctx>) -> IntValue<'ctx> {
        let payload_mask_int = self.context.i64_type().const_int(PAYLOAD_MASK_INT, false);
        let res = self.builder.build_and(payload_mask_int, value, "");
        self.builder.build_int_cast_sign_flag(res, self.context.i32_type(), true, "")
    }

    fn emit_extract_nan_tagged_float(&self, value: IntValue<'ctx>) -> FloatValue<'ctx> {
        self.builder.build_call(
            self.cached_fn(FN_VALUE_T_TO_DOUBLE),
            &[value.into()],
            ""
        ).try_as_basic_value().left().unwrap().into_float_value()
    }

    fn emit_extract_nan_tagged_bool(&self, value: IntValue<'ctx>) -> IntValue<'ctx> {
        self.builder.build_int_sub(value, self.context.i64_type().const_int(VAL_FALSE, false), "")
    }

    fn emit_extract_nan_tagged_obj(&self, value: IntValue<'ctx>) -> PointerValue<'ctx> {
        let payload_mask_obj = self.context.i64_type().const_int(PAYLOAD_MASK_OBJ, false);
        let ptr = self.builder.build_and(payload_mask_obj, value, "");
        self.builder.build_cast(InstructionOpcode::IntToPtr, ptr, self.context.i8_type().ptr_type(AddressSpace::Generic), "").into_pointer_value()
    }

    fn val_none(&self) -> IntValue<'ctx> {
        self.context.i64_type().const_int(VAL_NONE, false)
    }

    #[inline]
    fn visit_binding_pattern(&mut self, pat: BindingPattern, cur_val: BasicValueEnum<'ctx>) {
        match pat {
            BindingPattern::Variable(var_name) => {
                let name = Token::get_ident_name(&var_name);
                let local_ptr = self.builder.build_alloca(self.value_t(), &name);

                self.current_scope_mut().variables.insert(name, Variable { local_ptr, is_captured: false });
                self.builder.build_store(local_ptr, cur_val);
            }
            BindingPattern::Tuple(_, pats) => {
                for (idx, pat) in pats.into_iter().enumerate() {
                    let idx = self.context.i32_type().const_int(idx as u64, false);
                    let val = self.builder.build_call(
                        self.cached_fn(FN_TUPLE_GET),
                        &[cur_val.into(), idx.into()],
                        ""
                    ).try_as_basic_value().left().unwrap();
                    self.visit_binding_pattern(pat, val);
                }
            }
            BindingPattern::Array(_, pats, is_string) => {
                let num_pats = pats.len();
                let mut pats_iter = pats.into_iter();
                let mut cur_val = cur_val;
                let mut idx = 0;
                while let Some((pat, is_splat)) = pats_iter.next() {
                    if is_splat {
                        let (range_fn, split_fn) = if is_string {
                            (self.cached_fn(FN_STRING_RANGE), self.cached_fn(FN_STRING_SPLIT))
                        } else {
                            (self.cached_fn(FN_ARRAY_RANGE), self.cached_fn(FN_ARRAY_SPLIT))
                        };

                        let idx_val = self.emit_nan_tagged_int_const(idx as i32);
                        let tail = self.builder.build_call(range_fn, &[cur_val.into(), idx_val.into(), self.val_none().into()], "").try_as_basic_value().left().unwrap();

                        let split_idx = (num_pats - idx - 1) as i64;
                        let split_idx = self.context.i32_type().const_int((-split_idx) as u64, true);
                        let parts = self.builder.build_call(split_fn, &[tail.into(), split_idx.into()], "").try_as_basic_value().left().unwrap();
                        let l_part = self.builder.build_call(self.cached_fn(FN_TUPLE_GET), &[parts.into(), self.context.i32_type().const_int(0, false).into()], "").try_as_basic_value().left().unwrap();
                        let r_part = self.builder.build_call(self.cached_fn(FN_TUPLE_GET), &[parts.into(), self.context.i32_type().const_int(1, false).into()], "").try_as_basic_value().left().unwrap();

                        if idx == num_pats - 1 {
                            self.visit_binding_pattern(pat, r_part);
                        } else {
                            self.visit_binding_pattern(pat, l_part);
                            cur_val = r_part;
                            idx = 0;
                        }
                        continue;
                    }

                    let idx_val = self.context.i32_type().const_int(idx as u64, false);
                    let func = if is_string { self.cached_fn(FN_STRING_GET) } else { self.cached_fn(FN_ARRAY_GET) };
                    let val = self.builder.build_call(
                        func,
                        &[cur_val.into(), idx_val.into()],
                        ""
                    ).try_as_basic_value().left().unwrap();
                    self.visit_binding_pattern(pat, val);

                    idx += 1;
                }
            }
        }
    }

    #[inline]
    fn build_bool_negate(&self, bool_val: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        let value = self.emit_extract_nan_tagged_bool(bool_val.into_int_value());
        let res = self.builder.build_int_unsigned_rem(
            self.builder.build_int_add(value, self.context.i64_type().const_int(1, false), ""),
            self.context.i64_type().const_int(2, false),
            ""
        );
        self.emit_nan_tagged_bool(res).as_basic_value_enum()
    }

    fn resolve_ptr_to_variable(&self, var_name: &String) -> PointerValue<'ctx> {
        // Find the scope that contains the variable
        let containing_scope = self.scopes.iter().rev().find(|sc| sc.variables.contains_key(var_name));
        let containing_scope = containing_scope.expect(&format!("Internal error: no expected outer scope for variable '{}'", var_name));

        // If the containing scope represents a different function, this is a closed-over variable which is resolved by extracting
        // its value from the closest containing closure scope.
        if containing_scope.fn_depth != self.current_scope().fn_depth {
            let closure_scope = self.scopes.iter().rev().find(|sc| sc.closure_context.is_some());
            let closure_scope = closure_scope.expect(&format!("Internal error: no expected closure scope for closed-over variable '{}'", var_name));
            let closure_ctx = closure_scope.closure_context.as_ref().unwrap();

            let var_env_idx = closure_ctx.captured_variables.get(var_name).expect(&format!("Internal error: missing index for captured variable '{}' in closure scope", var_name));
            let var_ptr = unsafe { self.builder.build_gep(closure_ctx.env, &[self.context.i32_type().const_int(*var_env_idx as u64, false)], "") };
            let var_ptr = self.builder.build_load(var_ptr, "").into_int_value();
            let var_ptr = self.emit_extract_nan_tagged_obj(var_ptr);
            return self.builder.build_pointer_cast(var_ptr, self.value_t().ptr_type(AddressSpace::Generic), "");
        }

        // Otherwise, consider the non-closed-over variable in its non-closure scope.
        let Variable { local_ptr, is_captured, .. } = containing_scope.variables.get(var_name).expect(&format!("Internal error: variable '{}' not present in its containing scope", var_name));
        if *is_captured {
            let val = self.builder.build_load(*local_ptr, "");
            let ptr = self.emit_extract_nan_tagged_obj(val.into_int_value());
            self.builder.build_pointer_cast(ptr, self.value_t().ptr_type(AddressSpace::Generic), "")
        } else {
            local_ptr.clone()
        }
    }

    fn find_captured_variables(
        &self,
        args: &Vec<(/* name: */ String, /* default_value: */ Option<TypedAstNode>)>,
        body: &Vec<TypedAstNode>,
    ) -> HashMap<String, usize> {
        let mut context = HashSet::new();
        context.insert("println".to_string());
        context.insert("print".to_string());
        context.insert("None".to_string());

        for type_name in self.scopes[0].types.keys() {
            context.insert(type_name.clone());
        }
        for enum_name in self.scopes[0].enums.keys() {
            context.insert(enum_name.clone());
        }

        CapturedVariableFinder::find_captured_variables(context, args, body)
    }

    fn compile_function_start(
        &mut self,
        fn_name: &String,
        type_name: Option<&String>,
        captured_variables: HashMap<String, usize>,
        args: Vec<(/* name: */ String, /* default_value: */ Option<TypedAstNode>)>,
        has_return: bool,
    ) -> Result<(FunctionValue<'ctx>, PointerValue<'ctx>, Option<PointerValue<'ctx>>), CompilerError> {
        let is_method = type_name.is_some();

        let namespace = self.scopes.iter().map(|s| &s.name).join("::");
        let official_fn_name = if let Some(type_name) = type_name {
            format!("{}__{}", &type_name, fn_name)
        } else {
            fn_name.clone()
        };
        let fully_qualified_fn_name = format!("{}::{}", namespace, official_fn_name);

        let num_captured_variables = captured_variables.len();
        let is_closure = !captured_variables.is_empty();

        let fn_type = self.gen_llvm_fn_type(has_return, args.len());
        let func = self.module.add_function(&fully_qualified_fn_name, fn_type, Some(Linkage::Private));
        let fn_local = if !is_method {
            let local_ptr = self.builder.build_alloca(self.value_t(), &official_fn_name);
            self.current_scope_mut().variables.insert(official_fn_name.clone(), Variable { local_ptr, is_captured: false });
            Some(local_ptr)
        } else { None };

        let env_mem = if !is_closure {
            self.current_scope_mut().fns.insert(official_fn_name.clone(), func);

            self.value_t_ptr().const_zero() // Pass `NULL` as env if non-closure
        } else {
            // Allocate space for closure's `env`, lifting variables from the current scope if necessary
            let env_mem = self.builder.build_call(
                self.cached_fn(FN_MALLOC),
                &[self.builder.build_int_mul(self.value_t().size_of(), self.context.i64_type().const_int(num_captured_variables as u64, false), "").into()],
                ""
            ).try_as_basic_value().left().unwrap().into_pointer_value();
            let env_mem = self.builder.build_pointer_cast(env_mem, self.value_t().ptr_type(AddressSpace::Generic), "");

            for (name, idx) in &captured_variables {
                let var = self.scopes.iter_mut().rev()
                    .find_map(|sc| sc.variables.get_mut(name))
                    .expect(&format!("Internal error: could not find captured variable '{}' in outer scope", name));
                let needs_lift = if !var.is_captured {
                    var.is_captured = true;
                    true
                } else {
                    false
                };

                // If necessary, "lift" the variable from its current location as a stack local to the heap,
                // and overwrite stack local value with pointer to heap value. Subsequent accesses of this
                // variable will need to make an extra dereference through this pointer (see `resolve_ptr_to_variable`).
                let val = if needs_lift {
                    let local_ptr = var.local_ptr;
                    let val = self.builder.build_load(local_ptr, "");
                    let lifted_val_mem = self.builder.build_call(self.cached_fn(FN_MALLOC), &[self.value_t().size_of().into()], "").try_as_basic_value().left().unwrap().into_pointer_value();
                    let lifted_val_mem = self.builder.build_pointer_cast(lifted_val_mem, self.value_t().ptr_type(AddressSpace::Generic), "");
                    self.builder.build_store(lifted_val_mem, val);
                    let lifted_val = self.emit_nan_tagged_obj(lifted_val_mem).as_basic_value_enum();
                    self.builder.build_store(local_ptr, lifted_val);
                    lifted_val
                } else {
                    let ptr = self.resolve_ptr_to_variable(&name);
                    self.emit_nan_tagged_obj(ptr).as_basic_value_enum()
                };

                let env_slot = unsafe { self.builder.build_gep(env_mem, &[self.context.i32_type().const_int(*idx as u64, false)], "") };
                self.builder.build_store(env_slot, val);
            }

            env_mem
        };

        let fn_bb = self.context.append_basic_block(func, "fn_body");
        self.builder.position_at_end(fn_bb);
        self.cur_fn = func;

        let closure_context = if is_closure {
            let env = func.get_nth_param(0).unwrap().into_pointer_value();
            Some(ClosureContext { env, captured_variables })
        } else {
            None
        };
        let num_received_args = func.get_nth_param(1).unwrap().into_int_value();
        self.begin_new_fn_scope(official_fn_name.clone(), closure_context);
        for (mut idx, (name, default_value)) in args.into_iter().enumerate() {
            idx += 2; // Skip over `env` and `num_received_args` params

            let param_ptr = self.builder.build_alloca(self.value_t(), &name);
            self.current_scope_mut().variables.insert(name, Variable { local_ptr: param_ptr, is_captured: false });

            // If there's a default value for the parameter assign it, making sure to only evaluate the default expression if necessary
            let param_val = func.get_nth_param(idx as u32)
                .expect(&format!("Internal error: expected parameter idx {} to exist for function {}", idx, &official_fn_name));
            let param_val = if let Some(default_value) = default_value {
                // Supply the default value for the parameter if it was either explicitly/implicitly omitted.
                // Explicit omission:
                //   func foo(i = 0): Int = i
                //   foo() // A `None` will be explicitly passed in this case, since the required arity of `foo` is known at compile-time
                // Implicit omission:
                //   func foo(i = 0): Int = i
                //   func bar(fn: () => Int): Int = fn()
                //   bar(foo) // Within the body of `bar`, we cannot know the required arity of its argument, and thus we cannot emit `None` values to pass
                //
                // $param = if num_received_args < $param_idx || $param == None { $default } else { $param }
                let cond = self.builder.build_or(
                    self.builder.build_int_compare(IntPredicate::ULE, num_received_args, self.context.i8_type().const_int((idx - 2) as u64, false),  ""),
                    self.builder.build_int_compare(IntPredicate::EQ, param_val.into_int_value(), self.val_none(), ""),
                    "cond"
                );
                let then_bb = self.context.append_basic_block(self.cur_fn, "then");
                let else_bb = self.context.append_basic_block(self.cur_fn, "else");
                let cont_bb = self.context.append_basic_block(self.cur_fn, "cont");
                self.builder.build_conditional_branch(cond, then_bb, else_bb);

                self.builder.position_at_end(then_bb);
                let then_val = self.visit(default_value)?;
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(else_bb);
                let else_val = param_val;
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(cont_bb);
                let phi = self.builder.build_phi(self.value_t(), "");
                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
                phi.as_basic_value()
            } else {
                param_val
            };
            self.builder.build_store(param_ptr, param_val);
        }

        Ok((func, env_mem, fn_local))
    }

    fn compile_function_end(
        &mut self,
        fn_name: &String,
        func: FunctionValue<'ctx>,
        env_mem: PointerValue<'ctx>,
        fn_local: Option<PointerValue<'ctx>>,
        is_recursive: bool,
        fn_decl_site_bb: BasicBlock<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        // self.builder.position_at_end(self.cur_fn.get_last_basic_block().unwrap());
        self.builder.position_at_end(fn_decl_site_bb);
        self.end_scope();

        let fn_name_val = self.builder.build_global_string_ptr(&fn_name, "").as_pointer_value().into();
        let fn_ptr_val = self.builder.build_cast(InstructionOpcode::PtrToInt, func.as_global_value().as_pointer_value(), self.value_t(), "").into();
        let fn_val = self.builder.build_call(self.cached_fn(FN_FUNCTION_ALLOC), &[fn_name_val, fn_ptr_val, env_mem.into()], "").try_as_basic_value().left().unwrap();
        if let Some(fn_local) = fn_local {
            if is_recursive {
                let val = self.builder.build_load(fn_local, "");
                let ptr = self.emit_extract_nan_tagged_obj(val.into_int_value());
                let ptr = self.builder.build_pointer_cast(ptr, self.value_t().ptr_type(AddressSpace::Generic), "");
                self.builder.build_store(ptr, fn_val);
            } else {
                self.builder.build_store(fn_local, fn_val);
            }
        }

        fn_val
    }

    fn compile_function(
        &mut self,
        fn_name: String,
        type_name: Option<&String>,
        node: TypedFunctionDeclNode,
    ) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let old_fn = self.cur_fn;
        let fn_decl_site_bb = self.builder.get_insert_block().unwrap();
        let has_return = node.ret_type != Type::Unit;

        let args = node.args.into_iter()
            .map(|(tok, _, _, default_value)| (Token::get_ident_name(&tok), default_value))
            .collect();
        let captured_variables = self.find_captured_variables(&args, &node.body);
        let (func, env_mem, fn_local) = self.compile_function_start(&fn_name, type_name, captured_variables, args, has_return)?;

        let body_len = node.body.len();
        for (idx, node) in node.body.into_iter().enumerate() {
            let value = self.visit(node)?;

            if idx == body_len - 1 {
                if has_return {
                    self.builder.build_return(Some(&value.as_basic_value_enum()));
                } else {
                    self.builder.build_return(None);
                }
            }
        }

        self.cur_fn = old_fn;
        let fn_val = self.compile_function_end(&fn_name, func, env_mem, fn_local, node.is_recursive, fn_decl_site_bb);

        Ok(fn_val)
    }
}

impl<'a, 'ctx> TypedAstVisitor<BasicValueEnum<'ctx>, CompilerError> for Compiler<'a, 'ctx> {
    fn visit_literal(&mut self, _token: Token, node: TypedLiteralNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let value = match node {
            TypedLiteralNode::IntLiteral(v) => {
                if v > (i32::MAX as i64) || v < (i32::MIN as i64) {
                    unimplemented!()
                }
                self.emit_nan_tagged_int_const(v as i32).as_basic_value_enum()
            },
            TypedLiteralNode::FloatLiteral(v) => self.emit_nan_tagged_float_const(v).as_basic_value_enum(),
            TypedLiteralNode::StringLiteral(v) => self.alloc_const_string_obj(v.as_str()).as_basic_value_enum(),
            TypedLiteralNode::BoolLiteral(v) => self.emit_nan_tagged_bool_const(v).as_basic_value_enum(),
        };

        Ok(value)
    }

    fn visit_unary(&mut self, _token: Token, node: TypedUnaryNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let value = match node.op {
            UnaryOp::Minus => {
                let is_int = if node.expr.get_type() == Type::Int { true } else { false };
                let value = self.visit(*node.expr)?;
                if is_int {
                    let value = self.emit_extract_nan_tagged_int(value.into_int_value());
                    let res = self.builder.build_int_neg(value, "");
                    self.emit_nan_tagged_int(res).as_basic_value_enum()
                } else {
                    let value = self.emit_extract_nan_tagged_float(value.into_int_value());
                    let res = self.builder.build_float_neg(value, "");
                    self.emit_nan_tagged_float(res).as_basic_value_enum()
                }
            }
            UnaryOp::Negate => {
                let value = self.visit(*node.expr)?;
                self.build_bool_negate(value)
            }
        };

        Ok(value)
    }

    fn visit_binary(&mut self, _token: Token, node: TypedBinaryNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let ltype = node.left.get_type();
        let rtype = node.right.get_type();

        let value = match (ltype, rtype, &node.op) {
            (Type::Int, Type::Int, op) => {
                let left = self.visit(*node.left)?;
                let left = self.emit_extract_nan_tagged_int(left.into_int_value());
                let right = self.visit(*node.right)?;
                let right = self.emit_extract_nan_tagged_int(right.into_int_value());

                match op {
                    BinaryOp::Add => self.emit_nan_tagged_int(self.builder.build_int_add(left, right, "")).as_basic_value_enum(),
                    BinaryOp::Sub => self.emit_nan_tagged_int(self.builder.build_int_sub(left, right, "")).as_basic_value_enum(),
                    BinaryOp::Mul => self.emit_nan_tagged_int(self.builder.build_int_mul(left, right, "")).as_basic_value_enum(),
                    BinaryOp::Div => {
                        let left = self.builder.build_signed_int_to_float(left, self.context.f64_type(), "left");
                        let right = self.builder.build_signed_int_to_float(right, self.context.f64_type(), "right");

                        let res = self.builder.build_float_div(left, right, "");
                        self.emit_nan_tagged_float(res).as_basic_value_enum()
                    },
                    BinaryOp::Mod => self.emit_nan_tagged_int(self.builder.build_int_signed_rem(left, right, "")).as_basic_value_enum(),
                    BinaryOp::Pow => {
                        let left = self.builder.build_signed_int_to_float(left, self.context.f64_type(), "left");
                        let right = self.builder.build_signed_int_to_float(right, self.context.f64_type(), "right");

                        let res = self.builder.build_call(
                            self.cached_fn(FN_POWF64),
                            &[left.into(), right.into()],
                            "",
                        ).try_as_basic_value().left().unwrap().into_float_value();
                        self.emit_nan_tagged_float(res).as_basic_value_enum()
                    }
                    BinaryOp::Eq => self.emit_nan_tagged_bool(self.builder.build_int_compare(IntPredicate::EQ, left, right, "")).as_basic_value_enum(),
                    BinaryOp::Neq => self.emit_nan_tagged_bool(self.builder.build_int_compare(IntPredicate::NE, left, right, "")).as_basic_value_enum(),
                    BinaryOp::Gt => self.emit_nan_tagged_bool(self.builder.build_int_compare(IntPredicate::SGT, left, right, "")).as_basic_value_enum(),
                    BinaryOp::Gte => self.emit_nan_tagged_bool(self.builder.build_int_compare(IntPredicate::SGE, left, right, "")).as_basic_value_enum(),
                    BinaryOp::Lt => self.emit_nan_tagged_bool(self.builder.build_int_compare(IntPredicate::SLT, left, right, "")).as_basic_value_enum(),
                    BinaryOp::Lte => self.emit_nan_tagged_bool(self.builder.build_int_compare(IntPredicate::SLE, left, right, "")).as_basic_value_enum(),
                    BinaryOp::Coalesce => unreachable!("Coalesce op does not apply to 2 non-optionals"),
                    BinaryOp::And | BinaryOp::Or | BinaryOp::Xor | BinaryOp::AndEq | BinaryOp::OrEq => unreachable!("No boolean ops apply to numbers"),
                    BinaryOp::AddEq | BinaryOp::SubEq | BinaryOp::MulEq | BinaryOp::DivEq | BinaryOp::ModEq | BinaryOp::CoalesceEq => unreachable!("Assign ops are handled separately")
                }
            }
            (ltype @ Type::Float, rtype @ Type::Int, op) |
            (ltype @ Type::Int, rtype @ Type::Float, op) |
            (ltype @ Type::Float, rtype @ Type::Float, op) => {
                let left = self.visit(*node.left)?;
                let right = self.visit(*node.right)?;

                let left = if ltype == Type::Int {
                    let left = self.emit_extract_nan_tagged_int(left.into_int_value());
                    self.builder.build_signed_int_to_float(left, self.context.f64_type(), "left")
                } else { self.emit_extract_nan_tagged_float(left.into_int_value()) };
                let right = if rtype == Type::Int {
                    let right = self.emit_extract_nan_tagged_int(right.into_int_value());
                    self.builder.build_signed_int_to_float(right, self.context.f64_type(), "right")
                } else { self.emit_extract_nan_tagged_float(right.into_int_value()) };

                match op {
                    BinaryOp::Add => self.emit_nan_tagged_float(self.builder.build_float_add(left, right, "")).as_basic_value_enum(),
                    BinaryOp::Sub => self.emit_nan_tagged_float(self.builder.build_float_sub(left, right, "")).as_basic_value_enum(),
                    BinaryOp::Mul => self.emit_nan_tagged_float(self.builder.build_float_mul(left, right, "")).as_basic_value_enum(),
                    BinaryOp::Div => self.emit_nan_tagged_float(self.builder.build_float_div(left, right, "")).as_basic_value_enum(),
                    BinaryOp::Mod => self.emit_nan_tagged_float(self.builder.build_float_rem(left, right, "")).as_basic_value_enum(),
                    BinaryOp::Pow => {
                        // if a < 0 { -(-a ** b) } else { a ** b }
                        let cond = self.builder.build_float_compare(FloatPredicate::OLT, left, self.context.f64_type().const_float(0.0).into(), "cond");
                        let function = self.cur_fn;
                        let then_bb = self.context.append_basic_block(function, "then");
                        let else_bb = self.context.append_basic_block(function, "else");
                        let cont_bb = self.context.append_basic_block(function, "cont");
                        self.builder.build_conditional_branch(cond, then_bb, else_bb);

                        self.builder.position_at_end(then_bb);
                        let then_val = {
                            let left = self.builder.build_float_mul(left, self.context.f64_type().const_float(-1.0), "");
                            let left = self.builder.build_call(
                                self.cached_fn(FN_POWF64),
                                &[left.into(), right.into()],
                                "",
                            ).try_as_basic_value().left().unwrap().into_float_value();
                            self.builder.build_float_mul(left, self.context.f64_type().const_float(-1.0), "")
                        };
                        self.builder.build_unconditional_branch(cont_bb);
                        let then_bb = self.builder.get_insert_block().unwrap();

                        self.builder.position_at_end(else_bb);
                        let else_val = self.builder.build_call(
                            self.cached_fn(FN_POWF64),
                            &[left.into(), right.into()],
                            "",
                        ).try_as_basic_value().left().unwrap();
                        self.builder.build_unconditional_branch(cont_bb);
                        let else_bb = self.builder.get_insert_block().unwrap();

                        self.builder.position_at_end(cont_bb);
                        let phi = self.builder.build_phi(self.context.f64_type(), "");
                        phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
                        self.emit_nan_tagged_float(phi.as_basic_value().into_float_value()).as_basic_value_enum()
                    }
                    BinaryOp::Eq =>  self.emit_nan_tagged_bool(self.builder.build_float_compare(FloatPredicate::OEQ, left, right, "")).as_basic_value_enum(),
                    BinaryOp::Neq => self.emit_nan_tagged_bool(self.builder.build_float_compare(FloatPredicate::ONE, left, right, "")).as_basic_value_enum(),
                    BinaryOp::Gt =>  self.emit_nan_tagged_bool(self.builder.build_float_compare(FloatPredicate::OGT, left, right, "")).as_basic_value_enum(),
                    BinaryOp::Gte => self.emit_nan_tagged_bool(self.builder.build_float_compare(FloatPredicate::OGE, left, right, "")).as_basic_value_enum(),
                    BinaryOp::Lt => self.emit_nan_tagged_bool(self.builder.build_float_compare(FloatPredicate::OLT, left, right, "")).as_basic_value_enum(),
                    BinaryOp::Lte => self.emit_nan_tagged_bool(self.builder.build_float_compare(FloatPredicate::OLE, left, right, "")).as_basic_value_enum(),
                    BinaryOp::Coalesce => unreachable!("Coalesce op does not apply to 2 non-optionals"),
                    BinaryOp::And | BinaryOp::Or | BinaryOp::Xor | BinaryOp::AndEq | BinaryOp::OrEq => unreachable!("No boolean ops apply to numbers"),
                    BinaryOp::AddEq | BinaryOp::SubEq | BinaryOp::MulEq | BinaryOp::DivEq | BinaryOp::ModEq | BinaryOp::CoalesceEq => unreachable!("Assign ops are handled separately")
                }
            }
            (Type::Bool, Type::Bool, op) => {
                let left = self.visit(*node.left)?;
                let left = self.emit_extract_nan_tagged_bool(left.into_int_value());
                let right = self.visit(*node.right)?;
                let right = self.emit_extract_nan_tagged_bool(right.into_int_value());

                let res = match op {
                    BinaryOp::Eq => self.builder.build_int_compare(IntPredicate::EQ, left, right, "").into(),
                    BinaryOp::Neq => self.builder.build_int_compare(IntPredicate::NE, left, right, "").into(),
                    BinaryOp::And | BinaryOp::Or => unreachable!("`and` & `or` handled as if-expressions for short-circuiting"),
                    BinaryOp::Xor => self.builder.build_xor(left, right, "").into(),
                    BinaryOp::Coalesce => unreachable!("Coalesce op does not apply to 2 non-optionals"),
                    BinaryOp::AddEq | BinaryOp::SubEq | BinaryOp::MulEq | BinaryOp::DivEq | BinaryOp::ModEq | BinaryOp::AndEq | BinaryOp::OrEq | BinaryOp::CoalesceEq => unreachable!("Assign ops are handled separately"),
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod | BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte | BinaryOp::Pow => unreachable!("No arithmetic ops apply to boolean values"),
                };
                self.emit_nan_tagged_bool(res).as_basic_value_enum()
            }
            (Type::String, _, BinaryOp::Add) | (_, Type::String, BinaryOp::Add) => {
                let left = self.visit(*node.left)?;
                let right = self.visit(*node.right)?;

                self.builder.build_call(self.cached_fn(FN_STRING_CONCAT), &[left.into(), right.into()], "").try_as_basic_value().left().unwrap()
            }
            (Type::Option(_), _, BinaryOp::Coalesce) => {
                let left = self.visit(*node.left)?;

                let cond = self.builder.build_int_compare(IntPredicate::EQ, left.into_int_value(), self.val_none(), "cond");
                let then_bb = self.context.append_basic_block(self.cur_fn, "then");
                let else_bb = self.context.append_basic_block(self.cur_fn, "else");
                let cont_bb = self.context.append_basic_block(self.cur_fn, "cont");
                self.builder.build_conditional_branch(cond, then_bb, else_bb);

                self.builder.position_at_end(then_bb);
                let then_val = self.visit(*node.right)?;
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(else_bb);
                let else_val = left;
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(cont_bb);
                let phi = self.builder.build_phi(self.value_t(), "");
                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
                phi.as_basic_value()
            }
            (_, _, op @ BinaryOp::Eq) | (_, _, op @ BinaryOp::Neq) => {
                let left = self.visit(*node.left)?;
                let right = self.visit(*node.right)?;

                let res = self.builder.build_call(self.cached_fn(FN_VALUE_EQ), &[left.into(), right.into()], "").try_as_basic_value().left().unwrap();
                let res = self.emit_nan_tagged_bool(res.into_int_value());
                if op == &BinaryOp::Neq {
                    self.build_bool_negate(res.into())
                } else {
                    res.into()
                }
            }
            _ => todo!()
        };

        Ok(value)
    }

    fn visit_grouped(&mut self, _token: Token, node: TypedGroupedNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        self.visit(*node.expr)
    }

    fn visit_array(&mut self, _token: Token, node: TypedArrayNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let len = node.items.len();
        let length_val = self.context.i32_type().const_int(len as u64, false);
        let arr_value = self.alloc_array_obj_with_length(length_val);

        for (idx, item) in node.items.into_iter().enumerate() {
            let index_val = self.context.i32_type().const_int(idx as u64, false);
            let item_val = self.visit(item)?;
            self.array_obj_insert(arr_value, index_val, item_val.into_int_value());
        }

        Ok(arr_value.into())
    }

    fn visit_tuple(&mut self, _token: Token, node: TypedTupleNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let tuple_items = node.items.into_iter()
            .map(|n| self.visit(n).map(|r| r.into()))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(self.alloc_tuple_obj(tuple_items).into())
    }

    fn visit_map(&mut self, _token: Token, node: TypedMapNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let entries = node.items.into_iter()
            .map(|(key, value)| {
                self.visit(key)
                    .and_then(|key| self.visit(value).map(|value| (key, value)))
            })
            .collect::<Result<Vec<_>, _>>();
        let entries = entries?;

        Ok(self.alloc_map_obj(entries).into())
    }

    fn visit_set(&mut self, _token: Token, node: TypedSetNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let values = node.items.into_iter()
            .map(|value| self.visit(value))
            .collect::<Result<Vec<_>, _>>();
        let values = values?;

        Ok(self.alloc_set_obj(values).into())
    }

    fn visit_lambda(&mut self, _token: Token, node: TypedLambdaNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let TypedLambdaNode { typ, idx, args, typed_body, .. } = node;
        let body = typed_body.unwrap();
        let fn_name = format!("_${}", idx);
        let has_return = if let Type::Fn(FnType { ret_type, .. }) = typ { *ret_type != Type::Unit } else { unreachable!() };

        let old_fn = self.cur_fn;
        let fn_decl_site_bb = self.builder.get_insert_block().unwrap();

        let args = args.into_iter().map(|(tok, _, default_value)| (Token::get_ident_name(&tok), default_value)).collect();
        let captured_variables = self.find_captured_variables(&args, &body);
        let (func, env_mem, fn_local) = self.compile_function_start(&fn_name, None, captured_variables, args, has_return)?;

        let body_len = body.len();
        for (idx, node) in body.into_iter().enumerate() {
            let value = self.visit(node)?;

            if idx == body_len - 1 {
                if has_return {
                    self.builder.build_return(Some(&value.as_basic_value_enum()));
                } else {
                    self.builder.build_return(None);
                }
            }
        }

        self.cur_fn = old_fn;
        let fn_val = self.compile_function_end(&fn_name, func, env_mem, fn_local, false, fn_decl_site_bb);

        Ok(fn_val)
    }

    fn visit_binding_decl(&mut self, _token: Token, node: TypedBindingDeclNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let expr = if let Some(expr) = node.expr {
            self.visit(*expr)?
        } else {
            self.val_none().as_basic_value_enum()
        };

        self.visit_binding_pattern(node.binding, expr);

        Ok(self.val_none().as_basic_value_enum())
    }

    fn visit_function_decl(&mut self, _token: Token, node: TypedFunctionDeclNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let fn_name = Token::get_ident_name(&node.name);

        self.compile_function(fn_name, None, node)?;

        let res = self.emit_nan_tagged_int_const(0);
        Ok(res.into())
    }

    fn visit_type_decl(&mut self, _token: Token, node: TypedTypeDeclNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let num_fields = node.fields.len();
        let field_names = node.fields.iter().map(|f| Token::get_ident_name(&f.ident)).collect_vec();
        let type_name = Token::get_ident_name(&node.name);

        let struct_type = self.context.opaque_struct_type(&type_name);
        let mut field_types = vec![self.cached_type(TYPE_OBJ_HEADER).into()];
        field_types.append(&mut repeat(self.value_t().into()).take(num_fields).collect_vec());
        struct_type.set_body(field_types.as_slice(), false);

        let type_id = self.incr_next_type_id();
        let type_id_global = self.module.add_global(self.context.i32_type(), None, &format!("type_id_{}", &type_name));
        type_id_global.set_linkage(Linkage::Common);
        type_id_global.set_initializer(&self.context.i32_type().const_zero());
        self.builder.build_store(type_id_global.as_pointer_value(), type_id);

        // Generate constructor function
        let ctor_fn = {
            let old_fn = self.cur_fn;

            let ctor_fn_type = self.value_t().fn_type(repeat(self.value_t().into()).take(num_fields).collect_vec().as_slice(), false);
            let ctor_fn = self.module.add_function(&format!("{}__{}_new", &self.module_name, &type_name), ctor_fn_type, None);
            self.cur_fn = ctor_fn;
            let fn_bb = self.context.append_basic_block(self.cur_fn, "fn_body");
            self.builder.position_at_end(fn_bb);

            let struct_size = self.builder.build_int_add(
                self.cached_type(TYPE_OBJ_HEADER).size_of().unwrap(),
                self.builder.build_int_mul(
                    self.value_t().size_of(),
                    self.context.i64_type().const_int(num_fields as u64, false),
                    "",
                ),
                "",
            );
            let mem = self.builder.build_call(self.cached_fn(FN_MALLOC), &[struct_size.into()], "").try_as_basic_value().left().unwrap().into_pointer_value();
            let mem = self.builder.build_pointer_cast(mem, struct_type.ptr_type(AddressSpace::Generic), "");

            let header_slot = self.builder.build_struct_gep(mem, 0, "").unwrap();
            let header_type_id_slot = self.builder.build_struct_gep(header_slot, 0, "").unwrap();
            let header_type_id_slot = self.builder.build_pointer_cast(header_type_id_slot, self.context.i32_type().ptr_type(AddressSpace::Generic), "");
            self.builder.build_store(header_type_id_slot, self.builder.build_load(type_id_global.as_pointer_value(), ""));

            for idx in 0..num_fields {
                let slot = self.builder.build_struct_gep(mem, (idx + 1) as u32, "").unwrap();
                let field_value = self.cur_fn.get_nth_param(idx as u32).unwrap();
                self.builder.build_store(slot, field_value);
            }
            self.builder.build_return(Some(&self.emit_nan_tagged_obj(mem).as_basic_value_enum()));
            self.cur_fn = old_fn;
            self.builder.position_at_end(self.cur_fn.get_last_basic_block().unwrap());

            ctor_fn
        };

        let type_spec = TypeSpec { typ: node.self_type, type_id, struct_type, ctor_fn };
        self.current_scope_mut().types.insert(type_name.clone(), type_spec);

        let num_methods = node.methods.len();
        let vtable_entry = self.builder.build_call(
            self.cached_fn(FN_VTABLE_ALLOC_ENTRY),
            &[
                type_id.into(),
                self.context.i32_type().const_int((num_methods + 3) as u64, false).into()
            ],
            "",
        ).try_as_basic_value().left().unwrap().into_pointer_value();

        // Find the potential user-defined toString method definition for the current type
        let (mut tostring_method_defs, method_defs): (Vec<_>, Vec<_>) = node.methods.into_iter().partition(|(name, _)| name == "toString");
        debug_assert!(tostring_method_defs.len() <= 1);

        // Generate toString method, using user-defined method if provided; otherwise generate the default
        let tostring_fn_val = if let Some(tostring_method_def) = tostring_method_defs.pop() {
            let (name, node) = tostring_method_def;
            let decl_node = if let TypedAstNode::FunctionDecl(_, decl_node) = node { decl_node } else { unreachable!() };
            self.compile_function(name, Some(&type_name), decl_node)?
        } else {
            let old_fn = self.cur_fn;

            let tostring_fn_type = self.gen_llvm_fn_type(true, 1);
            let tostring_fn = self.module.add_function(&format!("{}__{}__toString", &self.module_name, &type_name), tostring_fn_type, None);
            self.cur_fn = tostring_fn;
            let fn_bb = self.context.append_basic_block(self.cur_fn, "fn_body");
            self.builder.position_at_end(fn_bb);

            let self_arg = self.cur_fn.get_nth_param(2).unwrap();
            let self_arg = self.emit_extract_nan_tagged_obj(self_arg.into_int_value());
            let self_arg = self.builder.build_pointer_cast(self_arg, struct_type.ptr_type(AddressSpace::Generic), "");

            let field_strs = (0..num_fields).into_iter()
                .map(|idx| {
                    let field_value_ptr = self.builder.build_struct_gep(self_arg, (idx + 1) as u32, "").unwrap();
                    let field_value = self.builder.build_load(field_value_ptr, "");
                    let str_val = self.builder.build_call(self.cached_fn(FN_VALUE_TO_STRING), &[field_value.into()], "").try_as_basic_value().left().unwrap();
                    let res_val = self.emit_extract_nan_tagged_obj(str_val.into_int_value());
                    let res_str = self.builder.build_cast(InstructionOpcode::BitCast, res_val, self.cached_type(TYPE_STRING).ptr_type(AddressSpace::Generic), "").into_pointer_value();
                    let chars_ptr = self.builder.build_struct_gep(res_str, 2, "").unwrap();

                    self.builder.build_load(chars_ptr, "").into()
                })
                .collect_vec();
            let fmt_str = format!("{}({})", &type_name, field_names.iter().map(|name| format!("{}: %s", name)).join(", "));
            let fmt_str_val = self.builder.build_global_string_ptr(&fmt_str, "").as_basic_value_enum();
            let mut args = vec![self.str_type().const_zero().into(), self.context.i32_type().const_zero().into(), fmt_str_val.into()];
            args.append(&mut field_strs.clone());
            let len_val = self.builder.build_call(self.cached_fn(FN_SNPRINTF), args.as_slice(), "").try_as_basic_value().left().unwrap().into_int_value();
            let len_val = self.builder.build_int_add(len_val, self.context.i64_type().const_int(1, false), "");
            let str_mem = self.builder.build_call(self.cached_fn(FN_MALLOC), &[len_val.into()], "").try_as_basic_value().left().unwrap().into_pointer_value();
            let len_val = self.builder.build_int_cast(len_val, self.context.i32_type(), "");
            let mut args = vec![str_mem.into(), len_val.into(), fmt_str_val.into()];
            args.append(&mut field_strs.clone());
            self.builder.build_call(self.cached_fn(FN_SNPRINTF), args.as_slice(), "");

            let len_val = self.builder.build_int_sub(len_val, self.context.i32_type().const_int(1, false), "");
            let str_repr = self.alloc_string_obj(len_val, str_mem);
            self.builder.build_return(Some(&str_repr.as_basic_value_enum()));

            self.cur_fn = old_fn;
            self.builder.position_at_end(self.cur_fn.get_last_basic_block().unwrap());

            let fn_ptr = tostring_fn.as_global_value().as_pointer_value();
            let fn_name_val = self.builder.build_global_string_ptr("toString", "").as_pointer_value().into();
            let fn_ptr_val = self.builder.build_cast(InstructionOpcode::PtrToInt, fn_ptr, self.value_t(), "").into();
            self.builder.build_call(self.cached_fn(FN_FUNCTION_ALLOC), &[fn_name_val, fn_ptr_val, self.value_t_ptr().const_zero().into()], "").try_as_basic_value().left().unwrap()
        };
        let slot = unsafe { self.builder.build_gep(vtable_entry, &[self.context.i32_type().const_int(0, false)], "") };
        self.builder.build_store(slot, self.builder.build_cast(InstructionOpcode::PtrToInt, tostring_fn_val, self.context.i64_type(), ""));

        // Generate eq method
        let eq_fn = {
            let old_fn = self.cur_fn;

            let eq_fn_type = self.gen_llvm_fn_type(true, 2);
            let eq_fn = self.module.add_function(&format!("{}__{}__eq", &self.module_name, &type_name), eq_fn_type, None);
            self.cur_fn = eq_fn;
            let fn_bb = self.context.append_basic_block(self.cur_fn, "fn_body");
            self.builder.position_at_end(fn_bb);

            let self_arg = self.cur_fn.get_nth_param(2).unwrap();
            let self_arg = self.emit_extract_nan_tagged_obj(self_arg.into_int_value());
            let self_arg = self.builder.build_pointer_cast(self_arg, struct_type.ptr_type(AddressSpace::Generic), "");

            let other_arg = self.cur_fn.get_nth_param(3).unwrap();
            let other_arg = self.emit_extract_nan_tagged_obj(other_arg.into_int_value());
            let other_arg = self.builder.build_pointer_cast(other_arg, struct_type.ptr_type(AddressSpace::Generic), "");

            for idx in 0..num_fields {
                let f1_ptr = self.builder.build_struct_gep(self_arg, (idx + 1) as u32, "").unwrap();
                let f1_val = self.builder.build_load(f1_ptr, "");
                let f2_ptr = self.builder.build_struct_gep(other_arg, (idx + 1) as u32, "").unwrap();
                let f2_val = self.builder.build_load(f2_ptr, "");
                let cond_val = self.builder.build_call(
                    self.cached_fn(FN_VALUE_EQ),
                    &[f1_val.into(), f2_val.into()],
                    ""
                ).try_as_basic_value().left().unwrap().into_int_value();
                let cond = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    cond_val,
                    self.context.bool_type().const_zero(),
                    ""
                );

                let then_bb = self.context.append_basic_block(self.cur_fn, "then");
                let cont_bb = self.context.append_basic_block(self.cur_fn, "cont");
                self.builder.build_conditional_branch(cond, then_bb, cont_bb);

                self.builder.position_at_end(then_bb);
                self.builder.build_return(Some(&self.context.i64_type().const_int(VAL_FALSE, false)));

                self.builder.position_at_end(cont_bb);
            }
            self.builder.build_return(Some(&self.context.i64_type().const_int(VAL_TRUE, false)));

            self.cur_fn = old_fn;
            self.builder.position_at_end(self.cur_fn.get_last_basic_block().unwrap());

            eq_fn
        };
        let fn_ptr = eq_fn.as_global_value().as_pointer_value();
        let slot = unsafe { self.builder.build_gep(vtable_entry, &[self.context.i32_type().const_int(1, false)], "") };
        self.builder.build_store(slot, self.builder.build_cast(InstructionOpcode::PtrToInt, fn_ptr, self.context.i64_type(), ""));

        // Generate hash method
        let hash_fn = {
            let old_fn = self.cur_fn;

            let hash_fn_type = self.gen_llvm_fn_type(true, 1);
            let hash_fn = self.module.add_function(&format!("{}__{}__hash", &self.module_name, &type_name), hash_fn_type, None);
            self.cur_fn = hash_fn;
            let fn_bb = self.context.append_basic_block(self.cur_fn, "fn_body");
            self.builder.position_at_end(fn_bb);

            let hash_val = self.builder.build_alloca(self.context.i32_type(), "hash");
            self.builder.build_store(hash_val, self.builder.build_load(type_id_global.as_pointer_value(), ""));

            let self_arg = self.cur_fn.get_nth_param(2).unwrap();
            let self_arg = self.emit_extract_nan_tagged_obj(self_arg.into_int_value());
            let self_arg = self.builder.build_pointer_cast(self_arg, struct_type.ptr_type(AddressSpace::Generic), "");

            for idx in 0..num_fields {
                let field_ptr = self.builder.build_struct_gep(self_arg, (idx + 1) as u32, "").unwrap();
                let field_val = self.builder.build_load(field_ptr, "");
                let field_hash = self.builder.build_call(
                    self.cached_fn(FN_VALUE_HASH),
                    &[field_val.into()],
                    ""
                ).try_as_basic_value().left().unwrap().into_int_value();

                let cur_hash_val = self.builder.build_load(hash_val, "").into_int_value();
                self.builder.build_store(
                    hash_val,
                    self.builder.build_int_add(cur_hash_val, field_hash, "")
                );
            }
            let ret_hash = self.emit_nan_tagged_int(self.builder.build_load(hash_val, "").into_int_value());
            self.builder.build_return(Some(&ret_hash));

            self.cur_fn = old_fn;
            self.builder.position_at_end(self.cur_fn.get_last_basic_block().unwrap());

            hash_fn
        };
        let fn_ptr = hash_fn.as_global_value().as_pointer_value();
        let slot = unsafe { self.builder.build_gep(vtable_entry, &[self.context.i32_type().const_int(2, false)], "") };
        self.builder.build_store(slot, self.builder.build_cast(InstructionOpcode::PtrToInt, fn_ptr, self.context.i64_type(), ""));

        // Generate user-defined methods
        for (idx, (name, decl_node)) in method_defs.into_iter().enumerate() {
            let decl_node = if let TypedAstNode::FunctionDecl(_, decl_node) = decl_node { decl_node } else { unreachable!() };
            let fn_val = self.compile_function(name, Some(&type_name), decl_node)?;
            let idx = idx + 3; // +3 to account for toString, eq, hash
            let slot = unsafe { self.builder.build_gep(vtable_entry, &[self.context.i32_type().const_int(idx as u64, false)], "") };
            self.builder.build_store(slot, fn_val);
        }

        Ok(self.val_none().as_basic_value_enum())
    }

    fn visit_enum_decl(&mut self, _token: Token, node: TypedEnumDeclNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let enum_name = Token::get_ident_name(&node.name);

        let max_num_fields = node.variants.iter()
            .map(|(_, (typ, _))| if let Type::Fn(FnType{arg_types, ..}) = typ { arg_types.len() } else { 0 })
            .max()
            .unwrap_or(0);
        let base_enum_struct_fields = vec![self.cached_type(TYPE_OBJ_HEADER).into(), self.context.i8_type().into()];
        let base_struct_fields = if max_num_fields == 0 {
            base_enum_struct_fields.clone()
        } else {
            vec![base_enum_struct_fields.clone(), vec![self.value_t().array_type(max_num_fields as u32).into()]].concat()
        };
        let base_struct_type = self.context.opaque_struct_type(&enum_name);
        base_struct_type.set_body(base_struct_fields.as_slice(), false);

        let type_id = self.incr_next_type_id();
        let type_id_global = self.module.add_global(self.context.i32_type(), None, &format!("type_id_{}", &enum_name));
        type_id_global.set_linkage(Linkage::Common);
        type_id_global.set_initializer(&self.context.i32_type().const_zero());
        self.builder.build_store(type_id_global.as_pointer_value(), type_id);

        #[inline]
        fn emit_alloc_variant<'a, 'ctx>(
            zelf: &mut Compiler<'a, 'ctx>,
            variant_idx: usize,
            variant_struct_size: IntValue<'ctx>,
            variant_struct_type: StructType<'ctx>,
            type_id_global: PointerValue<'ctx>,
        ) -> PointerValue<'ctx> {
            let mem = zelf.builder.build_call(zelf.cached_fn(FN_MALLOC), &[variant_struct_size.into()], "").try_as_basic_value().left().unwrap().into_pointer_value();
            let mem = zelf.builder.build_pointer_cast(mem, variant_struct_type.ptr_type(AddressSpace::Generic), "");
            let header_slot = zelf.builder.build_struct_gep(mem, 0, "").unwrap();
            let header_type_id_slot = zelf.builder.build_struct_gep(header_slot, 0, "").unwrap();
            let header_type_id_slot = zelf.builder.build_pointer_cast(header_type_id_slot, zelf.context.i32_type().ptr_type(AddressSpace::Generic), "");
            zelf.builder.build_store(header_type_id_slot, zelf.builder.build_load(type_id_global, ""));
            let variant_idx_slot = zelf.builder.build_struct_gep(mem, 1, "").unwrap();
            zelf.builder.build_store(variant_idx_slot, zelf.context.i8_type().const_int(variant_idx as u64, false));

            mem
        }

        // Generate values for each enum variant
        let mut variants = Vec::new();
        let mut variant_specs = Vec::new();
        for (idx, (tok, (typ, default_field_vals))) in node.variants.into_iter().enumerate() {
            let variant_name = Token::get_ident_name(&tok);
            let variant_struct_type = self.context.opaque_struct_type(&format!("{}_{}", &enum_name, &variant_name));

            let val = if let Type::Fn(FnType { arg_types, is_enum_constructor, .. }) = typ {
                debug_assert!(is_enum_constructor);
                let default_field_vals = default_field_vals.expect(&format!("Internal error: expected default_field_vals to be present for enum variant constructors"));

                let mut variant_struct_fields = base_enum_struct_fields.clone();
                variant_struct_fields.append(&mut repeat(self.value_t().into()).take(arg_types.len()).collect_vec());
                variant_struct_type.set_body(variant_struct_fields.as_slice(), false);
                let variant_struct_size = variant_struct_type.size_of().unwrap();

                let old_fn = self.cur_fn;
                let fn_decl_site_bb = self.builder.get_insert_block().unwrap();

                debug_assert!(arg_types.len() == default_field_vals.len());
                let arg_names = arg_types.iter().map(|(name, _, _)| name.clone()).collect_vec();
                let args = arg_names.iter().zip(default_field_vals)
                    .map(|(name, default_val)| (name.clone(), default_val))
                    .collect();
                let captured_variables = self.find_captured_variables(&args, &vec![]);
                let (func, env_mem, fn_local) = self.compile_function_start(&variant_name, Some(&enum_name), captured_variables, args, true)?;

                let mem = emit_alloc_variant(self, idx, variant_struct_size, variant_struct_type, type_id_global.as_pointer_value());
                let mem = self.builder.build_pointer_cast(mem, variant_struct_type.ptr_type(AddressSpace::Generic), "");

                for (idx, name) in arg_names.iter().enumerate() {
                    let ptr = self.resolve_ptr_to_variable(name);
                    let field_val = self.builder.build_load(ptr, "");
                    let slot = self.builder.build_struct_gep(mem, (idx + 2) as u32, "").unwrap();
                    self.builder.build_store(slot, field_val);
                }

                let val = self.emit_nan_tagged_obj(mem);
                self.builder.build_return(Some(&val.as_basic_value_enum()));

                self.cur_fn = old_fn;
                let variant_fn_name = format!("{}.{}", &enum_name, &variant_name);
                let fn_val = self.compile_function_end(&variant_fn_name, func, env_mem, fn_local, false, fn_decl_site_bb);

                variants.push((variant_name, variant_struct_type, arg_names));

                Either::Right(fn_val)
            } else {
                variant_struct_type.set_body(base_enum_struct_fields.as_slice(), false);
                let variant_struct_size = variant_struct_type.size_of().unwrap();

                let variant_value_global = self.module.add_global(self.value_t(), None, &format!("{}_{}", &enum_name, &variant_name));
                variant_value_global.set_linkage(Linkage::Common);
                variant_value_global.set_initializer(&self.val_none().as_basic_value_enum());

                let mem = emit_alloc_variant(self, idx, variant_struct_size, variant_struct_type, type_id_global.as_pointer_value());
                let val = self.emit_nan_tagged_obj(mem);
                self.builder.build_store(variant_value_global.as_pointer_value(), val);

                variants.push((variant_name, variant_struct_type, vec![]));

                Either::Left(variant_value_global.as_pointer_value())
            };

            variant_specs.push(EnumVariantSpec {
                variant_idx: idx,
                struct_type: variant_struct_type,
                val,
            });
        }

        let enum_spec = EnumSpec { typ: node.self_type, type_id, base_struct_type, variants: variant_specs };
        self.current_scope_mut().enums.insert(enum_name.clone(), enum_spec);

        let num_methods = node.methods.len();
        let vtable_entry = self.builder.build_call(
            self.cached_fn(FN_VTABLE_ALLOC_ENTRY),
            &[
                type_id.into(),
                self.context.i32_type().const_int((num_methods + 3) as u64, false).into()
            ],
            "",
        ).try_as_basic_value().left().unwrap().into_pointer_value();

        // Find the potential user-defined toString method definition for the current type
        let (mut tostring_method_defs, method_defs): (Vec<_>, Vec<_>) = node.methods.into_iter().partition(|(name, _)| name == "toString");
        debug_assert!(tostring_method_defs.len() <= 1);

        // Generate toString method, using user-defined method if provided; otherwise generate the default
        let tostring_fn_val = if let Some(tostring_method_def) = tostring_method_defs.pop() {
            let (name, node) = tostring_method_def;
            let decl_node = if let TypedAstNode::FunctionDecl(_, decl_node) = node { decl_node } else { unreachable!() };
            self.compile_function(name, Some(&enum_name), decl_node)?
        } else {
            let old_fn = self.cur_fn;

            let tostring_fn_type = self.gen_llvm_fn_type(true, 1);
            let tostring_fn = self.module.add_function(&format!("{}__{}__toString", &self.module_name, &enum_name), tostring_fn_type, None);
            self.cur_fn = tostring_fn;
            let fn_bb = self.context.append_basic_block(self.cur_fn, "fn_body");
            self.builder.position_at_end(fn_bb);

            let self_arg = self.cur_fn.get_nth_param(2).unwrap();
            let self_arg = self.emit_extract_nan_tagged_obj(self_arg.into_int_value());
            let self_arg = self.builder.build_pointer_cast(self_arg, base_struct_type.ptr_type(AddressSpace::Generic), "");
            let variant_idx_ptr = self.builder.build_struct_gep(self_arg, 1, "").unwrap();
            let variant_idx_val = self.builder.build_load(variant_idx_ptr, "").into_int_value();

            for (idx, (variant_name, variant_struct_type, variant_field_names)) in variants.iter().enumerate() {
                let idx_val = self.context.i8_type().const_int(idx as u64, false);
                let cond = self.builder.build_int_compare(IntPredicate::EQ, variant_idx_val, idx_val, "");

                let then_bb = self.context.append_basic_block(self.cur_fn, "then_bb");
                let cont_bb = self.context.append_basic_block(self.cur_fn, "cont_bb");
                self.builder.build_conditional_branch(cond, then_bb, cont_bb);

                self.builder.position_at_end(then_bb);
                let str_repr = if variant_field_names.is_empty() {
                    self.alloc_const_string_obj(format!("{}.{}", &enum_name, &variant_name))
                } else {
                    let self_as_variant = self.builder.build_pointer_cast(self_arg, variant_struct_type.ptr_type(AddressSpace::Generic), "");
                    let field_strs = (0..variant_field_names.len()).into_iter()
                        .map(|idx| {
                            let field_value_ptr = self.builder.build_struct_gep(self_as_variant, (idx + 2) as u32, "").unwrap();
                            let field_value = self.builder.build_load(field_value_ptr, "");
                            let str_val = self.builder.build_call(self.cached_fn(FN_VALUE_TO_STRING), &[field_value.into()], "").try_as_basic_value().left().unwrap();
                            let res_val = self.emit_extract_nan_tagged_obj(str_val.into_int_value());
                            let res_str = self.builder.build_cast(InstructionOpcode::BitCast, res_val, self.cached_type(TYPE_STRING).ptr_type(AddressSpace::Generic), "").into_pointer_value();
                            let chars_ptr = self.builder.build_struct_gep(res_str, 2, "").unwrap();

                            self.builder.build_load(chars_ptr, "").into()
                        })
                        .collect_vec();
                    let fmt_str = format!("{}.{}({})", &enum_name, &variant_name, variant_field_names.iter().map(|name| format!("{}: %s", name)).join(", "));
                    let fmt_str_val = self.builder.build_global_string_ptr(&fmt_str, "").as_basic_value_enum();
                    let mut args = vec![self.str_type().const_zero().into(), self.context.i32_type().const_zero().into(), fmt_str_val.into()];
                    args.append(&mut field_strs.clone());
                    let len_val = self.builder.build_call(self.cached_fn(FN_SNPRINTF), args.as_slice(), "").try_as_basic_value().left().unwrap().into_int_value();
                    let len_val = self.builder.build_int_add(len_val, self.context.i64_type().const_int(1, false), "");
                    let str_mem = self.builder.build_call(self.cached_fn(FN_MALLOC), &[len_val.into()], "").try_as_basic_value().left().unwrap().into_pointer_value();
                    let len_val = self.builder.build_int_cast(len_val, self.context.i32_type(), "");
                    let mut args = vec![str_mem.into(), len_val.into(), fmt_str_val.into()];
                    args.append(&mut field_strs.clone());
                    self.builder.build_call(self.cached_fn(FN_SNPRINTF), args.as_slice(), "");

                    let len_val = self.builder.build_int_sub(len_val, self.context.i32_type().const_int(1, false), "");
                    self.alloc_string_obj(len_val, str_mem)
                };
                self.builder.build_return(Some(&str_repr.as_basic_value_enum()));

                self.builder.position_at_end(cont_bb);
            }

            self.builder.build_return(Some(&self.val_none().as_basic_value_enum()));

            self.cur_fn = old_fn;
            self.builder.position_at_end(self.cur_fn.get_last_basic_block().unwrap());

            let fn_ptr = tostring_fn.as_global_value().as_pointer_value();
            let fn_name_val = self.builder.build_global_string_ptr("toString", "").as_pointer_value().into();
            let fn_ptr_val = self.builder.build_cast(InstructionOpcode::PtrToInt, fn_ptr, self.value_t(), "").into();
            self.builder.build_call(self.cached_fn(FN_FUNCTION_ALLOC), &[fn_name_val, fn_ptr_val, self.value_t_ptr().const_zero().into()], "").try_as_basic_value().left().unwrap()
        };
        let slot = unsafe { self.builder.build_gep(vtable_entry, &[self.context.i32_type().const_int(0, false)], "") };
        self.builder.build_store(slot, self.builder.build_cast(InstructionOpcode::PtrToInt, tostring_fn_val, self.context.i64_type(), ""));

        // Generate eq method
        let eq_fn = {
            let old_fn = self.cur_fn;

            let eq_fn_type = self.gen_llvm_fn_type(true, 2);
            let eq_fn = self.module.add_function(&format!("{}__{}__eq", &self.module_name, &enum_name), eq_fn_type, None);
            self.cur_fn = eq_fn;
            let fn_bb = self.context.append_basic_block(self.cur_fn, "fn_body");
            self.builder.position_at_end(fn_bb);

            let self_arg = self.cur_fn.get_nth_param(2).unwrap();
            let self_arg = self.emit_extract_nan_tagged_obj(self_arg.into_int_value());
            let self_arg = self.builder.build_pointer_cast(self_arg, base_struct_type.ptr_type(AddressSpace::Generic), "");
            let self_variant_idx_ptr = self.builder.build_struct_gep(self_arg, 1, "").unwrap();
            let self_variant_idx = self.builder.build_load(self_variant_idx_ptr, "").into_int_value();

            let other_arg = self.cur_fn.get_nth_param(3).unwrap();
            let other_arg = self.emit_extract_nan_tagged_obj(other_arg.into_int_value());
            let other_arg = self.builder.build_pointer_cast(other_arg, base_struct_type.ptr_type(AddressSpace::Generic), "");
            let other_variant_idx_ptr = self.builder.build_struct_gep(other_arg, 1, "").unwrap();
            let other_variant_idx = self.builder.build_load(other_variant_idx_ptr, "").into_int_value();

            let cond = self.builder.build_int_compare(IntPredicate::NE, self_variant_idx, other_variant_idx, "");
            let then_bb = self.context.append_basic_block(self.cur_fn, "then");
            let cont_bb = self.context.append_basic_block(self.cur_fn, "cont");
            self.builder.build_conditional_branch(cond, then_bb, cont_bb);

            self.builder.position_at_end(then_bb);
            self.builder.build_return(Some(&self.context.i64_type().const_int(VAL_FALSE, false)));
            self.builder.position_at_end(cont_bb);

            let end_bb = self.context.append_basic_block(self.cur_fn, "end");
            for (idx, (_, variant_struct_type, variant_fields)) in variants.iter().enumerate() {
                if variant_fields.is_empty() { continue; }

                let idx_val = self.context.i8_type().const_int(idx as u64, false);
                let cond = self.builder.build_int_compare(IntPredicate::EQ, self_variant_idx, idx_val, "");
                let then_bb = self.context.append_basic_block(self.cur_fn, "then");
                let cont_bb = self.context.append_basic_block(self.cur_fn, "cont");
                self.builder.build_conditional_branch(cond, then_bb, cont_bb);

                self.builder.position_at_end(then_bb);
                let self_val = self.builder.build_pointer_cast(self_arg, variant_struct_type.ptr_type(AddressSpace::Generic), "");
                let other_val = self.builder.build_pointer_cast(other_arg, variant_struct_type.ptr_type(AddressSpace::Generic), "");

                for field_idx in 0..variant_fields.len() {
                    let self_field = self.builder.build_struct_gep(self_val, (field_idx + 2) as u32, "").unwrap();
                    let self_field = self.builder.build_load(self_field, "");
                    let other_field = self.builder.build_struct_gep(other_val, (field_idx + 2) as u32, "").unwrap();
                    let other_field = self.builder.build_load(other_field, "");

                    let fields_eq = self.builder.build_call(self.cached_fn(FN_VALUE_EQ), &[self_field.into(), other_field.into()], "")
                        .try_as_basic_value().left().unwrap().into_int_value();
                    let cond = self.builder.build_int_compare(IntPredicate::EQ, fields_eq, self.context.bool_type().const_zero(), "");

                    let then_bb = self.context.append_basic_block(self.cur_fn, "then");
                    let cont_bb = self.context.append_basic_block(self.cur_fn, "cont");
                    self.builder.build_conditional_branch(cond, then_bb, cont_bb);

                    self.builder.position_at_end(then_bb);
                    self.builder.build_return(Some(&self.context.i64_type().const_int(VAL_FALSE, false)));

                    self.builder.position_at_end(cont_bb);
                }
                self.builder.build_unconditional_branch(end_bb);

                self.builder.position_at_end(cont_bb);
            }
            self.builder.build_unconditional_branch(end_bb);

            self.builder.position_at_end(end_bb);
            self.builder.build_return(Some(&self.context.i64_type().const_int(VAL_TRUE, false)));

            self.cur_fn = old_fn;
            self.builder.position_at_end(self.cur_fn.get_last_basic_block().unwrap());

            eq_fn
        };
        let fn_ptr = eq_fn.as_global_value().as_pointer_value();
        let slot = unsafe { self.builder.build_gep(vtable_entry, &[self.context.i32_type().const_int(1, false)], "") };
        self.builder.build_store(slot, self.builder.build_cast(InstructionOpcode::PtrToInt, fn_ptr, self.context.i64_type(), ""));

        // Generate hash method
        let hash_fn = {
            let old_fn = self.cur_fn;

            let hash_fn_type = self.gen_llvm_fn_type(true, 1);
            let hash_fn = self.module.add_function(&format!("{}__{}__hash", &self.module_name, &enum_name), hash_fn_type, None);
            self.cur_fn = hash_fn;
            let fn_bb = self.context.append_basic_block(self.cur_fn, "fn_body");
            self.builder.position_at_end(fn_bb);

            let hash_val = self.builder.build_alloca(self.context.i32_type(), "hash");
            self.builder.build_store(hash_val, self.builder.build_load(type_id_global.as_pointer_value(), ""));

            let self_arg = self.cur_fn.get_nth_param(2).unwrap();
            let self_arg = self.emit_extract_nan_tagged_obj(self_arg.into_int_value());
            let self_arg = self.builder.build_pointer_cast(self_arg, base_struct_type.ptr_type(AddressSpace::Generic), "");
            let self_variant_idx_ptr = self.builder.build_struct_gep(self_arg, 1, "").unwrap();
            let self_variant_idx = self.builder.build_load(self_variant_idx_ptr, "").into_int_value();
            let self_variant_idx = self.builder.build_int_cast(self_variant_idx, self.context.i32_type(), "");

            let cur_hash_val = self.builder.build_load(hash_val, "").into_int_value();
            self.builder.build_store(
                hash_val,
                self.builder.build_int_add(cur_hash_val, self_variant_idx, "")
            );

            let end_bb = self.context.append_basic_block(self.cur_fn, "end");

            for (idx, (_, variant_struct_type, variant_fields)) in variants.iter().enumerate() {
                if variant_fields.is_empty() { continue; }

                let idx_val = self.context.i32_type().const_int(idx as u64, false);
                let cond = self.builder.build_int_compare(IntPredicate::EQ, self_variant_idx, idx_val, "");
                let then_bb = self.context.append_basic_block(self.cur_fn, "then");
                let cont_bb = self.context.append_basic_block(self.cur_fn, "cont");
                self.builder.build_conditional_branch(cond, then_bb, cont_bb);

                self.builder.position_at_end(then_bb);
                let self_val = self.builder.build_pointer_cast(self_arg, variant_struct_type.ptr_type(AddressSpace::Generic), "");

                let num_fields = variant_fields.len();
                for field_idx in 0..num_fields {
                    let field_ptr = self.builder.build_struct_gep(self_val, (field_idx + 2) as u32, "").unwrap();
                    let field_val = self.builder.build_load(field_ptr, "");

                    let field_hash = self.builder.build_call(self.cached_fn(FN_VALUE_HASH), &[field_val.into()], "")
                        .try_as_basic_value().left().unwrap().into_int_value();
                    let cur_hash_val = self.builder.build_load(hash_val, "").into_int_value();
                    self.builder.build_store(hash_val, self.builder.build_int_add(cur_hash_val, field_hash, ""));
                }
                self.builder.build_unconditional_branch(end_bb);

                self.builder.position_at_end(cont_bb);
            }
            self.builder.build_unconditional_branch(end_bb);

            self.builder.position_at_end(end_bb);
            let ret_hash = self.emit_nan_tagged_int(self.builder.build_load(hash_val, "").into_int_value());
            self.builder.build_return(Some(&ret_hash));

            self.cur_fn = old_fn;
            self.builder.position_at_end(self.cur_fn.get_last_basic_block().unwrap());

            hash_fn
        };
        let fn_ptr = hash_fn.as_global_value().as_pointer_value();
        let slot = unsafe { self.builder.build_gep(vtable_entry, &[self.context.i32_type().const_int(2, false)], "") };
        self.builder.build_store(slot, self.builder.build_cast(InstructionOpcode::PtrToInt, fn_ptr, self.context.i64_type(), ""));

        // Generate user-defined methods
        for (idx, (name, decl_node)) in method_defs.into_iter().enumerate() {
            let decl_node = if let TypedAstNode::FunctionDecl(_, decl_node) = decl_node { decl_node } else { unreachable!() };
            let fn_val = self.compile_function(name, Some(&enum_name), decl_node)?;
            let idx = idx + 3; // +3 to account for toString, eq, hash
            let slot = unsafe { self.builder.build_gep(vtable_entry, &[self.context.i32_type().const_int(idx as u64, false)], "") };
            self.builder.build_store(slot, fn_val);
        }

        Ok(self.val_none().as_basic_value_enum())
    }

    fn visit_identifier(&mut self, _token: Token, node: TypedIdentifierNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let res = if &node.name == "None" {
            self.val_none().as_basic_value_enum()
        } else {
            let ptr = self.resolve_ptr_to_variable(&node.name);
            self.builder.build_load(ptr, "")
        };

        Ok(res)
    }

    fn visit_assignment(&mut self, _token: Token, node: TypedAssignmentNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let expr = self.visit(*node.expr)?;

        match &node.kind {
            AssignmentTargetKind::Identifier => {
                if let TypedAstNode::Identifier(_, TypedIdentifierNode{ name, .. }) = *node.target {
                    let ptr = self.resolve_ptr_to_variable(&name);
                    self.builder.build_store(ptr, expr);
                } else { unreachable!() }
            }
            k @ AssignmentTargetKind::ArrayIndex | k @ AssignmentTargetKind::MapIndex => {
                let (target_type, target, idx) = if let TypedAstNode::Indexing(_, TypedIndexingNode{ target, index, .. }) = *node.target {
                    let target_type = target.get_type();
                    if let IndexingMode::Index(idx_expr) = index {
                        let target = self.visit(*target)?;
                        let idx = self.visit(*idx_expr)?;
                        (target_type, target, idx)
                    } else { unreachable!() }
                } else { unreachable!() };

                if k == &AssignmentTargetKind::ArrayIndex {
                    let idx = self.builder.build_int_cast(idx.into_int_value(), self.context.i32_type(), "");
                    if let Type::Tuple(_) = target_type {
                        self.builder.build_call(self.cached_fn(FN_TUPLE_SET), &[target.into(), idx.into(), expr.into()], "");
                    } else {
                        self.builder.build_call(self.cached_fn(FN_ARRAY_INSERT), &[target.into(), idx.into(), expr.into()], "");
                    }
                } else {
                    self.builder.build_call(self.cached_fn(FN_MAP_INSERT), &[target.into(), idx.into(), expr.into()], "");
                }
            }
            AssignmentTargetKind::Field => {
                let (target, field_idx) = if let TypedAstNode::Accessor(_, TypedAccessorNode { target, field_idx, .. }) = *node.target { (target, field_idx) } else { unreachable!() };
                let target_type = target.get_type();

                if Compiler::is_builtin_type(&target_type) {
                    unreachable!("Internal error: builtin fields are readonly and cannot be assigned to");
                }

                let target = self.visit(*target)?;

                let type_spec = self.scopes[0].types
                    .values()
                    .find(|spec| spec.typ == target_type)
                    .expect(&format!("Internal error: could not find type '{:?}' in scope", target_type));

                let target_ptr = self.builder.build_pointer_cast(
                    self.emit_extract_nan_tagged_obj(target.into_int_value()),
                    type_spec.struct_type.ptr_type(AddressSpace::Generic),
                    ""
                );
                let slot = self.builder.build_struct_gep(target_ptr, (field_idx + 1) as u32, "").unwrap();

                self.builder.build_store(slot, expr);
            }
        }

        Ok(expr)
    }

    fn visit_indexing(&mut self, _token: Token, node: TypedIndexingNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let target_type = node.target.get_type();
        let target = self.visit(*node.target)?;

        let res = match node.index {
            IndexingMode::Index(idx) => {
                let idx = self.visit(*idx)?;

                let func = match target_type {
                    Type::String => self.cached_fn(FN_STRING_GET),
                    Type::Array(_) => self.cached_fn(FN_ARRAY_GET),
                    Type::Tuple(_) => self.cached_fn(FN_TUPLE_GET),
                    Type::Map(_, _) => self.cached_fn(FN_MAP_GET),
                    _ => unreachable!("Internal error: attempting to index into non-indexable type")
                };

                if let Type::Map(_, _) = target_type {
                    self.builder.build_call(func, &[target.into(), idx.into()], "")
                } else {
                    let idx = self.emit_extract_nan_tagged_int(idx.into_int_value());
                    self.builder.build_call(func, &[target.into(), idx.into()], "")
                }
            }
            IndexingMode::Range(s, e) => {
                let start = s.map_or(Ok(self.val_none().into()), |v| self.visit(*v))?;
                let end = e.map_or(Ok(self.val_none().into()), |v| self.visit(*v))?;

                let func = match target_type {
                    Type::String => self.cached_fn(FN_STRING_RANGE),
                    Type::Array(_) => self.cached_fn(FN_ARRAY_RANGE),
                    _ => unreachable!("Internal error: attempting to range-index into non-indexable type")
                };

                self.builder.build_call(func, &[target.into(), start.into(), end.into()], "")
            }
        };

        Ok(res.try_as_basic_value().left().unwrap())
    }

    fn visit_if_statement(&mut self, is_stmt: bool, _token: Token, node: TypedIfNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let cond_val = self.visit(*node.condition)?;
        let cond = self.builder.build_and(
            self.builder.build_int_compare(IntPredicate::NE, cond_val.into_int_value(), self.context.i64_type().const_int(VAL_FALSE, false), ""),
            self.builder.build_int_compare(IntPredicate::NE, cond_val.into_int_value(), self.context.i64_type().const_int(VAL_NONE, false), ""),
            "cond"
        );
        let then_bb = self.context.append_basic_block(self.cur_fn, "then");
        let else_bb = self.context.append_basic_block(self.cur_fn, "else");
        let cont_bb = self.context.append_basic_block(self.cur_fn, "cont");
        self.builder.build_conditional_branch(cond, then_bb, else_bb);

        self.begin_new_scope("then-block".to_string());
        self.builder.position_at_end(then_bb);
        if let Some(cond_binding_pat) = node.condition_binding {
            self.visit_binding_pattern(cond_binding_pat, cond_val);
        }
        let mut last_value = self.val_none().as_basic_value_enum();
        let mut terminates_early = false;
        for node in node.if_block {
            let terminates = node.all_branches_terminate().is_some();
            last_value = self.visit(node)?;
            if terminates {
                last_value = self.val_none().as_basic_value_enum();
                terminates_early = true;
            }
        }
        let then_val = last_value;
        if !terminates_early {
            self.builder.build_unconditional_branch(cont_bb);
        }
        let then_bb = self.builder.get_insert_block().unwrap();
        self.end_scope();

        self.begin_new_scope("else-block".to_string());
        self.builder.position_at_end(else_bb);
        let mut last_value = self.val_none().as_basic_value_enum();
        let mut terminates_early = false;
        if let Some(nodes) = node.else_block {
            for node in nodes {
                let terminates = node.all_branches_terminate().is_some();
                last_value = self.visit(node)?;
                if terminates {
                    last_value = self.val_none().as_basic_value_enum();
                    terminates_early = true;
                }
            }
        }
        let else_val = last_value;
        if !terminates_early {
            self.builder.build_unconditional_branch(cont_bb);
        }
        let else_bb = self.builder.get_insert_block().unwrap();
        self.end_scope();

        self.builder.position_at_end(cont_bb);

        let res = if is_stmt {
            self.val_none().as_basic_value_enum()
        } else {
            let phi = self.builder.build_phi(self.value_t(), "");
            phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
            phi.as_basic_value()
        };

        Ok(res)
    }

    fn visit_if_expression(&mut self, token: Token, node: TypedIfNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        self.visit_if_statement(false, token, node)
    }

    fn visit_invocation(&mut self, token: Token, node: TypedInvocationNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let null_ptr = self.value_t().ptr_type(AddressSpace::Generic).const_zero();
        let num_passed_args = node.args.len();

        #[inline]
        fn passed_args<'a, 'ctx>(zelf: &mut Compiler<'a, 'ctx>, args: Vec<Option<TypedAstNode>>) -> Result<Vec<BasicMetadataValueEnum<'ctx>>, CompilerError> {
            args.into_iter()
                .map(|arg| if let Some(arg) = arg { zelf.visit(arg).map(|v| v.into()) } else { Ok(zelf.val_none().into()) })
                .collect::<Result<Vec<_>, _>>()
        }

        // If the invokee is an identifier and it represents a static function in scope, call that function.
        // If the function is a closure or a bound method, it will be handled below.
        if let TypedAstNode::Identifier(_, TypedIdentifierNode { name, .. }) = &*node.target {
            let found_function = self.scopes.iter().rev().find_map(|sc| sc.fns.get(name)).map(|f| CallableValue::from(f.clone()));
            if let Some(fn_value) = found_function {
                let mut args = vec![null_ptr.into(), self.context.i8_type().const_int(num_passed_args as u64, false).into()];
                args.append(&mut passed_args(self, node.args)?);

                let res = self.builder.build_call(fn_value, args.as_slice(), "");
                let res = if node.typ == Type::Unit { self.val_none().as_basic_value_enum() } else { res.try_as_basic_value().left().unwrap() };

                return Ok(res);
            }
        }

        let res = match *node.target {
            // If the node is an opt-safe accessor (`x?.y`), transform code to if-expr and compile
            TypedAstNode::Accessor(_, TypedAccessorNode { typ, target, field_ident, field_idx, is_opt_safe, is_method, is_readonly }) if is_method && is_opt_safe => {
                let target_type = target.get_type().get_opt_unwrapped();

                let transformed_node = TypedAstNode::IfExpression(
                    token.clone(),
                    TypedIfNode {
                        typ: node.typ.clone(),
                        condition: target,
                        condition_binding: Some(BindingPattern::Variable(Token::Ident(token.get_position(), "target".to_string()))),
                        if_block: vec![
                            TypedAstNode::Invocation(
                                token.clone(),
                                TypedInvocationNode {
                                    typ: node.typ,
                                    target: Box::new(
                                        TypedAstNode::Accessor(
                                            token.clone(),
                                            TypedAccessorNode {
                                                typ: typ.get_opt_unwrapped(),
                                                target: Box::new(TypedAstNode::Identifier(token, TypedIdentifierNode { typ: target_type, name: "target".to_string(), is_mutable: false, scope_depth: 0 /* doesn't matter */ })),
                                                field_ident,
                                                field_idx,
                                                is_opt_safe: false,
                                                is_method,
                                                is_readonly,
                                            }
                                        )
                                    ),
                                    args: node.args
                                }
                            )
                        ],
                        else_block: None,
                    },
                );

                return self.visit(transformed_node);
            }
            // If the node is an accessor (`x.y`), and `y` is a method of `x`, look up that type's function and call it, passing `x`
            // as its `self` arg, and any env in case the function is a closure.
            // If `y` is _not_ a method of `x`, we are just invoking a field of `x` which happens to be a function. This is handled in
            // the case below.
            TypedAstNode::Accessor(_, TypedAccessorNode { typ, target, field_idx, is_opt_safe, is_method, field_ident, .. }) if is_method && !is_opt_safe => {
                let target_type = target.get_type();
                let field_idx = if Compiler::is_builtin_type(&target_type) {
                    field_idx
                } else {
                    let field_name = Token::get_ident_name(&field_ident);
                    if field_name == "toString" { 0 } else { field_idx + 2 }
                };

                let (ret_type, arg_types) = if let Type::Fn(FnType { ret_type, arg_types, .. }) = typ { (ret_type, arg_types) } else { unreachable!() };
                let num_args = arg_types.len() + 1;

                let rcv = self.visit(*target)?;

                let idx = self.context.i32_type().const_int(field_idx as u64, false);
                let fn_val_as_value_t = self.builder.build_call(self.cached_fn(FN_VTABLE_LOOKUP), &[rcv.into(), idx.into()], "").try_as_basic_value().left().unwrap().into_int_value();

                let (fn_ptr_as_int, env_ptr) = if Compiler::is_builtin_type(&target_type) {
                    (fn_val_as_value_t, self.value_t_ptr().const_zero().into())
                } else {
                    let fn_val_ptr = self.builder.build_pointer_cast(
                        self.emit_extract_nan_tagged_obj(fn_val_as_value_t),
                        self.cached_type(TYPE_FUNCTION).ptr_type(AddressSpace::Generic),
                        ""
                    );
                    let fn_ptr_value_t = self.builder.build_struct_gep(fn_val_ptr, 2, "").unwrap();
                    let fn_ptr_as_int = self.builder.build_load(fn_ptr_value_t, "").into_int_value();

                    let env_ptr_ptr = self.builder.build_struct_gep(fn_val_ptr, 3, "").unwrap();
                    let env_ptr = self.builder.build_load(env_ptr_ptr, "");

                    (fn_ptr_as_int, env_ptr)
                };

                let mut args = vec![env_ptr.into(), self.context.i8_type().const_int((num_passed_args + 1) as u64, false).into(), rcv.into()];
                args.append(&mut passed_args(self, node.args)?);

                let fn_type = self.gen_llvm_fn_type(*ret_type != Type::Unit, num_args);
                let fn_ptr = self.builder.build_cast(InstructionOpcode::IntToPtr, fn_ptr_as_int, fn_type.ptr_type(AddressSpace::Generic), "").into_pointer_value();
                let callable = CallableValue::try_from(fn_ptr).unwrap();
                let res = self.builder.build_call(callable, args.as_slice(), "");
                let res = if node.typ == Type::Unit { self.val_none().as_basic_value_enum() } else { res.try_as_basic_value().left().unwrap() };

                res
            }
            // If the invokee is any other expression, there's not much we can know about it. We have no choice but to rely on the dynamic
            // nature of the `function_call` function from the runtime lib to handle invocation.
            expr => {
                let (fn_ret_type, num_args) = if let Type::Fn(FnType { ret_type, arg_types, .. }) = expr.get_type() { (ret_type, arg_types.len()) } else { unreachable!() };
                let typed_expr = self.visit(expr)?;
                let fn_val_as_value_t = typed_expr.into_int_value();

                let has_return = *fn_ret_type != Type::Unit;

                let mut args = Vec::new();
                args.push(fn_val_as_value_t.into());
                args.push(self.context.bool_type().const_int(if has_return { 1 } else { 0 }, false).into());
                args.push(self.context.i8_type().const_int(num_args as u64, false).into());
                args.push(self.context.i8_type().const_int(num_passed_args as u64, false).into());
                args.append(&mut passed_args(self, node.args)?);

                let res = self.builder.build_call(self.cached_fn(FN_FUNCTION_CALL), args.as_slice(), "").try_as_basic_value().left().unwrap();
                if has_return {
                    res
                } else {
                    self.val_none().into()
                }
            }
        };

        Ok(res)
    }

    fn visit_instantiation(&mut self, _token: Token, node: TypedInstantiationNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let type_name = if let TypedAstNode::Identifier(_, TypedIdentifierNode { name, .. }) = *node.target {
            name
        } else { unreachable!() };

        let type_spec = self.scopes[0].types
            .get(&type_name)
            .expect(&format!("Internal error: no type named '{}' exists", &type_name));
        let ctor_fn = type_spec.ctor_fn;

        let field_values = node.fields.into_iter()
            .map(|(_, node)| self.visit(node).map(|r| r.into()))
            .collect::<Result<Vec<_>, _>>();
        let field_values = field_values?;

        let res = self.builder.build_call(
            ctor_fn,
            field_values.as_slice(),
            ""
        ).try_as_basic_value().left().unwrap();

        Ok(res)
    }

    fn visit_accessor(&mut self, token: Token, node: TypedAccessorNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        // If the node is an opt-safe accessor (`x?.y`), transform code to if-expr and compile
        if node.is_opt_safe {
            let target_type = node.target.get_type().get_opt_unwrapped();

            let transformed_node = TypedAstNode::IfExpression(
                token.clone(),
                TypedIfNode {
                    typ: node.typ.clone(),
                    condition: node.target,
                    condition_binding: Some(BindingPattern::Variable(Token::Ident(token.get_position(), "target".to_string()))),
                    if_block: vec![
                        TypedAstNode::Accessor(
                            token.clone(),
                            TypedAccessorNode {
                                typ: node.typ,
                                target: Box::new(TypedAstNode::Identifier(token, TypedIdentifierNode { typ: target_type, name: "target".to_string(), is_mutable: false, scope_depth: 0 /* doesn't matter */ })),
                                field_ident: node.field_ident,
                                field_idx: node.field_idx,
                                is_opt_safe: false,
                                is_method: node.is_method,
                                is_readonly: node.is_readonly,
                            }
                        )
                    ],
                    else_block: None,
                },
            );
            return self.visit(transformed_node);
        }

        let target_type = node.target.get_type();
        match target_type {
            // enum
            Type::Type(_, typ, true) => {
                let enum_spec = self.scopes[0].enums
                    .values()
                    .find(|spec| spec.typ == *typ)
                    .expect(&format!("Internal error: could not find enum '{:?}' in scope", typ));
                let variant = match enum_spec.variants[node.field_idx].val {
                    Either::Left(const_global_ptr) => {
                        self.builder.build_load(const_global_ptr, "")
                    },
                    Either::Right(fn_val) => fn_val,
                };

                return Ok(variant);
            }
            // type
            Type::Type(_, _, false) => todo!("Static methods/fields on Types"),
            _ => {}
        }

        let target = self.visit(*node.target)?;

        let field_name = Token::get_ident_name(&node.field_ident);

        if node.is_method {
            let idx = if Compiler::is_builtin_type(&target_type) {
                node.field_idx
            } else if field_name == "toString" {
                0
            } else {
                node.field_idx + 2
            };
            let idx = self.context.i32_type().const_int(idx as u64, false);

            let fn_val_as_value_t = self.builder.build_call(self.cached_fn(FN_VTABLE_LOOKUP), &[target.into(), idx.into()], "").try_as_basic_value().left().unwrap();
            let fn_val_as_value_t = if Compiler::is_builtin_type(&target_type) {
                let fn_name_val = self.builder.build_global_string_ptr(&field_name, "").as_pointer_value().into();
                let fn_ptr_val = fn_val_as_value_t.into_int_value();
                let null_ptr = self.value_t_ptr().const_zero();
                self.builder.build_call(self.cached_fn(FN_FUNCTION_ALLOC), &[fn_name_val, fn_ptr_val.into(), null_ptr.into()], "").try_as_basic_value().left().unwrap()
            } else {
                fn_val_as_value_t
            };

            return Ok(self.builder.build_call(self.cached_fn(FN_FUNCTION_BIND), &[fn_val_as_value_t.into(), target.into()], "").try_as_basic_value().left().unwrap())
        }

        let val = match target_type {
            Type::Int => unreachable!("Internal error: type Int has no accessible fields"),
            Type::Float => unreachable!("Internal error: type Float has no accessible fields"),
            Type::Bool => unreachable!("Internal error: type Bool has no accessible fields"),
            Type::Tuple(_) => unreachable!("Internal error: type Tuple has no accessible fields"),
            Type::String => {
                let target = self.emit_extract_nan_tagged_obj(target.into_int_value());
                let target = self.builder.build_pointer_cast(target, self.cached_type(TYPE_STRING).ptr_type(AddressSpace::Generic), "");

                if field_name == "length" {
                    let length_ptr = self.builder.build_struct_gep(target, 1, "").unwrap();
                    let length_val = self.builder.build_load(length_ptr, "");
                    self.emit_nan_tagged_int(length_val.into_int_value()).into()
                } else {
                    unreachable!("Internal error: type String has no field '{}'", field_name);
                }
            }
            Type::Array(_) => {
                let target = self.emit_extract_nan_tagged_obj(target.into_int_value());
                let target = self.builder.build_pointer_cast(target, self.cached_type(TYPE_ARRAY).ptr_type(AddressSpace::Generic), "");

                if field_name == "length" {
                    let length_ptr = self.builder.build_struct_gep(target, 1, "").unwrap();
                    let length_val = self.builder.build_load(length_ptr, "");
                    self.emit_nan_tagged_int(length_val.into_int_value()).into()
                } else {
                    unreachable!("Internal error: type Array has no field '{}'", field_name);
                }
            }
            Type::Map(_, _) => {
                let target = self.emit_extract_nan_tagged_obj(target.into_int_value());
                let target = self.builder.build_pointer_cast(target, self.cached_type(TYPE_MAP).ptr_type(AddressSpace::Generic), "");

                if field_name == "size" {
                    let hashmap_ptr = self.builder.build_struct_gep(target, 1, "").unwrap();
                    let hashmap_size_ptr = self.builder.build_struct_gep(hashmap_ptr, 1, "").unwrap(); // Not sure why this is 1 and not 0, since `size` is the first field on hashmap_t...
                    let hashmap_size_val = self.builder.build_load(hashmap_size_ptr, "");

                    self.emit_nan_tagged_int(hashmap_size_val.into_int_value()).into()
                } else {
                    unreachable!("Internal error: type Map has no field '{}'", field_name);
                }
            }
            Type::Set(_) => {
                let target = self.emit_extract_nan_tagged_obj(target.into_int_value());
                let target = self.builder.build_pointer_cast(target, self.cached_type(TYPE_SET).ptr_type(AddressSpace::Generic), "");

                if field_name == "size" {
                    let hashmap_ptr = self.builder.build_struct_gep(target, 1, "").unwrap();
                    let hashmap_size_ptr = self.builder.build_struct_gep(hashmap_ptr, 1, "").unwrap(); // Not sure why this is 1 and not 0, since `size` is the first field on hashmap_t...
                    let hashmap_size_val = self.builder.build_load(hashmap_size_ptr, "");

                    self.emit_nan_tagged_int(hashmap_size_val.into_int_value()).into()
                } else {
                    unreachable!("Internal error: type Set has no field '{}'", field_name);
                }
            }
            _ => {
                let type_spec = self.scopes[0].types
                    .values()
                    .find(|spec| spec.typ == target_type)
                    .expect(&format!("Internal error: could not find type '{:?}' in scope", target_type));

                let target = self.emit_extract_nan_tagged_obj(target.into_int_value());
                let target = self.builder.build_pointer_cast(target, type_spec.struct_type.ptr_type(AddressSpace::Generic), "");

                let idx = (node.field_idx + 1) as u32; // +1 to account for obj_header
                let field_ptr = self.builder.build_struct_gep(target, idx, "").unwrap();
                let field_val = self.builder.build_load(field_ptr, "");

                field_val.into()
            }
        };

        Ok(val)
    }

    fn visit_for_loop(&mut self, _token: Token, node: TypedForLoopNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let TypedForLoopNode { binding, index_ident, iterator, body } = node;

        self.begin_new_scope("for-container".to_string());

        let iter = self.visit(*iterator)?;
        let enumerate_fn_idx = self.context.i32_type().const_int(2, false);
        let enumerate_fn_val = self.builder.build_call(self.cached_fn(FN_VTABLE_LOOKUP), &[iter.into(), enumerate_fn_idx.into()], "").try_as_basic_value().left().unwrap().into_int_value();
        let fn_type = self.gen_llvm_fn_type(true, 1);
        let fn_ptr = self.builder.build_cast(InstructionOpcode::IntToPtr, enumerate_fn_val, fn_type.ptr_type(AddressSpace::Generic), "").into_pointer_value();
        let callable = CallableValue::try_from(fn_ptr).unwrap();
        let res = self.builder.build_call(
            callable,
            &[self.value_t_ptr().const_zero().into(), self.context.i8_type().const_int(1, false).into(), iter.into()],
            ""
        ).try_as_basic_value().left().unwrap();
        let iter_local = self.builder.build_alloca(self.value_t(), "$iter");
        self.builder.build_store(iter_local, res);
        let iter_len_local = self.builder.build_alloca(self.context.i32_type(), "$iter_len");
        let iter_len_val = self.builder.build_load(
            self.builder.build_struct_gep(
                self.builder.build_pointer_cast(
                    self.emit_extract_nan_tagged_obj(res.into_int_value()),
                    self.cached_type(TYPE_ARRAY).ptr_type(AddressSpace::Generic),
                    ""
                ),
                1,
                ""
            ).unwrap(),
            ""
        );
        self.builder.build_store(iter_len_local, iter_len_val);

        let idx_local = self.builder.build_alloca(self.context.i32_type(), "$idx");
        self.builder.build_store(idx_local, self.context.i32_type().const_zero());

        let item_local = self.builder.build_alloca(self.value_t(), "$item");
        self.builder.build_store(item_local, self.val_none());

        let loop_start_bb = self.context.append_basic_block(self.cur_fn, "loop_cond");
        self.builder.build_unconditional_branch(loop_start_bb);
        self.builder.position_at_end(loop_start_bb);

        let loop_body_bb = self.context.append_basic_block(self.cur_fn, "loop_body");
        let loop_incr_bb = self.context.append_basic_block(self.cur_fn, "loop_incr");
        let loop_end_bb = self.context.append_basic_block(self.cur_fn, "loop_end");
        let cond = self.builder.build_int_compare(
            IntPredicate::ULT,
            self.builder.build_load(idx_local, "").into_int_value(),
            self.builder.build_load(iter_len_local, "").into_int_value(),
            ""
        );
        self.builder.build_conditional_branch(cond, loop_body_bb, loop_end_bb);

        let old_loop = self.cur_loop;
        self.cur_loop = Some((loop_incr_bb, loop_end_bb));

        self.builder.position_at_end(loop_body_bb);
        let item_pair_val = self.builder.build_call(self.cached_fn(FN_ARRAY_GET), &[self.builder.build_load(iter_local, "").into(), self.builder.build_load(idx_local, "").into()], "").try_as_basic_value().left().unwrap();
        let pair_l_val = self.builder.build_call(self.cached_fn(FN_TUPLE_GET), &[item_pair_val.into(), self.context.i32_type().const_zero().into()], "").try_as_basic_value().left().unwrap();
        self.visit_binding_pattern(binding, pair_l_val);
        if let Some(ident_tok) = index_ident {
            let pair_r_val = self.builder.build_call(self.cached_fn(FN_TUPLE_GET), &[item_pair_val.into(), self.context.i32_type().const_int(1, false).into()], "").try_as_basic_value().left().unwrap();
            self.visit_binding_pattern(BindingPattern::Variable(ident_tok), pair_r_val);
        }

        let mut terminates_early = false;
        for node in body {
            let terminates = node.all_branches_terminate().is_some();

            self.visit(node)?;

            if terminates {
                terminates_early = terminates;
                break;
            }
        }

        if !terminates_early {
            self.builder.build_unconditional_branch(loop_incr_bb);
        }

        self.builder.position_at_end(loop_incr_bb);
        self.builder.build_store(
            idx_local,
            self.builder.build_int_add(
                self.builder.build_load(idx_local, "").into_int_value(),
                self.context.i32_type().const_int(1, false),
                "",
            )
        );
        self.builder.build_unconditional_branch(loop_start_bb);

        self.builder.position_at_end(loop_end_bb);

        self.cur_loop = old_loop;
        self.end_scope();

        let res = self.val_none().as_basic_value_enum();
        Ok(res)
    }

    fn visit_while_loop(&mut self, _token: Token, _node: TypedWhileLoopNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_break(&mut self, _token: Token) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        if let Some((_loop_start_bb, loop_end_bb)) = self.cur_loop {
            self.builder.build_unconditional_branch(loop_end_bb);
        } else {
            unreachable!("Internal error: break found outside of a loop context");
        }

        let res = self.val_none().as_basic_value_enum();
        Ok(res)
    }

    fn visit_continue(&mut self, _token: Token) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        if let Some((loop_start_bb, _loop_end_bb)) = self.cur_loop {
            self.builder.build_unconditional_branch(loop_start_bb);
        } else {
            unreachable!("Internal error: continue found outside of a loop context");
        }

        let res = self.val_none().as_basic_value_enum();
        Ok(res)
    }

    fn visit_return(&mut self, _token: Token, _node: TypedReturnNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_match_statement(&mut self, _is_stmt: bool, _token: Token, _node: TypedMatchNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_match_expression(&mut self, _token: Token, _node: TypedMatchNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_import_statement(&mut self, _token: Token, _node: TypedImportNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_nil(&mut self, _token: Token) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        Ok(self.val_none().as_basic_value_enum())
    }
}

type CVFScope = (/* fn_id: */usize, /* vars: */HashSet<String>);
struct CapturedVariableFinder {
    scopes: Vec<CVFScope>,
    cur_fn_id: usize, // TODO: Do we need to track this?
    captured_variables: HashMap<String, usize>,
    context: HashSet<String>,
}

impl CapturedVariableFinder {
    fn find_captured_variables(
        context: HashSet<String>,
        args: &Vec<(/* name: */ String, /* default_value: */ Option<TypedAstNode>)>,
        body: &Vec<TypedAstNode>,
    ) -> HashMap<String, usize> {
        let mut finder = CapturedVariableFinder { scopes: vec![(0, HashSet::new())], cur_fn_id: 0, captured_variables: HashMap::new(), context };

        for (var_name, default_value) in args {
            if let Some(default_value) = default_value {
                finder.find_foreign_variables(default_value );
            }
            finder.add_variable_to_cur_scope(var_name.clone());
        }

        for node in body {
            finder.find_foreign_variables(node);
        }

        finder.captured_variables
    }

    fn is_builtin(&self, var_name: &String) -> bool {
        self.context.contains(var_name)
    }

    fn begin_new_fn_scope(&mut self) {
        self.cur_fn_id += 1;
        self.scopes.push((self.cur_fn_id, HashSet::new()));
    }

    fn end_fn_scope(&mut self) {
        self.cur_fn_id -= 1;
        self.scopes.pop();
    }

    fn begin_new_scope(&mut self) {
        self.scopes.push((self.cur_fn_id, HashSet::new()));
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn add_variable_to_cur_scope(&mut self, var_name: String) {
        self.scopes.last_mut().unwrap().1.insert(var_name);
    }

    fn visit_binding_pattern(&mut self, pat: &BindingPattern) {
        match pat {
            BindingPattern::Variable(tok) => {
                let var_name = Token::get_ident_name(tok);
                self.add_variable_to_cur_scope(var_name);
            }
            BindingPattern::Tuple(_, pats) => {
                for pat in pats {
                    self.visit_binding_pattern(pat);
                }
            }
            BindingPattern::Array(_, pats, _) => {
                for (pat, _) in pats {
                    self.visit_binding_pattern(pat);
                }
            }
        }
    }

    fn find_foreign_variables(&mut self, node: &TypedAstNode) {
        match node {
            TypedAstNode::Literal(_, _) => {}
            TypedAstNode::Unary(_, n) => self.find_foreign_variables(&*n.expr),
            TypedAstNode::Binary(_, n) => {
                self.find_foreign_variables(&*n.left);
                self.find_foreign_variables(&*n.right);
            }
            TypedAstNode::Grouped(_, n) => self.find_foreign_variables(&*n.expr),
            TypedAstNode::Array(_, n) => {
                for item in &n.items {
                    self.find_foreign_variables(item);
                }
            }
            TypedAstNode::Map(_, n) => {
                for (key, val) in &n.items {
                    self.find_foreign_variables(key);
                    self.find_foreign_variables(val);
                }
            }
            TypedAstNode::Set(_, n) => {
                for item in &n.items {
                    self.find_foreign_variables(item);
                }
            }
            TypedAstNode::Tuple(_, n) => {
                for item in &n.items {
                    self.find_foreign_variables(item);
                }
            }
            TypedAstNode::Lambda(_, n) => {
                self.begin_new_fn_scope();
                for (tok, _, default_value) in &n.args {
                    if let Some(default_value) = default_value { self.find_foreign_variables(default_value); }
                    let var_name = Token::get_ident_name(tok);
                    self.add_variable_to_cur_scope(var_name);
                }
                if let Some(typed_body) = &n.typed_body {
                    for node in typed_body {
                        self.find_foreign_variables(node);
                    }
                }
                self.end_fn_scope();
            }
            TypedAstNode::BindingDecl(_, n) => {
                self.visit_binding_pattern(&n.binding);
                if let Some(expr) = &n.expr {
                    self.find_foreign_variables(&*expr);
                }
            }
            TypedAstNode::FunctionDecl(_, n) => {
                self.add_variable_to_cur_scope(Token::get_ident_name(&n.name));
                self.begin_new_fn_scope();
                for (tok, _, _, default_value) in &n.args {
                    if let Some(default_value) = default_value { self.find_foreign_variables(default_value); }
                    let var_name = Token::get_ident_name(tok);
                    self.add_variable_to_cur_scope(var_name);
                }
                for node in &n.body {
                    self.find_foreign_variables(node);
                }
                self.end_fn_scope();
            }
            TypedAstNode::TypeDecl(_, n) => {
                for TypedTypeDeclField { default_value, .. } in &n.fields {
                    if let Some(default_value) = default_value { self.find_foreign_variables(default_value); }
                }
                for (_, func_decl_node) in &n.methods {
                    self.find_foreign_variables(func_decl_node);
                }
            }
            TypedAstNode::EnumDecl(_, n) => {
                for (_, _, default_value) in &n.static_fields {
                    if let Some(default_value) = default_value { self.find_foreign_variables(default_value); }
                }
                for (_, (_, fields)) in &n.variants {
                    if let Some(fields) = fields {
                        for field in fields {
                            if let Some(field) = field { self.find_foreign_variables(field); }
                        }
                    }
                }
                for (_, func_decl_node) in &n.methods {
                    self.find_foreign_variables(func_decl_node);
                }
            }
            TypedAstNode::Identifier(_, n) => {
                let var_name = &n.name;
                if self.is_builtin(var_name) { return; }

                let containing_scope = self.scopes.iter().rev().find(|(_, vars)| vars.contains(var_name));
                if containing_scope.is_none() {
                    if !self.captured_variables.contains_key(var_name) {
                        let var_num = self.captured_variables.len();
                        self.captured_variables.insert(var_name.clone(), var_num);
                    }
                }
            }
            TypedAstNode::Assignment(_, n) => {
                self.find_foreign_variables(&*n.target);
                self.find_foreign_variables(&*n.expr);
            }
            TypedAstNode::Indexing(_, n) => {
                self.find_foreign_variables(&*n.target);
                match &n.index {
                    IndexingMode::Index(idx) => self.find_foreign_variables(&*idx),
                    IndexingMode::Range(s, e) => {
                        if let Some(s) = s { self.find_foreign_variables(&*s); }
                        if let Some(e) = e { self.find_foreign_variables(&*e); }
                    }
                }
            }
            TypedAstNode::IfStatement(_, n) | TypedAstNode::IfExpression(_, n) => {
                self.find_foreign_variables(&*n.condition);
                self.begin_new_scope();
                if let Some(condition_binding_pat) = &n.condition_binding {
                    self.visit_binding_pattern(condition_binding_pat);
                }
                for node in &n.if_block {
                    self.find_foreign_variables(node);
                }
                self.end_scope();

                self.begin_new_scope();
                if let Some(else_block) = &n.else_block {
                    for node in else_block {
                        self.find_foreign_variables(node);
                    }
                }
                self.end_scope();
            }
            TypedAstNode::Invocation(_, n) => {
                self.find_foreign_variables(&*n.target);
                for arg in &n.args {
                    if let Some(arg) = arg { self.find_foreign_variables(arg); }
                }
            }
            TypedAstNode::Instantiation(_, n) => {
                self.find_foreign_variables(&*n.target);
                for (_, arg) in &n.fields {
                    self.find_foreign_variables(arg);
                }
            }
            TypedAstNode::ForLoop(_, n) => {
                self.begin_new_scope();
                self.find_foreign_variables(&*n.iterator);
                if let Some(index_ident) = &n.index_ident {
                    let var_name = Token::get_ident_name(index_ident);
                    self.add_variable_to_cur_scope(var_name);
                }
                for node in &n.body {
                    self.find_foreign_variables(node);
                }
                self.end_scope();
            }
            TypedAstNode::WhileLoop(_, n) => {
                self.begin_new_scope();
                self.find_foreign_variables(&*n.condition);
                if let Some(condition_binding) = &n.condition_binding {
                    let var_name = Token::get_ident_name(condition_binding);
                    self.add_variable_to_cur_scope(var_name);
                }
                for node in &n.body {
                    self.find_foreign_variables(node);
                }
                self.end_scope();
            }
            TypedAstNode::Break(_) => {}
            TypedAstNode::Continue(_) => {}
            TypedAstNode::ReturnStatement(_, n) => {
                if let Some(target) = &n.target {
                    self.find_foreign_variables(target);
                }
            }
            TypedAstNode::Accessor(_, n) => self.find_foreign_variables(&*n.target),
            TypedAstNode::MatchStatement(_, n) | TypedAstNode::MatchExpression(_, n) => {
                self.find_foreign_variables(&*n.target);
                for (_, binding, body) in &n.branches {
                    self.begin_new_scope();
                    if let Some(binding) = binding {
                        self.add_variable_to_cur_scope(binding.clone());
                    }
                    for node in body {
                        self.find_foreign_variables(node);
                    }
                    self.end_scope();
                }
            }
            TypedAstNode::ImportStatement(_, _) => {}
            TypedAstNode::_Nil(_) => {}
        }
    }
}
