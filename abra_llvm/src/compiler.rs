use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::iter::repeat;
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType, IntType, PointerType, StructType};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallableValue, FloatValue, FunctionValue, InstructionOpcode, IntValue, PointerValue};
use itertools::Itertools;
use abra_core::common::typed_ast_visitor::TypedAstVisitor;
use abra_core::lexer::tokens::Token;
use abra_core::parser::ast::{BinaryOp, BindingPattern, IndexingMode, UnaryOp};
use abra_core::typechecker::typechecker::TypedModule;
use abra_core::typechecker::typed_ast::{AssignmentTargetKind, TypedAccessorNode, TypedArrayNode, TypedAssignmentNode, TypedAstNode, TypedBinaryNode, TypedBindingDeclNode, TypedEnumDeclNode, TypedForLoopNode, TypedFunctionDeclNode, TypedGroupedNode, TypedIdentifierNode, TypedIfNode, TypedImportNode, TypedIndexingNode, TypedInstantiationNode, TypedInvocationNode, TypedLambdaNode, TypedLiteralNode, TypedMapNode, TypedMatchNode, TypedReturnNode, TypedSetNode, TypedTupleNode, TypedTypeDeclField, TypedTypeDeclNode, TypedUnaryNode, TypedWhileLoopNode};
use abra_core::typechecker::types::{FnType, Type};

const ENTRY_FN_NAME: &str = "__mod_entry";
const PLACEHOLDER_FN_NAME: &str = "__placeholder";
const PLACEHOLDER_TYPE_NAME: &str = "__placeholder";

#[derive(Debug)]
pub enum CompilerError {}

struct KnownFns<'ctx> {
    snprintf: FunctionValue<'ctx>,
    printf: FunctionValue<'ctx>,
    powf64: FunctionValue<'ctx>,
    malloc: FunctionValue<'ctx>,
    memcpy: FunctionValue<'ctx>,
    double_to_value_t: FunctionValue<'ctx>,
    value_t_to_double: FunctionValue<'ctx>,
    string_alloc: FunctionValue<'ctx>,
    string_concat: FunctionValue<'ctx>,
    string_get: FunctionValue<'ctx>,
    string_range: FunctionValue<'ctx>,
    string_split: FunctionValue<'ctx>,
    tuple_alloc: FunctionValue<'ctx>,
    array_alloc: FunctionValue<'ctx>,
    array_insert: FunctionValue<'ctx>,
    array_get: FunctionValue<'ctx>,
    array_range: FunctionValue<'ctx>,
    array_split: FunctionValue<'ctx>,
    tuple_get: FunctionValue<'ctx>,
    function_alloc: FunctionValue<'ctx>,
    closure_alloc: FunctionValue<'ctx>,
    vtable_alloc_entry: FunctionValue<'ctx>,
    vtable_lookup: FunctionValue<'ctx>,
    value_to_string: FunctionValue<'ctx>,
}

impl<'ctx> KnownFns<'ctx> {
    fn initial_value(placeholder: FunctionValue<'ctx>) -> Self {
        KnownFns {
            snprintf: placeholder,
            printf: placeholder,
            powf64: placeholder,
            malloc: placeholder,
            memcpy: placeholder,
            double_to_value_t: placeholder,
            value_t_to_double: placeholder,
            string_alloc: placeholder,
            string_concat: placeholder,
            string_get: placeholder,
            string_range: placeholder,
            string_split: placeholder,
            tuple_alloc: placeholder,
            array_alloc: placeholder,
            array_insert: placeholder,
            array_get: placeholder,
            array_range: placeholder,
            array_split: placeholder,
            tuple_get: placeholder,
            function_alloc: placeholder,
            closure_alloc: placeholder,
            vtable_alloc_entry: placeholder,
            vtable_lookup: placeholder,
            value_to_string: placeholder,
        }
    }

    fn is_initialized(&self) -> bool {
        let KnownFns { snprintf, printf, powf64, malloc, memcpy, double_to_value_t, value_t_to_double, string_alloc, string_concat, string_get, string_range, string_split, tuple_alloc, array_alloc, array_insert, array_get, array_range, array_split, tuple_get, function_alloc, closure_alloc, vtable_alloc_entry, vtable_lookup, value_to_string } = self;
        [snprintf, printf, powf64, malloc, memcpy, double_to_value_t, value_t_to_double, string_alloc, string_concat, string_get, string_range, string_split, tuple_alloc, array_alloc, array_insert, array_get, array_range, array_split, tuple_get, function_alloc, closure_alloc, vtable_alloc_entry, vtable_lookup, value_to_string].iter()
            .all(|f| f.get_name().to_str().unwrap().ne(PLACEHOLDER_FN_NAME))
    }
}

struct KnownTypes<'ctx> {
    string: StructType<'ctx>,
    array: StructType<'ctx>,
    obj_header_t: StructType<'ctx>,
    function: StructType<'ctx>,
}

impl<'ctx> KnownTypes<'ctx> {
    fn is_initialized(&self) -> bool {
        let KnownTypes { string, array, obj_header_t, function } = self;
        [string, array, obj_header_t, function].iter()
            .all(|t| t.get_name().unwrap().to_str().unwrap().ne(PLACEHOLDER_TYPE_NAME))
    }
}

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
struct Scope<'ctx> {
    name: String,
    fn_depth: usize,
    fns: HashMap<String, FunctionValue<'ctx>>,
    variables: HashMap<String, Variable<'ctx>>,
    closure_context: Option<ClosureContext<'ctx>>,
}

pub struct Compiler<'a, 'ctx> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    cur_fn: FunctionValue<'ctx>,
    known_fns: KnownFns<'ctx>,
    known_types: KnownTypes<'ctx>,
    scopes: Vec<Scope<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile_module(context: &'ctx Context, typed_module: TypedModule) -> Result<Module<'ctx>, CompilerError> {
        let builder = context.create_builder();
        let module = context.create_module("__main");

        let placeholder_fn = module.add_function(PLACEHOLDER_FN_NAME, context.void_type().fn_type(&[], false), None);
        let placeholder_type = context.opaque_struct_type(PLACEHOLDER_TYPE_NAME);

        let mut compiler = Compiler {
            context: &context,
            builder: &builder,
            module: &module,
            cur_fn: placeholder_fn,
            known_fns: KnownFns::initial_value(placeholder_fn),
            known_types: KnownTypes {
                string: placeholder_type,
                array: placeholder_type,
                obj_header_t: placeholder_type,
                function: placeholder_type,
            },
            scopes: vec![Scope { name: "$root".to_string(), fn_depth: 0, fns: HashMap::new(), variables: HashMap::new(), closure_context: None }],
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

    fn add_type_id_and_vtable<S: AsRef<str>>(&self, type_id_name: S, vtable_size: usize) -> PointerValue<'ctx> {
        let next_type_id = self.module.get_global("next_type_id").unwrap();

        let new_type_id = self.module.add_global(self.context.i32_type(), None, type_id_name.as_ref());
        let type_id = self.builder.build_load(next_type_id.as_pointer_value(), "");
        self.builder.build_store(new_type_id.as_pointer_value(), type_id);
        self.builder.build_store(next_type_id.as_pointer_value(), self.builder.build_int_add(type_id.into_int_value(), self.context.i32_type().const_int(1, false), ""));

        self.builder.build_call(
            self.known_fns.vtable_alloc_entry,
            &[
                self.builder.build_load(new_type_id.as_pointer_value(), "").into_int_value().into(),
                self.context.i32_type().const_int(vtable_size as u64, false).into()
            ],
            "",
        ).try_as_basic_value().left().unwrap().into_pointer_value()
    }

    fn add_vtable_fn<S: AsRef<str>>(&self, vtable_ptr: PointerValue<'ctx>, method_idx: usize, method_name: S, method_type: FunctionType<'ctx>) {
        let slot = unsafe { self.builder.build_gep(vtable_ptr, &[self.context.i32_type().const_int(method_idx as u64, false)], "") };
        let fn_ptr = self.module.add_function(method_name.as_ref(), method_type, None).as_global_value().as_pointer_value();
        self.builder.build_store(slot, self.builder.build_cast(InstructionOpcode::PtrToInt, fn_ptr, self.context.i64_type(), ""));
    }

    fn init_prelude(&mut self) {
        let println = self.module.add_function("prelude__println", self.context.void_type().fn_type(&[self.value_t_ptr().into(), self.value_t().into()], false), None);
        self.current_scope_mut().fns.insert("println".to_string(), println);
        let print = self.module.add_function("prelude__print", self.context.void_type().fn_type(&[self.value_t_ptr().into(), self.value_t().into()], false), None);
        self.current_scope_mut().fns.insert("print".to_string(), print);

        self.init_int_type();
        self.init_float_type();
        self.init_bool_type();
        self.init_string_type();
        self.init_array_type();
        self.init_tuple_type();
        self.init_function_type();
    }

    fn init_int_type(&self)  {
        let vtable_entry = self.add_type_id_and_vtable("type_id_Int", 1);
        self.add_vtable_fn(vtable_entry, 0, "prelude__Int__toString", self.value_t().fn_type(&[self.value_t_ptr().into(), self.value_t().into()], false));
    }

    fn init_float_type(&self) {
        let vtable_entry = self.add_type_id_and_vtable("type_id_Float", 1);
        self.add_vtable_fn(vtable_entry, 0, "prelude__Float__toString", self.value_t().fn_type(&[self.value_t_ptr().into(), self.value_t().into()], false));
    }

    fn init_bool_type(&self)  {
        let vtable_entry = self.add_type_id_and_vtable("type_id_Bool", 1);
        self.add_vtable_fn(vtable_entry, 0, "prelude__Bool__toString", self.value_t().fn_type(&[self.value_t_ptr().into(), self.value_t().into()], false));
    }

    fn init_string_type(&self)  {
        let vtable_entry = self.add_type_id_and_vtable("type_id_String", 3);
        self.add_vtable_fn(vtable_entry, 0, "prelude__String__toString", self.value_t().fn_type(&[self.value_t_ptr().into(), self.value_t().into()], false));
        self.add_vtable_fn(vtable_entry, 1, "prelude__String__toLower", self.value_t().fn_type(&[self.value_t_ptr().into(), self.value_t().into()], false));
        self.add_vtable_fn(vtable_entry, 2, "prelude__String__toUpper", self.value_t().fn_type(&[self.value_t_ptr().into(), self.value_t().into()], false));
    }

    fn init_array_type(&self) {
        let vtable_entry = self.add_type_id_and_vtable("type_id_Array", 0);
        self.add_vtable_fn(vtable_entry, 0, "prelude__Array__toString", self.value_t().fn_type(&[self.value_t_ptr().into(), self.value_t().into()], false));
    }

    fn init_tuple_type(&self) {
        let vtable_entry = self.add_type_id_and_vtable("type_id_Tuple", 0);
        self.add_vtable_fn(vtable_entry, 0, "prelude__Tuple__toString", self.value_t().fn_type(&[self.value_t_ptr().into(), self.value_t().into()], false));
    }

    fn init_function_type(&self) {
        let vtable_entry = self.add_type_id_and_vtable("type_id_Function", 0);
        self.add_vtable_fn(vtable_entry, 0, "prelude__Function__toString", self.value_t().fn_type(&[self.value_t_ptr().into(), self.value_t().into()], false));
    }

    fn init(&mut self) {
        self.module.add_global(self.context.i32_type(), None, "next_type_id");

        let malloc_type = self.str_type().fn_type(&[self.context.i64_type().into()], false);
        self.known_fns.malloc = self.module.add_function("GC_malloc", malloc_type, None);
        let snprintf_type = self.context.i64_type().fn_type(&[self.str_type().into(), self.context.i64_type().into(), self.str_type().into()], true);
        self.known_fns.snprintf = self.module.add_function("snprintf", snprintf_type, None);
        let printf_type = self.context.i64_type().fn_type(&[self.str_type().into()], true);
        self.known_fns.printf = self.module.add_function("printf", printf_type, None);
        let powf64_type = self.context.f64_type().fn_type(&[self.context.f64_type().into(), self.context.f64_type().into()], false);
        self.known_fns.powf64 = self.module.add_function("llvm.pow.f64", powf64_type, None);
        let memcpy_type = self.str_type().fn_type(&[self.str_type().into(), self.str_type().into(), self.context.i32_type().into()], false);
        self.known_fns.memcpy = self.module.add_function("memcpy", memcpy_type, None);
        let double_to_value_t_type = self.value_t().fn_type(&[self.context.f64_type().into()], false);
        self.known_fns.double_to_value_t = self.module.add_function("double_to_value_t", double_to_value_t_type, None);
        let value_t_to_double_type = self.context.f64_type().fn_type(&[self.value_t().into()], false);
        self.known_fns.value_t_to_double = self.module.add_function("value_t_to_double", value_t_to_double_type, None);
        let string_alloc_type = self.value_t().fn_type(&[self.context.i32_type().into(), self.str_type().into()], false);
        self.known_fns.string_alloc = self.module.add_function("string_alloc", string_alloc_type, None);
        let string_concat_type = self.value_t().fn_type(&[self.value_t().into(), self.value_t().into()], false);
        self.known_fns.string_concat = self.module.add_function("string_concat", string_concat_type, None);
        let string_get_type = self.value_t().fn_type(&[self.value_t().into(), self.context.i32_type().into()], false);
        self.known_fns.string_get = self.module.add_function("string_get", string_get_type, None);
        let string_range_type = self.value_t().fn_type(&[self.value_t().into(), self.value_t().into(), self.value_t().into()], false);
        self.known_fns.string_range = self.module.add_function("string_range", string_range_type, None);
        let string_split_type = self.value_t().fn_type(&[self.value_t().into(), self.context.i32_type().into()], false);
        self.known_fns.string_split = self.module.add_function("string_split", string_split_type, None);
        let tuple_alloc_type = self.value_t().fn_type(&[self.context.i32_type().into()], true);
        self.known_fns.tuple_alloc = self.module.add_function("tuple_alloc", tuple_alloc_type, None);
        let array_alloc_type = self.value_t().fn_type(&[self.context.i32_type().into()], false);
        self.known_fns.array_alloc = self.module.add_function("array_alloc", array_alloc_type, None);
        let array_insert_type = self.context.void_type().fn_type(&[self.value_t().into(), self.context.i32_type().into(), self.value_t().into()], false);
        self.known_fns.array_insert = self.module.add_function("array_insert", array_insert_type, None);
        let array_get_type = self.value_t().fn_type(&[self.value_t().into(), self.context.i32_type().into()], false);
        self.known_fns.array_get = self.module.add_function("array_get", array_get_type, None);
        let array_range_type = self.value_t().fn_type(&[self.value_t().into(), self.value_t().into(), self.value_t().into()], false);
        self.known_fns.array_range = self.module.add_function("array_range", array_range_type, None);
        let array_split_type = self.value_t().fn_type(&[self.value_t().into(), self.context.i32_type().into()], false);
        self.known_fns.array_split = self.module.add_function("array_split", array_split_type, None);
        let tuple_get_type = self.value_t().fn_type(&[self.value_t().into(), self.context.i32_type().into()], false);
        self.known_fns.tuple_get = self.module.add_function("tuple_get", tuple_get_type, None);
        let function_alloc_type = self.value_t().fn_type(&[self.str_type().into(), self.value_t().into()], false);
        self.known_fns.function_alloc = self.module.add_function("function_alloc", function_alloc_type, None);
        let closure_alloc_type = self.value_t().fn_type(&[self.str_type().into(), self.value_t().into(), self.value_t().ptr_type(AddressSpace::Generic).into()], false);
        self.known_fns.closure_alloc = self.module.add_function("closure_alloc", closure_alloc_type, None);
        self.known_fns.vtable_alloc_entry = self.module.add_function(
            "vtable_alloc_entry",
            self.context.i64_type().ptr_type(AddressSpace::Generic).fn_type(&[self.context.i32_type().into(), self.context.i32_type().into()], false),
            None
        );
        self.known_fns.vtable_lookup = self.module.add_function(
            "vtable_lookup",
            self.value_t().fn_type(&[self.value_t().into(), self.context.i32_type().into()], false),
            None
        );
        self.known_fns.value_to_string = self.module.add_function(
            "value_to_string",
            self.value_t().fn_type(&[self.value_t().into()], false),
            None
        );

        self.known_types.obj_header_t = {
            let t = self.context.opaque_struct_type("obj_header_t");
            t.set_body(&[
                self.context.i32_type().into() // type_id
            ], false);
            t
        };
        self.known_types.string = {
            let string_type = self.context.opaque_struct_type("String");
            string_type.set_body(&[
                self.known_types.obj_header_t.into(), // obj_header_t h
                self.context.i32_type().into(), // int32_t length
                self.str_type(), // char* chars
            ], false);
            string_type
        };
        self.known_types.array = { // TODO: Clean up unused `known_types`
            let array_type = self.context.opaque_struct_type("Array");
            array_type.set_body(&[
                self.context.i32_type().into(), // int64_t length
                self.context.i32_type().into(), // int64_t _capacity
                self.value_t().ptr_type(AddressSpace::Generic).into(), // void** items
            ], false);
            array_type
        };
        self.known_types.function = {
            let fn_type = self.context.opaque_struct_type("Function");
            fn_type.set_body(&[
                self.known_types.obj_header_t.into(), // obj_header_t h
                self.str_type().into(), // char* name
                self.value_t().into(), // value_t fn_ptr
                self.value_t().ptr_type(AddressSpace::Generic).into(), // value_t* env
            ], false);
            fn_type
        };

        // Prelude initialization function
        let init_fn_type = self.context.void_type().fn_type(&[], false);
        let init_fn = self.module.add_function("$init", init_fn_type, None);
        let init_fn_bb = self.context.append_basic_block(init_fn, "init_fn_bb");
        self.builder.position_at_end(init_fn_bb);
        self.init_prelude();
        self.builder.build_return(None);

        let entry_fn_type = self.context.void_type().fn_type(&[], false);
        let entry_fn = self.module.add_function(ENTRY_FN_NAME, entry_fn_type, None);
        let entry_fn_bb = self.context.append_basic_block(entry_fn, "entry_fn_bb");
        self.builder.position_at_end(entry_fn_bb);
        self.cur_fn = entry_fn;

        self.builder.build_call(init_fn, &[], "");

        // The placeholder values were only used during initialization for convenience. They can
        // be disposed of now that all known_fns, known_types, and self.cur_fn are initialized.
        debug_assert!(self.known_fns.is_initialized());
        debug_assert!(self.known_types.is_initialized());
        unsafe {
            self.module.get_function(PLACEHOLDER_FN_NAME).unwrap().delete();
        }
    }

    fn finalize(&self,  last_item_type: &Type, last_item: BasicValueEnum<'ctx>) {
        if *last_item_type != Type::Unit {
            let result_value = self.builder.build_call(
                self.known_fns.value_to_string,
                &[last_item.into()],
                ""
            ).try_as_basic_value().left().unwrap().into_int_value();
            let result_value = self.emit_extract_nan_tagged_obj(result_value);
            let result_string = self.builder.build_cast(InstructionOpcode::BitCast, result_value, self.known_types.string.ptr_type(AddressSpace::Generic), "").into_pointer_value();
            let result_string_chars = self.builder.build_struct_gep(result_string, 2, "").unwrap();
            let result = self.builder.build_load(result_string_chars, "res");

            self.builder.build_call(
                self.known_fns.printf,
                &[self.builder.build_global_string_ptr("%s\n", "fmt").as_basic_value_enum().into(), result.into()],
                "",
            );
        }

        self.builder.build_return(None);
    }

    #[allow(dead_code)]
    fn println_debug(&self, tag: &str, value: BasicValueEnum<'ctx>) {
        let result_value = self.builder.build_call(
            self.known_fns.value_to_string,
            &[value.into()],
            ""
        ).try_as_basic_value().left().unwrap().into_int_value();
        let result_value = self.emit_extract_nan_tagged_obj(result_value);
        let result_string = self.builder.build_cast(InstructionOpcode::BitCast, result_value, self.known_types.string.ptr_type(AddressSpace::Generic), "").into_pointer_value();
        let result_string_chars = self.builder.build_struct_gep(result_string, 2, "").unwrap();
        let result = self.builder.build_load(result_string_chars, "");

        self.builder.build_call(
            self.known_fns.printf,
            &[self.builder.build_global_string_ptr(&format!("{}: %s\n", tag), "fmt").as_basic_value_enum().into(), result.into()],
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
        self.scopes.push(Scope { name, fn_depth, fns: HashMap::new(), variables: HashMap::new(), closure_context })
    }

    fn begin_new_scope(&mut self, name: String) {
        let fn_depth = self.current_scope().fn_depth;
        self.scopes.push(Scope { name, fn_depth, fns: HashMap::new(), variables: HashMap::new(), closure_context: None })
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn str_type(&self) -> BasicTypeEnum<'ctx> {
        self.context.i8_type().ptr_type(AddressSpace::Generic).as_basic_type_enum()
    }

    fn alloc_string_obj(&self, length_val: IntValue<'ctx>, chars_val: PointerValue<'ctx>) -> IntValue<'ctx> {
        self.builder.build_call(self.known_fns.string_alloc, &[length_val.into(), chars_val.into()], "").try_as_basic_value().left().unwrap().into_int_value()
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

        self.builder.build_call(self.known_fns.tuple_alloc, args.as_slice(), "").try_as_basic_value().left().unwrap().into_int_value()
    }

    fn alloc_array_obj_with_length(&self, length_val: IntValue<'ctx>) -> IntValue<'ctx> {
        self.builder.build_call(self.known_fns.array_alloc, &[length_val.into()], "").try_as_basic_value().left().unwrap().into_int_value()
    }

    fn array_obj_insert(&self, array_val: IntValue<'ctx>, index_val: IntValue<'ctx>, item_val: IntValue<'ctx>) {
        self.builder.build_call(self.known_fns.array_insert, &[array_val.into(), index_val.into(), item_val.into()], "");
    }

    fn value_t(&self) -> IntType<'ctx> {
        self.context.i64_type()
    }

    fn value_t_ptr(&self) -> PointerType<'ctx> {
        self.context.i64_type().ptr_type(AddressSpace::Generic)
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
            self.known_fns.double_to_value_t,
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
            self.known_fns.value_t_to_double,
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
                        self.known_fns.tuple_get,
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
                            (self.known_fns.string_range, self.known_fns.string_split)
                        } else {
                            (self.known_fns.array_range, self.known_fns.array_split)
                        };

                        let idx_val = self.emit_nan_tagged_int_const(idx as i32);
                        let tail = self.builder.build_call(range_fn, &[cur_val.into(), idx_val.into(), self.val_none().into()], "").try_as_basic_value().left().unwrap();

                        let split_idx = (num_pats - idx - 1) as i64;
                        let split_idx = self.context.i32_type().const_int((-split_idx) as u64, true);
                        let parts = self.builder.build_call(split_fn, &[tail.into(), split_idx.into()], "").try_as_basic_value().left().unwrap();
                        let l_part = self.builder.build_call(self.known_fns.tuple_get, &[parts.into(), self.context.i32_type().const_int(0, false).into()], "").try_as_basic_value().left().unwrap();
                        let r_part = self.builder.build_call(self.known_fns.tuple_get, &[parts.into(), self.context.i32_type().const_int(1, false).into()], "").try_as_basic_value().left().unwrap();

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
                    let func = if is_string { self.known_fns.string_get } else { self.known_fns.array_get };
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
                let value = self.emit_extract_nan_tagged_bool(value.into_int_value());
                let res = self.builder.build_int_unsigned_rem(
                    self.builder.build_int_add(value, self.context.i64_type().const_int(1, false), ""),
                    self.context.i64_type().const_int(2, false),
                    ""
                );
                self.emit_nan_tagged_bool(res).as_basic_value_enum()
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
                            self.known_fns.powf64,
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
                                self.known_fns.powf64,
                                &[left.into(), right.into()],
                                "",
                            ).try_as_basic_value().left().unwrap().into_float_value();
                            self.builder.build_float_mul(left, self.context.f64_type().const_float(-1.0), "")
                        };
                        self.builder.build_unconditional_branch(cont_bb);
                        let then_bb = self.builder.get_insert_block().unwrap();

                        self.builder.position_at_end(else_bb);
                        let else_val = self.builder.build_call(
                            self.known_fns.powf64,
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

                self.builder.build_call(self.known_fns.string_concat, &[left.into(), right.into()], "").try_as_basic_value().left().unwrap()
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

    fn visit_map(&mut self, _token: Token, _node: TypedMapNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_set(&mut self, _token: Token, _node: TypedSetNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_lambda(&mut self, _token: Token, _node: TypedLambdaNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
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
        let old_fn = self.cur_fn;

        let namespace = self.scopes.iter().map(|s| &s.name).join("::");
        let fn_name = Token::get_ident_name(&node.name);
        let fully_qualified_fn_name = format!("{}::{}", namespace, fn_name);

        let captured_variables = CapturedVariableFinder::find_captured_variables(&node);
        let num_captured_variables = captured_variables.len();
        let is_closure = !captured_variables.is_empty();

        let mut fn_arg_types = Vec::new();
        fn_arg_types.push(self.value_t().ptr_type(AddressSpace::Generic).into());
        fn_arg_types.append(&mut repeat(self.value_t().into()).take(node.args.len()).collect_vec());
        let fn_type = self.value_t().fn_type(fn_arg_types.as_slice(), false);

        let func = self.module.add_function(&fully_qualified_fn_name, fn_type, Some(Linkage::Private));
        let fn_local = self.builder.build_alloca(self.value_t(), &fn_name);
        self.current_scope_mut().variables.insert(fn_name.clone(), Variable { local_ptr: fn_local, is_captured: false });

        let env_mem= if !is_closure {
            self.current_scope_mut().fns.insert(fn_name.clone(), func);

            None
        } else {
            // Allocate space for closure's `env`, lifting variables from the current scope if necessary
            let env_mem = self.builder.build_call(
                self.known_fns.malloc,
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
                    let lifted_val_mem = self.builder.build_call(self.known_fns.malloc, &[self.value_t().size_of().into()], "").try_as_basic_value().left().unwrap().into_pointer_value();
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

            Some(env_mem)
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
        self.begin_new_fn_scope(fn_name.clone(), closure_context);
        for (mut idx, (tok, _, _, default_value)) in node.args.into_iter().enumerate() {
            idx += 1; // Skip over _env param

            let name = Token::get_ident_name(&tok);
            let param_ptr = self.builder.build_alloca(self.value_t(), &name);
            self.current_scope_mut().variables.insert(name, Variable { local_ptr: param_ptr, is_captured: false });

            // If there's a default value for the parameter assign it, making sure to only evaluate the default expression if necessary
            let param_val = func.get_nth_param(idx as u32)
                .expect(&format!("Internal error: expected parameter idx {} to exist for function {}", idx, &fn_name));
            let param_val = if let Some(default_value) = default_value {
                // $param = if $param == None { $default } else { $param }
                let cond = self.builder.build_int_compare(IntPredicate::EQ, param_val.into_int_value(), self.val_none(), "cond");
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

        let body_len = node.body.len();
        for (idx, node) in node.body.into_iter().enumerate() {
            let value = self.visit(node)?;

            if idx == body_len - 1 {
                self.builder.build_return(Some(&value.as_basic_value_enum()));
            }
        }

        self.cur_fn = old_fn;
        self.builder.position_at_end(self.cur_fn.get_last_basic_block().unwrap());
        self.end_scope();

        let fn_name_val = self.builder.build_global_string_ptr(&fn_name, "").as_pointer_value().into();
        let fn_ptr_val = self.builder.build_cast(InstructionOpcode::PtrToInt, func.as_global_value().as_pointer_value(), self.value_t(), "").into();
        let fn_val = if let Some(env_mem) = env_mem {
            self.builder.build_call(self.known_fns.closure_alloc, &[fn_name_val, fn_ptr_val, env_mem.into()], "").try_as_basic_value().left().unwrap()
        } else {
            self.builder.build_call(self.known_fns.function_alloc, &[fn_name_val, fn_ptr_val], "").try_as_basic_value().left().unwrap()
        };
        if node.is_recursive {
            let val = self.builder.build_load(fn_local, "");
            let ptr = self.emit_extract_nan_tagged_obj(val.into_int_value());
            let ptr = self.builder.build_pointer_cast(ptr, self.value_t().ptr_type(AddressSpace::Generic), "");
            self.builder.build_store(ptr, fn_val);
        } else {
            self.builder.build_store(fn_local, fn_val);
        }

        let res = self.emit_nan_tagged_int_const(0);
        Ok(res.into())
    }

    fn visit_type_decl(&mut self, _token: Token, _node: TypedTypeDeclNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_enum_decl(&mut self, _token: Token, _node: TypedEnumDeclNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
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
        let res = match &node.kind {
            AssignmentTargetKind::Identifier => {
                if let TypedAstNode::Identifier(_, TypedIdentifierNode{ name, .. }) = *node.target {
                    let expr = self.visit(*node.expr)?;

                    let ptr = self.resolve_ptr_to_variable(&name);
                    self.builder.build_store(ptr, expr);

                    expr
                } else { unreachable!() }
            }
            AssignmentTargetKind::ArrayIndex |
            AssignmentTargetKind::MapIndex |
            AssignmentTargetKind::Field => todo!()
        };

        Ok(res)
    }

    fn visit_indexing(&mut self, _token: Token, node: TypedIndexingNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let target_type = node.target.get_type();
        let target = self.visit(*node.target)?;

        let res = match node.index {
            IndexingMode::Index(idx) => {
                let idx = self.visit(*idx)?;
                let idx = self.emit_extract_nan_tagged_int(idx.into_int_value());

                let func = match target_type {
                    Type::String => self.known_fns.string_get,
                    Type::Array(_) => self.known_fns.array_get,
                    Type::Tuple(_) => self.known_fns.tuple_get,
                    Type::Map(_, _) => todo!(),
                    _ => unreachable!("Internal error: attempting to index into non-indexable type")
                };
                self.builder.build_call(func, &[target.into(), idx.into()], "")
            }
            IndexingMode::Range(s, e) => {
                let start = s.map_or(Ok(self.val_none().into()), |v| self.visit(*v))?;
                let end = e.map_or(Ok(self.val_none().into()), |v| self.visit(*v))?;

                let func = match target_type {
                    Type::String => self.known_fns.string_range,
                    Type::Array(_) => self.known_fns.array_range,
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
        for node in node.if_block {
            last_value = self.visit(node)?;
        }
        let then_val = last_value;
        self.builder.build_unconditional_branch(cont_bb);
        self.end_scope();

        self.begin_new_scope("else-block".to_string());
        self.builder.position_at_end(else_bb);
        last_value = self.val_none().as_basic_value_enum();
        if let Some(nodes) = node.else_block {
            for node in nodes {
                last_value = self.visit(node)?;
            }
        }
        let else_val = last_value;
        self.builder.build_unconditional_branch(cont_bb);
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

    fn visit_invocation(&mut self, _token: Token, node: TypedInvocationNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let mut args: Vec<BasicMetadataValueEnum<'ctx>> = vec![
          self.value_t().ptr_type(AddressSpace::Generic).const_zero().into()
        ];

        #[inline]
        fn convert_serialized_fn_ptr_to_callable<'a, 'ctx>(zelf: &Compiler<'a, 'ctx>, fn_ptr_value_t: IntValue<'ctx>, num_args: usize, ret_type: &Type) -> Option<CallableValue<'ctx>> {
            let mut args = Vec::new();
            args.push(zelf.value_t().ptr_type(AddressSpace::Generic).into());
            args.append(&mut repeat(zelf.value_t().into()).take(num_args).collect_vec());

            let fn_type = if ret_type == &Type::Unit {
                zelf.context.void_type().fn_type(args.as_slice(), false)
            } else {
                zelf.value_t().fn_type(args.as_slice(), false)
            };
            let fn_ptr = zelf.builder.build_cast(InstructionOpcode::IntToPtr, fn_ptr_value_t, fn_type.ptr_type(AddressSpace::Generic), "").into_pointer_value();
            CallableValue::try_from(fn_ptr).ok()
        }

        let fn_value = match *node.target {
            TypedAstNode::Identifier(_, TypedIdentifierNode { name, .. }) => {
                let num_args = node.args.len();
                let fn_ret_type = &node.typ;

                // If there's a known function in the scopes by the given name, use that FunctionValue.
                // Otherwise, if there's a variable in the scopes by the given name, then that variable
                // is an instance of `Function` and we construct a CallableValue from its fn_ptr.
                let found_function = self.scopes.iter().rev().find_map(|sc| sc.fns.get(&name)).map(|f| CallableValue::from(f.clone()));
                match found_function {
                    Some(f) => Some(f),
                    None => {
                        let ptr = self.resolve_ptr_to_variable(&name);
                        let fn_val = self.builder.build_load(ptr, "").into_int_value();
                        let fn_val_ptr = self.builder.build_pointer_cast(
                            self.emit_extract_nan_tagged_obj(fn_val),
                            self.known_types.function.ptr_type(AddressSpace::Generic),
                            ""
                        );
                        let fn_ptr_value_t = self.builder.build_struct_gep(fn_val_ptr, 2, "fn_ptr_value_t").unwrap();
                        let fn_ptr_value_t = self.builder.build_load(fn_ptr_value_t, "loaded").into_int_value();

                        let env_ptr = self.builder.build_struct_gep(fn_val_ptr, 3, "").unwrap();
                        args[0] = self.builder.build_load(env_ptr, "").into();

                        convert_serialized_fn_ptr_to_callable(self, fn_ptr_value_t, num_args, fn_ret_type)
                    }
                }
            }
            TypedAstNode::Accessor(_, TypedAccessorNode { typ, target, field_idx, is_method, .. }) => {
                if !is_method { todo!(); }

                let rcv = self.visit(*target)?;
                args.push(rcv.into());

                let idx = self.context.i32_type().const_int(field_idx as u64, false);
                let fn_ptr_value_t = self.builder.build_call(self.known_fns.vtable_lookup, &[rcv.into(), idx.into()], "").try_as_basic_value().left().unwrap().into_int_value();
                if let Type::Fn(FnType { ret_type, arg_types, .. }) = &typ {
                    convert_serialized_fn_ptr_to_callable(self, fn_ptr_value_t, arg_types.len() + 1, ret_type)
                } else { unreachable!() }
            }
            _ => todo!()
        };
        let fn_value = fn_value.unwrap();

        for arg in node.args {
            let arg = if let Some(arg) = arg {
                self.visit(arg)?
            } else {
                self.val_none().as_basic_value_enum()
            };

            args.push(arg.into());
        }

        let res = self.builder.build_call(fn_value, args.as_slice(), "");
        let res = if node.typ == Type::Unit {
            self.val_none().as_basic_value_enum()
        } else {
            res.try_as_basic_value().left().unwrap()
        };

        Ok(res)
    }

    fn visit_instantiation(&mut self, _token: Token, _node: TypedInstantiationNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_accessor(&mut self, _token: Token, _node: TypedAccessorNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_for_loop(&mut self, _token: Token, _node: TypedForLoopNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_while_loop(&mut self, _token: Token, _node: TypedWhileLoopNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_break(&mut self, _token: Token) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_continue(&mut self, _token: Token) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
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
    builtins: HashSet<String>,
}

impl CapturedVariableFinder {
    fn find_captured_variables(fn_decl_node: &TypedFunctionDeclNode) -> HashMap<String, usize> {
        let builtins = {
            let mut s = HashSet::new();
            s.insert("println".to_string());
            s.insert("print".to_string());
            s.insert("None".to_string());
            s
        };
        let mut finder = CapturedVariableFinder { scopes: vec![(0, HashSet::new())], cur_fn_id: 0, captured_variables: HashMap::new(), builtins };

        let TypedFunctionDeclNode { args, body, .. } = fn_decl_node;
        for (tok, _, _, default_value) in args {
            if let Some(default_value) = default_value {
                finder.find_foreign_variables(default_value );
            }
            let var_name = Token::get_ident_name(tok);
            finder.add_variable_to_cur_scope(var_name);
        }
        for node in body {
            finder.find_foreign_variables(node);
        }

        finder.captured_variables
    }

    fn is_builtin(&self, var_name: &String) -> bool {
        self.builtins.contains(var_name)
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
                self.end_fn_scope();
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
                self.end_fn_scope();
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
                    self.end_fn_scope();
                }
            }
            TypedAstNode::ImportStatement(_, _) => {}
            TypedAstNode::_Nil(_) => {}
        }
    }
}
