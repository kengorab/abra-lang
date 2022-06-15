use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::iter::repeat;
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType, IntType, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, CallableValue, FloatValue, FunctionValue, InstructionOpcode, IntValue, PointerValue};
use itertools::Itertools;
use abra_core::common::typed_ast_visitor::TypedAstVisitor;
use abra_core::lexer::tokens::Token;
use abra_core::parser::ast::{BinaryOp, UnaryOp};
use abra_core::typechecker::typechecker::TypedModule;
use abra_core::typechecker::typed_ast::{TypedAccessorNode, TypedArrayNode, TypedAssignmentNode, TypedAstNode, TypedBinaryNode, TypedBindingDeclNode, TypedEnumDeclNode, TypedForLoopNode, TypedFunctionDeclNode, TypedGroupedNode, TypedIdentifierNode, TypedIfNode, TypedImportNode, TypedIndexingNode, TypedInstantiationNode, TypedInvocationNode, TypedLambdaNode, TypedLiteralNode, TypedMapNode, TypedMatchNode, TypedReturnNode, TypedSetNode, TypedTupleNode, TypedTypeDeclNode, TypedUnaryNode, TypedWhileLoopNode};
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
    array_alloc: FunctionValue<'ctx>,
    array_insert: FunctionValue<'ctx>,
    array_get: FunctionValue<'ctx>,
    vtable_alloc_entry: FunctionValue<'ctx>,
    vtable_lookup: FunctionValue<'ctx>,
    type_id_for_val: FunctionValue<'ctx>,
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
            array_alloc: placeholder,
            array_insert: placeholder,
            array_get: placeholder,
            vtable_alloc_entry: placeholder,
            vtable_lookup: placeholder,
            type_id_for_val: placeholder,
            value_to_string: placeholder,
        }
    }

    fn is_initialized(&self) -> bool {
        let KnownFns { snprintf, printf, powf64, malloc, memcpy, double_to_value_t, value_t_to_double, string_alloc, string_concat, array_alloc, array_insert, array_get, vtable_alloc_entry, vtable_lookup, type_id_for_val, value_to_string } = self;
        [snprintf, printf, powf64, malloc, memcpy, double_to_value_t, value_t_to_double, string_alloc, string_concat, array_alloc, array_insert, array_get, vtable_alloc_entry, vtable_lookup, type_id_for_val, value_to_string].iter()
            .all(|f| f.get_name().to_str().unwrap().ne(PLACEHOLDER_FN_NAME))
    }
}

struct KnownTypes<'ctx> {
    string: StructType<'ctx>,
    array: StructType<'ctx>,
    obj_header_t: StructType<'ctx>,
}

impl<'ctx> KnownTypes<'ctx> {
    fn is_initialized(&self) -> bool {
        let KnownTypes { string, array, obj_header_t } = self;
        [string, array, obj_header_t].iter()
            .all(|t| t.get_name().unwrap().to_str().unwrap().ne(PLACEHOLDER_TYPE_NAME))
    }
}

// IMPORTANT! These must stay in sync with the constants in `rt.h`

const MASK_NAN: u64 = 0x7ffc000000000000;
const MASK_INT: u64 = MASK_NAN | 0x0002000000000000;
// const MASK_OBJ: u64 = MASK_NAN | 0x8000000000000000;

// const VAL_NONE: u64  = MASK_NAN | 0x0001000000000000;
const VAL_FALSE: u64 = MASK_NAN | 0x0001000000000001;
const VAL_TRUE: u64  = MASK_NAN | 0x0001000000000002;

const PAYLOAD_MASK_INT: u64 = 0x00000000ffffffff;
const PAYLOAD_MASK_OBJ: u64 = 0x0000ffffffffffff;

struct Scope {
    name: String,
    fns: HashMap<String, String>,
    _bindings: HashSet<String>,
}

pub struct Compiler<'a, 'ctx> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    cur_fn: FunctionValue<'ctx>,
    known_fns: KnownFns<'ctx>,
    known_types: KnownTypes<'ctx>,
    scopes: Vec<Scope>,
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
            },
            scopes: vec![Scope { name: "$root".to_string(), fns: HashMap::new(), _bindings: HashSet::new() }],
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

    fn init_int_type(&self)  {
        let vtable_entry = self.add_type_id_and_vtable("type_id_Int", 1);
        self.add_vtable_fn(vtable_entry, 0, "prelude__Int__toString", self.value_t().fn_type(&[self.value_t().into()], false));
    }

    fn init_float_type(&self) {
        let vtable_entry = self.add_type_id_and_vtable("type_id_Float", 1);
        self.add_vtable_fn(vtable_entry, 0, "prelude__Float__toString", self.value_t().fn_type(&[self.value_t().into()], false));
    }

    fn init_bool_type(&self)  {
        let vtable_entry = self.add_type_id_and_vtable("type_id_Bool", 1);
        self.add_vtable_fn(vtable_entry, 0, "prelude__Bool__toString", self.value_t().fn_type(&[self.value_t().into()], false));
    }

    fn init_string_type(&self)  {
        let vtable_entry = self.add_type_id_and_vtable("type_id_String", 3);
        self.add_vtable_fn(vtable_entry, 0, "prelude__String__toString", self.value_t().fn_type(&[self.value_t().into()], false));
        self.add_vtable_fn(vtable_entry, 1, "prelude__String__toLower", self.value_t().fn_type(&[self.value_t().into()], false));
        self.add_vtable_fn(vtable_entry, 2, "prelude__String__toUpper", self.value_t().fn_type(&[self.value_t().into()], false));
    }

    fn init_array_type(&self) {
        let vtable_entry = self.add_type_id_and_vtable("type_id_Array", 0);
        self.add_vtable_fn(vtable_entry, 0, "prelude__Array__toString", self.value_t().fn_type(&[self.value_t().into()], false));
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
        let array_alloc_type = self.value_t().fn_type(&[self.context.i32_type().into()], false);
        self.known_fns.array_alloc = self.module.add_function("array_alloc", array_alloc_type, None);
        let array_insert_type = self.context.void_type().fn_type(&[self.value_t().into(), self.context.i32_type().into(), self.value_t().into()], false);
        self.known_fns.array_insert = self.module.add_function("array_insert", array_insert_type, None);
        let array_get_type = self.value_t().fn_type(&[self.value_t().into(), self.context.i32_type().into()], false);
        self.known_fns.array_get = self.module.add_function("array_get", array_get_type, None);
        self.known_fns.vtable_alloc_entry = self.module.add_function(
            "vtable_alloc_entry",
            self.context.i64_type().ptr_type(AddressSpace::Generic).fn_type(&[self.context.i32_type().into(), self.context.i32_type().into()], false),
            None
        );
        self.known_fns.vtable_lookup = self.module.add_function(
            "vtable_lookup",
            self.value_t().fn_type(&[self.context.i32_type().into(), self.context.i32_type().into()], false),
            None
        );
        self.known_fns.type_id_for_val = self.module.add_function(
            "type_id_for_val",
            self.context.i32_type().fn_type(&[self.value_t().into()], false),
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
        self.known_types.array = {
            let array_type = self.context.opaque_struct_type("Array");
            array_type.set_body(&[
                self.context.i32_type().into(), // int64_t length
                self.context.i32_type().into(), // int64_t _capacity
                self.value_t().ptr_type(AddressSpace::Generic).into(), // void** items
            ], false);
            array_type
        };

        let entry_fn_type = self.context.void_type().fn_type(&[], false);
        let entry_fn = self.module.add_function(ENTRY_FN_NAME, entry_fn_type, None);
        let entry_fn_bb = self.context.append_basic_block(entry_fn, "entry_fn_bb");
        self.builder.position_at_end(entry_fn_bb);
        self.cur_fn = entry_fn;

        self.init_int_type();
        self.init_float_type();
        self.init_bool_type();
        self.init_string_type();
        self.init_array_type();

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
    fn println_debug(&self, fmt_str: &str, value: BasicValueEnum<'ctx>) {
        self.builder.build_call(
            self.known_fns.printf,
            &[self.builder.build_global_string_ptr(fmt_str, "fmt").as_basic_value_enum().into(), value.into()],
            "",
        );
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("There should always be at least 1 scope")
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

    fn alloc_array_obj_with_length(&self, length_val: IntValue<'ctx>) -> IntValue<'ctx> {
        self.builder.build_call(self.known_fns.array_alloc, &[length_val.into()], "").try_as_basic_value().left().unwrap().into_int_value()
    }

    fn array_obj_insert(&self, array_val: IntValue<'ctx>, index_val: IntValue<'ctx>, item_val: IntValue<'ctx>) {
        self.builder.build_call(self.known_fns.array_insert, &[array_val.into(), index_val.into(), item_val.into()], "");
    }

    fn value_t(&self) -> IntType<'ctx> {
        self.context.i64_type()
    }

    fn emit_nan_tagged_int_const(&self, int: i32) -> IntValue<'ctx> {
        self.emit_nan_tagged_int(self.context.i64_type().const_int(int as u64, false))
    }

    fn emit_nan_tagged_int(&self, int_val: IntValue<'ctx>) -> IntValue<'ctx> {
        let mask = self.context.i64_type().const_int(MASK_INT, false);
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

    // fn emit_nan_tagged_obj(&self, ptr: PointerValue<'ctx>) -> IntValue<'ctx> {
    //     let ptr = self.builder.build_cast(InstructionOpcode::PtrToInt, ptr, self.context.i64_type(), "");
    //     let mask = self.context.i64_type().const_int(MASK_OBJ, false);
    //     self.builder.build_or(mask, ptr.into_int_value(), "")
    // }

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

    fn visit_tuple(&mut self, _token: Token, _node: TypedTupleNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
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

    fn visit_binding_decl(&mut self, _token: Token, _node: TypedBindingDeclNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_function_decl(&mut self, _token: Token, node: TypedFunctionDeclNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let old_fn = self.cur_fn;

        let namespace = self.scopes.iter().map(|s| &s.name).join("::");
        let fn_name = Token::get_ident_name(&node.name);
        let fully_qualified_fn_name = format!("{}::{}", namespace, fn_name);
        self.current_scope_mut().fns.insert(fn_name, fully_qualified_fn_name.clone());

        let fn_type = self.value_t().fn_type(repeat(self.value_t().into()).take(node.args.len()).collect_vec().as_slice(), false);
        let func = self.module.add_function(&fully_qualified_fn_name, fn_type, None);
        let fn_bb = self.context.append_basic_block(func, "fn_body");
        self.builder.position_at_end(fn_bb);
        self.cur_fn = func;

        let body_len = node.body.len();
        for (idx, node) in node.body.into_iter().enumerate() {
            let value = self.visit(node)?;

            if idx == body_len - 1 {
                self.builder.build_return(Some(&value.as_basic_value_enum()));
            }
        }

        self.cur_fn = old_fn;
        self.builder.position_at_end(self.cur_fn.get_last_basic_block().unwrap());

        let res = self.emit_nan_tagged_int_const(0);
        Ok(res.into())
    }

    fn visit_type_decl(&mut self, _token: Token, _node: TypedTypeDeclNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_enum_decl(&mut self, _token: Token, _node: TypedEnumDeclNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_identifier(&mut self, _token: Token, _node: TypedIdentifierNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_assignment(&mut self, _token: Token, _node: TypedAssignmentNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_indexing(&mut self, _token: Token, _node: TypedIndexingNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_if_statement(&mut self, _is_stmt: bool, _token: Token, _node: TypedIfNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_if_expression(&mut self, _token: Token, _node: TypedIfNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_invocation(&mut self, _token: Token, node: TypedInvocationNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let mut args = vec![];

        let fn_value: Option<CallableValue<'ctx>> = match *node.target {
            TypedAstNode::Identifier(_, TypedIdentifierNode { name, .. }) => {
                self.scopes.iter()
                    .rev()
                    .find_map(|s| s.fns.get(&name))
                    .and_then(|name| self.module.get_function(name))
                    .map(|f| CallableValue::from(f))
            }
            TypedAstNode::Accessor(_, TypedAccessorNode { typ, target, field_idx, is_method, .. }) if is_method => {
                let rcv = self.visit(*target)?;
                args.push(rcv.into());

                let type_id = self.builder.build_call(self.known_fns.type_id_for_val, &[rcv.into()], "").try_as_basic_value().left().unwrap().into_int_value();
                let idx = self.context.i32_type().const_int(field_idx as u64, false);
                let fn_value = self.builder.build_call(self.known_fns.vtable_lookup, &[type_id.into(), idx.into()], "").try_as_basic_value().left().unwrap().into_int_value();
                let fn_ptr_typ = if let Type::Fn(FnType { ret_type, arg_types, .. }) = typ {
                    let args = repeat(self.value_t().into()).take(arg_types.len() + 1).collect_vec();
                    let fn_type = if *ret_type == Type::Unit {
                        self.context.void_type().fn_type(args.as_slice(), false)
                    } else {
                        self.value_t().fn_type(args.as_slice(), false)
                    };
                    fn_type.ptr_type(AddressSpace::Generic)
                } else { unreachable!() };
                let fn_value = self.builder.build_cast(InstructionOpcode::IntToPtr, fn_value, fn_ptr_typ, "").into_pointer_value();
                CallableValue::try_from(fn_value).ok()
            }
            _ => todo!()
        };
        let fn_value = fn_value.unwrap();

        for arg in node.args {
            let arg = arg.unwrap();
            let arg = self.visit(arg)?;
            args.push(arg.into());
        }

        let res = self.builder.build_call(fn_value, args.as_slice(), "");
        Ok(res.try_as_basic_value().left().unwrap())
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
        Ok(self.context.i64_type().const_zero().into())
    }
}
