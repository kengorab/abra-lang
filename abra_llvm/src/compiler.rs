use std::collections::{HashMap, HashSet};
use std::iter::repeat;
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum, IntType, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, FunctionValue, InstructionOpcode, IntValue, PointerValue};
use itertools::Itertools;
use abra_core::common::typed_ast_visitor::TypedAstVisitor;
use abra_core::lexer::tokens::Token;
use abra_core::parser::ast::{BinaryOp, UnaryOp};
use abra_core::typechecker::typechecker::TypedModule;
use abra_core::typechecker::typed_ast::{TypedAccessorNode, TypedArrayNode, TypedAssignmentNode, TypedAstNode, TypedBinaryNode, TypedBindingDeclNode, TypedEnumDeclNode, TypedForLoopNode, TypedFunctionDeclNode, TypedGroupedNode, TypedIdentifierNode, TypedIfNode, TypedImportNode, TypedIndexingNode, TypedInstantiationNode, TypedInvocationNode, TypedLambdaNode, TypedLiteralNode, TypedMapNode, TypedMatchNode, TypedReturnNode, TypedSetNode, TypedTupleNode, TypedTypeDeclNode, TypedUnaryNode, TypedWhileLoopNode};
use abra_core::typechecker::types::Type;

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
    alloc_array: FunctionValue<'ctx>,
    array_insert: FunctionValue<'ctx>,
    array_get: FunctionValue<'ctx>,
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
            alloc_array: placeholder,
            array_insert: placeholder,
            array_get: placeholder,
        }
    }

    fn is_initialized(&self) -> bool {
        let KnownFns { snprintf, printf, powf64, malloc, memcpy, double_to_value_t, value_t_to_double, alloc_array, array_insert, array_get } = self;
        [snprintf, printf, powf64, malloc, memcpy, double_to_value_t, value_t_to_double, alloc_array, array_insert, array_get].iter()
            .all(|f| f.get_name().to_str().unwrap().ne(PLACEHOLDER_FN_NAME))
    }
}

enum RTType<'ctx> {
    Primitive {
        typ: BasicTypeEnum<'ctx>,
        methods: HashMap<String, FunctionValue<'ctx>>
    },
    Struct {
        struct_type: StructType<'ctx>,
        methods: HashMap<String, FunctionValue<'ctx>>
    }
}

impl<'ctx> RTType<'ctx> {
    fn get_type(&self) -> BasicTypeEnum<'ctx> {
        match self {
            RTType::Primitive {typ,..} => typ.clone(),
            RTType::Struct { struct_type, .. } => struct_type.as_basic_type_enum(),
        }
    }

    fn methods(&self) -> &HashMap<String, FunctionValue<'ctx>> {
        match self {
            RTType::Primitive { methods, .. } |
            RTType::Struct { methods, .. } => methods,
        }
    }
}

struct KnownTypes<'ctx> {
    int: RTType<'ctx>,
    float: RTType<'ctx>,
    string: RTType<'ctx>,
    array: RTType<'ctx>,
}

impl<'ctx> KnownTypes<'ctx> {
    fn is_initialized(&self) -> bool {
        let KnownTypes { int: _, float: _, string, array } = self;
        [string, array].iter()
            .all(|t| match t {
                RTType::Primitive { .. } => unreachable!(),
                RTType::Struct { struct_type, .. } => struct_type.get_name().unwrap().to_str().unwrap().ne(PLACEHOLDER_TYPE_NAME)
            })
    }
}

// IMPORTANT! These must stay in sync with the constants in `rt.h`

const MASK_NAN: u64 = 0x7ffc000000000000;
const MASK_INT: u64 = MASK_NAN | 0x7ffc000000000000;
const MASK_OBJ: u64 = MASK_NAN | 0x8000000000000000;

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
                int: RTType::Primitive { typ: context.i64_type().into(), methods: HashMap::new() },
                float: RTType::Primitive { typ: context.f64_type().into(), methods: HashMap::new() },
                string: RTType::Struct { struct_type: placeholder_type, methods: HashMap::new() },
                array: RTType::Struct { struct_type: placeholder_type, methods: HashMap::new() },
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

    fn build_int_type(&self) -> RTType<'ctx> {
        let int_type = self.context.i64_type();

        let mut methods = HashMap::new();
        methods.insert("toString".to_string(), {
            let t = self.value_t().fn_type(&[self.value_t().into()], false);
            self.module.add_function("prelude__Int__toString", t, None)
        });

        RTType::Primitive { typ: int_type.into(), methods }
    }

    fn build_float_type(&self) -> RTType<'ctx> {
        let float_type = self.context.f64_type();

        let mut methods = HashMap::new();
        methods.insert("toString".to_string(), {
            let t = self.value_t().fn_type(&[self.value_t().into()], false);
            self.module.add_function("prelude__Float__toString", t, None)
        });

        RTType::Primitive { typ: float_type.into(), methods }
    }

    fn build_string_type(&self) -> RTType<'ctx> {
        let string_type = self.context.opaque_struct_type("String");
        string_type.set_body(&[
            self.context.i32_type().into(), // int32_t length
            self.str_type(), // char* chars
        ], false);

        let mut methods = HashMap::new();
        methods.insert("toString".to_string(), {
            let t = self.value_t().fn_type(&[self.value_t().into()], false);
            self.module.add_function("prelude__String__toString", t, None)
        });
        methods.insert("toUpper".to_string(), {
            let t = self.value_t().fn_type(&[self.value_t().into()], false);
            self.module.add_function("prelude__String__toUpper", t, None)
        });

        RTType::Struct { struct_type: string_type, methods }
    }

    fn build_array_type(&self) -> RTType<'ctx> {
        let array_type = self.context.opaque_struct_type("Array");
        array_type.set_body(&[
            self.context.i32_type().into(), // int64_t length
            self.context.i32_type().into(), // int64_t _capacity
            self.value_t().ptr_type(AddressSpace::Generic).into(), // void** items
        ], false);

        let methods = HashMap::new();

        RTType::Struct { struct_type: array_type, methods }
    }

    fn init(&mut self) {
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
        let alloc_array_type = self.value_t().fn_type(&[self.context.i32_type().into()], false);
        self.known_fns.alloc_array = self.module.add_function("alloc_array", alloc_array_type, None);
        let array_insert_type = self.context.void_type().fn_type(&[self.value_t().into(), self.context.i32_type().into(), self.value_t().into()], false);
        self.known_fns.array_insert = self.module.add_function("array_insert", array_insert_type, None);
        let array_get_type = self.value_t().fn_type(&[self.value_t().into(), self.context.i32_type().into()], false);
        self.known_fns.array_get = self.module.add_function("array_get", array_get_type, None);

        self.known_types.string = self.build_string_type();
        self.known_types.int = self.build_int_type();
        self.known_types.float = self.build_float_type();
        self.known_types.array = self.build_array_type();

        let entry_fn_type = self.context.void_type().fn_type(&[], false);
        let entry_fn = self.module.add_function(ENTRY_FN_NAME, entry_fn_type, None);
        let entry_fn_bb = self.context.append_basic_block(entry_fn, "entry_fn_bb");
        self.builder.position_at_end(entry_fn_bb);
        self.cur_fn = entry_fn;

        // The placeholder values were only used during initialization for convenience. They can
        // be disposed of now that all known_fns, known_types, and self.cur_fn are initialized.
        debug_assert!(self.known_fns.is_initialized());
        debug_assert!(self.known_types.is_initialized());
        unsafe {
            self.module.get_function(PLACEHOLDER_FN_NAME).unwrap().delete();
        }
    }

    fn gen_num_to_string(&self, is_float: bool, value: BasicValueEnum<'ctx>) -> PointerValue<'ctx> {
        let fmt = if is_float { "%f" } else { "%d" };
        let fmt_str = self.builder.build_global_string_ptr(fmt, "fmt").as_basic_value_enum();

        let value = if is_float {
            self.emit_extract_nan_tagged_float(value.into_int_value()).as_basic_value_enum()
        } else {
            self.emit_extract_nan_tagged_int(value.into_int_value()).as_basic_value_enum()
        };

        // Print formatted value, using extra snprintf to determine required buffer length
        //   int len = snprintf(NULL, 0, fmt, value);
        //   char *result = malloc(len + 1);
        //   snprintf(result, len + 1, fmt, value);
        let len = self.builder.build_call(
            self.known_fns.snprintf,
            &[self.null_ptr().into(), self.context.i64_type().const_zero().into(), fmt_str.into(), value.into()],
            "len",
        ).try_as_basic_value().left().unwrap().into_int_value();
        let len_plus_1 = self.builder.build_int_add(len, self.context.i64_type().const_int(1, false).into(), "len");
        let result = self.builder.build_call(
            self.known_fns.malloc,
            &[len_plus_1.into()],
            "result",
        ).try_as_basic_value().left().unwrap();
        let result = self.builder.build_pointer_cast(result.into_pointer_value(), self.str_type().into_pointer_type(), "result");
        self.builder.build_call(
            self.known_fns.snprintf,
            &[result.into(), len_plus_1.into(), fmt_str.into(), value.into()],
            "",
        ).try_as_basic_value().left().unwrap();

        self.alloc_string_obj(self.builder.build_int_cast(len, self.context.i32_type(), ""), result)
    }

    fn gen_bool_to_str(&self, value: BasicValueEnum<'ctx>) -> PointerValue<'ctx> {
        let value = self.emit_extract_nan_tagged_bool(value.into_int_value());
        let cond = self.builder.build_int_compare(
            IntPredicate::EQ,
            value,
            self.builder.build_int_cast(self.context.bool_type().const_zero(), self.context.i64_type(), ""),
            "cond"
        );

        let function = self.cur_fn;
        let then_bb = self.context.append_basic_block(function, "then");
        let else_bb = self.context.append_basic_block(function, "else");
        let cont_bb = self.context.append_basic_block(function, "cont");
        self.builder.build_conditional_branch(cond, then_bb, else_bb);

        self.builder.position_at_end(then_bb);
        let then_val = self.alloc_const_string_obj("false");
        self.builder.build_unconditional_branch(cont_bb);
        let then_bb = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(else_bb);
        let else_val = self.alloc_const_string_obj("true");
        self.builder.build_unconditional_branch(cont_bb);
        let else_bb = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(cont_bb);
        let phi = self.builder.build_phi(self.known_types.string.get_type().ptr_type(AddressSpace::Generic), "");
        phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
        phi.as_basic_value().into_pointer_value()
    }

    fn gen_array_to_str(&self, inner_type: &Type, array: PointerValue<'ctx>) -> PointerValue<'ctx> {
        let function = self.cur_fn;

        let arr_len = self.builder.build_load(self.builder.build_struct_gep(array, 0, "").unwrap(), "array_length").into_int_value();
        let arr_len = self.builder.build_int_cast(arr_len, self.context.i32_type(), "");

        let cond_bb = self.context.append_basic_block(function, "if_cond_bb");
        let if_then_bb = self.context.append_basic_block(function, "if_then_bb");
        let if_else_bb = self.context.append_basic_block(function, "if_else_bb");
        let if_end_bb = self.context.append_basic_block(function, "if_end_bb");

        self.builder.build_unconditional_branch(cond_bb);
        self.builder.position_at_end(cond_bb);
        let if_cond = self.builder.build_int_compare(IntPredicate::EQ, arr_len, self.context.i32_type().const_zero(), "");
        self.builder.build_conditional_branch(if_cond.into(), if_then_bb, if_else_bb);

        self.builder.position_at_end(if_then_bb);
        let if_then_val = self.alloc_const_string_obj("[]");
        self.builder.build_unconditional_branch(if_end_bb);
        let if_then_bb = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(if_else_bb);
        let if_else_val = {
            let lbrack_ch = self.context.i8_type().const_int(0x5B, false);
            let rbrack_ch = self.context.i8_type().const_int(0x5D, false);
            let comma_ch = self.context.i8_type().const_int(0x2C, false);
            let space_ch = self.context.i8_type().const_int(0x20, false);

            let idx_ptr = self.builder.build_alloca(self.context.i32_type(), "idx");
            self.builder.build_store(idx_ptr, self.context.i32_type().const_int(0, false));

            let str_size_ptr = self.builder.build_alloca(self.context.i32_type(), "str_size");
            // Account for '[', ']', '\0', and ", " for all but last item
            self.builder.build_store(
                str_size_ptr,
                self.builder.build_int_add(
                    self.context.i32_type().const_int(3, false),
                    self.builder.build_int_mul(
                        self.context.i32_type().const_int(2, false),
                        self.builder.build_int_sub(arr_len, self.context.i32_type().const_int(1, false), ""),
                        "",
                    ),
                    "",
                ),
            );
            let insert_idx_ptr = self.builder.build_alloca(self.context.i32_type(), "insert_idx");
            self.builder.build_store(insert_idx_ptr, self.context.i32_type().const_int(1, false));

            let str_chars_mem = self.builder.build_call(
                self.known_fns.malloc,
                &[self.context.i64_type().const_int(100, false).into()], // TODO: Fix arbitrary limiting of array's string repr length
                "str_chars_mem",
            ).try_as_basic_value().left().unwrap().into_pointer_value();
            self.builder.build_store(
                unsafe { self.builder.build_gep(str_chars_mem, &[self.context.i64_type().const_zero()], "") },
                lbrack_ch
            );

            let cond_bb = self.context.append_basic_block(function, "loop_cond_bb");
            let body_bb = self.context.append_basic_block(function, "loop_body_bb");
            let after_bb = self.context.append_basic_block(function, "loop_after_bb");

            self.builder.build_unconditional_branch(cond_bb);
            self.builder.position_at_end(cond_bb);

            let idx = self.builder.build_load(idx_ptr, "idx").into_int_value();
            let cond = self.builder.build_int_compare(IntPredicate::SLT, idx, arr_len, "loop_cond");
            self.builder.build_conditional_branch(cond.into(), body_bb, after_bb);

            self.builder.position_at_end(body_bb);
            let arr_item = self.array_obj_get(self.emit_nan_tagged_obj(array), idx);
            let arr_item_str = self.gen_value_to_str(inner_type, arr_item.into());

            let arr_item_str_len = self.builder.build_struct_gep(arr_item_str, 0, "").unwrap();
            let arr_item_str_len = self.builder.build_int_cast(self.builder.build_load(arr_item_str_len, "item_len").into_int_value(), self.context.i32_type(), "");
            self.builder.build_store(str_size_ptr, self.builder.build_int_add(self.builder.build_load(str_size_ptr, "").into_int_value(), arr_item_str_len, ""));
            let arr_item_str_chars = self.builder.build_struct_gep(arr_item_str, 1, "").unwrap();
            let arr_item_str_chars = self.builder.build_load(arr_item_str_chars, "");

            let cursor = unsafe { self.builder.build_gep(str_chars_mem, &[self.builder.build_load(insert_idx_ptr, "").into_int_value()], "cursor") };
            self.builder.build_call(
                self.known_fns.memcpy,
                &[cursor.into(), arr_item_str_chars.into(), arr_item_str_len.into()],
                "",
            ).try_as_basic_value().left().unwrap();
            self.builder.build_store(insert_idx_ptr, self.builder.build_int_add(self.builder.build_load(insert_idx_ptr, "").into_int_value(), arr_item_str_len, ""));
            let cursor = unsafe { self.builder.build_gep(str_chars_mem, &[self.builder.build_load(insert_idx_ptr, "").into_int_value()], "cursor") };
            self.builder.build_store(cursor, comma_ch);
            self.builder.build_store(insert_idx_ptr, self.builder.build_int_add(self.builder.build_load(insert_idx_ptr, "").into_int_value(), self.context.i32_type().const_int(1, false), ""));
            let cursor = unsafe { self.builder.build_gep(str_chars_mem, &[self.builder.build_load(insert_idx_ptr, "").into_int_value()], "cursor") };
            self.builder.build_store( cursor, space_ch);
            self.builder.build_store(insert_idx_ptr, self.builder.build_int_add(self.builder.build_load(insert_idx_ptr, "").into_int_value(), self.context.i32_type().const_int(1, false), ""));

            self.builder.build_store(idx_ptr, self.builder.build_int_add(idx, self.context.i32_type().const_int(1, false).into(), ""));
            self.builder.build_unconditional_branch(cond_bb);

            self.builder.position_at_end(after_bb);
            let cursor = unsafe { self.builder.build_gep(str_chars_mem, &[self.builder.build_int_sub(self.builder.build_load(insert_idx_ptr, "").into_int_value(), self.context.i32_type().const_int(2, false), "")], "cursor") };
            self.builder.build_store( cursor, rbrack_ch );
            let cursor = unsafe { self.builder.build_gep(str_chars_mem, &[self.builder.build_int_sub(self.builder.build_load(insert_idx_ptr, "").into_int_value(), self.context.i32_type().const_int(1, false), "")], "cursor") };
            self.builder.build_store( cursor, self.context.i8_type().const_zero() );

            // The length passed to alloc_string_obj should not account for the the null terminator
            let str_len_val = self.builder.build_int_sub(
                self.builder.build_load(str_size_ptr, "").into_int_value(),
                self.context.i32_type().const_int(1, false),
                "",
            );
            self.alloc_string_obj(str_len_val, str_chars_mem)
        };
        self.builder.build_unconditional_branch(if_end_bb);
        let if_else_bb = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(if_end_bb);
        let phi = self.builder.build_phi(self.known_types.string.get_type().ptr_type(AddressSpace::Generic), "");
        phi.add_incoming(&[(&if_then_val, if_then_bb), (&if_else_val, if_else_bb)]);
        phi.as_basic_value().into_pointer_value()
    }

    fn gen_value_to_str(&self, typ: &Type, value: BasicValueEnum<'ctx>) -> PointerValue<'ctx> {
        match typ {
            Type::Int => self.gen_num_to_string(false, value),
            Type::Float => self.gen_num_to_string(true, value),
            Type::Bool => self.gen_bool_to_str(value),
            Type::String => {
                let ptr = self.emit_extract_nan_tagged_obj(value.into_int_value());
                self.builder.build_cast(InstructionOpcode::BitCast, ptr, self.known_types.string.get_type().ptr_type(AddressSpace::Generic), "").into_pointer_value()
            },
            Type::Array(inner) => {
                let ptr = self.emit_extract_nan_tagged_obj(value.into_int_value());
                let ptr = self.builder.build_cast(InstructionOpcode::BitCast, ptr, self.known_types.array.get_type().ptr_type(AddressSpace::Generic), "").into_pointer_value();
                self.gen_array_to_str(&inner, ptr)
            },
            _ => self.alloc_const_string_obj("todo")
        }
    }

    fn finalize(&self,  last_item_type: &Type, last_item: BasicValueEnum<'ctx>) {
        if *last_item_type != Type::Unit {
            let result_string= self.gen_value_to_str(last_item_type, last_item);
            let result_string_chars = self.builder.build_struct_gep(result_string, 1, "").unwrap();
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

    fn null_ptr(&self) -> BasicValueEnum<'ctx> {
        self.str_type().const_zero()
    }

    fn str_type(&self) -> BasicTypeEnum<'ctx> {
        self.context.i8_type().ptr_type(AddressSpace::Generic).as_basic_type_enum()
    }

    fn alloc_string_obj(&self, length_val: IntValue<'ctx>, chars_val: PointerValue<'ctx>) -> PointerValue<'ctx> {
        // String* str = (String*) malloc(sizeof(String));
        // str->length = <length_val>;
        // str->chars = <chars_val>;
        let str_mem = self.builder.build_call(
            self.known_fns.malloc,
            &[self.known_types.string.get_type().size_of().unwrap().into()],
            "str_mem",
        ).try_as_basic_value().left().unwrap().into_pointer_value();
        let str = self.builder.build_pointer_cast(str_mem, self.known_types.string.get_type().ptr_type(AddressSpace::Generic), "str");
        let str_length = self.builder.build_struct_gep(str, 0, "str_length").unwrap();
        self.builder.build_store(str_length, length_val);
        let str_chars = self.builder.build_struct_gep(str, 1, "str_chars").unwrap();
        self.builder.build_store(str_chars, chars_val);

        str
    }

    fn alloc_const_string_obj<S: AsRef<str>>(&self, s: S) -> PointerValue<'ctx> {
        let str = s.as_ref();
        let len = str.len();

        self.alloc_string_obj(
            self.context.i32_type().const_int(len as u64, false),
            self.builder.build_global_string_ptr(str, "").as_pointer_value(),
        )
    }

    fn alloc_array_obj_with_length(&self, length_val: IntValue<'ctx>) -> IntValue<'ctx> {
        self.builder.build_call(self.known_fns.alloc_array, &[length_val.into()], "").try_as_basic_value().left().unwrap().into_int_value()
    }

    fn array_obj_insert(&self, array_val: IntValue<'ctx>, index_val: IntValue<'ctx>, item_val: IntValue<'ctx>) {
        self.builder.build_call(self.known_fns.array_insert, &[array_val.into(), index_val.into(), item_val.into()], "");
    }

    fn array_obj_get(&self, array_val: IntValue<'ctx>, index_val: IntValue<'ctx>) -> BasicValueEnum<'ctx> {
        self.builder.build_call(self.known_fns.array_get, &[array_val.into(), index_val.into()], "").try_as_basic_value().left().unwrap()
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
            self.builder.build_int_cast(bool_val, self.context.i64_type(), ""),
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
            TypedLiteralNode::StringLiteral(v) => {
                let ptr = self.alloc_const_string_obj(v.as_str());
                self.emit_nan_tagged_obj(ptr).as_basic_value_enum()
            },
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
            (ltype @ Type::String, rtype @ Type::Int, BinaryOp::Add) |
            (ltype @ Type::String, rtype @ Type::Float, BinaryOp::Add) |
            (ltype @ Type::String, rtype @ Type::Bool, BinaryOp::Add) |
            (ltype @ Type::Int, rtype @ Type::String, BinaryOp::Add) |
            (ltype @ Type::Float, rtype @ Type::String, BinaryOp::Add) |
            (ltype @ Type::Bool, rtype @ Type::String, BinaryOp::Add) |
            (ltype @ Type::String, rtype @ Type::String, BinaryOp::Add) => {
                let left = self.visit(*node.left)?;
                let right = self.visit(*node.right)?;

                let left_string = self.gen_value_to_str(&ltype, left);
                let left_len = self.builder.build_load(self.builder.build_struct_gep(left_string, 0, "left_len").unwrap(), "left_len").into_int_value();
                let left_chars = self.builder.build_load(self.builder.build_struct_gep(left_string, 1, "left_chars").unwrap(), "").into_pointer_value();
                let right_string = self.gen_value_to_str(&rtype, right);
                let right_len = self.builder.build_load(self.builder.build_struct_gep(right_string, 0, "right_len").unwrap(), "right_len").into_int_value();
                let right_chars = self.builder.build_load(self.builder.build_struct_gep(right_string, 1, "right_chars").unwrap(), "").into_pointer_value();

                let length = self.builder.build_int_add(left_len, right_len, "length");
                let length_plus_1 = self.builder.build_int_cast(
                    self.builder.build_int_add(length, self.context.i32_type().const_int(1, false).into(), ""),
                    self.context.i64_type(),
                    ""
                );
                let mem = self.builder.build_call(
                    self.known_fns.malloc,
                    &[length_plus_1.into()],
                    "mem"
                ).try_as_basic_value().left().unwrap().into_pointer_value();
                self.builder.build_call(self.known_fns.memcpy, &[mem.into(), left_chars.into(), left_len.into()], "")
                    .try_as_basic_value().left().unwrap();
                let mem_offset = unsafe { self.builder.build_gep(mem, &[left_len], "") };
                self.builder.build_call(self.known_fns.memcpy, &[mem_offset.into(), right_chars.into(), right_len.into()], "")
                    .try_as_basic_value().left().unwrap();
                let mem_offset = unsafe { self.builder.build_gep(mem, &[length], "") };
                self.builder.build_store(mem_offset, self.context.i8_type().const_zero());

                self.emit_nan_tagged_obj(self.alloc_string_obj(length, mem)).as_basic_value_enum()
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

        let fn_value = match *node.target {
            TypedAstNode::Identifier(_, TypedIdentifierNode { name, .. }) => {
                self.scopes.iter()
                    .rev()
                    .find_map(|s| s.fns.get(&name))
                    .and_then(|name| self.module.get_function(name))
            }
            TypedAstNode::Accessor(_, TypedAccessorNode { target, field_ident, is_method, .. }) if is_method => {
                let target_type = target.get_type();
                let method_name = Token::get_ident_name(&field_ident);
                let fn_value = match target_type {
                    Type::Int => self.known_types.int.methods().get(&method_name),
                    Type::Float => self.known_types.float.methods().get(&method_name),
                    Type::String => self.known_types.string.methods().get(&method_name),
                    _ => unimplemented!()
                };
                let fn_value = fn_value.map(|f| f.clone());
                let rcv = self.visit(*target)?;
                args.push(rcv.into());

                fn_value
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
