use std::collections::{HashMap, HashSet};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, InstructionOpcode, IntValue, PointerValue};
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
    transmute_double_int64: FunctionValue<'ctx>,
    transmute_int64_double: FunctionValue<'ctx>,
}

impl<'ctx> KnownFns<'ctx> {
    fn initial_value(placeholder: FunctionValue<'ctx>) -> Self {
        KnownFns {
            snprintf: placeholder,
            printf: placeholder,
            powf64: placeholder,
            malloc: placeholder,
            memcpy: placeholder,
            transmute_double_int64: placeholder,
            transmute_int64_double: placeholder
        }
    }

    fn is_initialized(&self) -> bool {
        let KnownFns { snprintf, printf, powf64, malloc, memcpy, transmute_double_int64, transmute_int64_double } = self;
        [snprintf, printf, powf64, malloc, memcpy, transmute_double_int64, transmute_int64_double].iter()
            .all(|f| f.get_name().to_str().unwrap().ne(PLACEHOLDER_FN_NAME))
    }
}

struct KnownTypes<'ctx> {
    string: StructType<'ctx>,
    array: StructType<'ctx>,
}

impl<'ctx> KnownTypes<'ctx> {
    fn initial_value(placeholder: StructType<'ctx>) -> Self {
        KnownTypes {
            string: placeholder,
            array: placeholder,
        }
    }

    fn is_initialized(&self) -> bool {
        let KnownTypes { string, array } = self;
        [string, array].iter()
            .all(|f| f.get_name().unwrap().to_str().unwrap().ne(PLACEHOLDER_TYPE_NAME))
    }
}

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
            known_types: KnownTypes::initial_value(placeholder_type),
            scopes: vec![Scope { name: "$root".to_string(), fns: HashMap::new(), _bindings: HashSet::new() }],
        };
        compiler.init();

        let mut last_item = compiler.const0().as_basic_value_enum();
        let mut last_item_type = Type::Unit;
        for node in typed_module.typed_nodes {
            last_item_type = node.get_type();
            last_item = compiler.visit(node)?;
        }

        compiler.finalize(&last_item_type, last_item);

        // module.print_to_stderr();
        Ok(module)
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
        let transmute_double_int64_type = self.context.i64_type().fn_type(&[self.context.f64_type().into()], false);
        self.known_fns.transmute_double_int64 = self.module.add_function("transmute_double_int64", transmute_double_int64_type, None);
        let transmute_int64_double_type = self.context.f64_type().fn_type(&[self.context.i64_type().into()], false);
        self.known_fns.transmute_int64_double = self.module.add_function("transmute_int64_double", transmute_int64_double_type, None);

        let string_type = self.context.opaque_struct_type("String");
        string_type.set_body(&[
            self.context.i32_type().into(), // int64_t length
            self.str_type(), // char* chars
        ], false);
        self.known_types.string = string_type;
        let array_type = self.context.opaque_struct_type("Array");
        array_type.set_body(&[
            self.context.i32_type().into(), // int64_t length
            self.context.i32_type().into(), // int64_t _capacity
            self.context.i64_type().ptr_type(AddressSpace::Generic).into(), // void** items
        ], false);
        self.known_types.array = array_type;

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
        let fmt = if is_float { "%f" } else { "%lld" };
        let fmt_str = self.builder.build_global_string_ptr(fmt, "fmt").as_basic_value_enum();

        // Print formatted value, using extra snprintf to determine required buffer length
        //   int len = snprintf(NULL, 0, fmt, value);
        //   char *result = malloc(len + 1);
        //   snprintf(result, len + 1, fmt, value);
        let len = self.builder.build_call(
            self.known_fns.snprintf,
            &[self.null_ptr().into(), self.const0().into(), fmt_str.into(), value.into()],
            "len",
        ).try_as_basic_value().left().unwrap().into_int_value();
        let len_plus_1 = self.builder.build_int_add(len, self.const_int(1).into(), "len");
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
        let cond = self.builder.build_int_compare(IntPredicate::EQ, value.into_int_value(), self.context.bool_type().const_zero(), "cond");

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
        let phi = self.builder.build_phi(self.known_types.string.ptr_type(AddressSpace::Generic), "");
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
                unsafe { self.builder.build_gep(str_chars_mem, &[self.const_int(0)], "") },
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
            let arr_item = self.array_obj_get(inner_type, array, idx);
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
        let phi = self.builder.build_phi(self.known_types.string.ptr_type(AddressSpace::Generic), "");
        phi.add_incoming(&[(&if_then_val, if_then_bb), (&if_else_val, if_else_bb)]);
        phi.as_basic_value().into_pointer_value()
    }

    fn gen_value_to_str(&self, typ: &Type, value: BasicValueEnum<'ctx>) -> PointerValue<'ctx> {
        match typ {
            Type::Int => self.gen_num_to_string(false, value),
            Type::Float => self.gen_num_to_string(true, value),
            Type::Bool => self.gen_bool_to_str(value),
            Type::String => value.into_pointer_value(),
            Type::Array(inner) => self.gen_array_to_str(&inner, value.into_pointer_value()),
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

    fn llvm_type(&self, typ: &Type) -> BasicTypeEnum<'ctx> {
        match typ {
            Type::Int => self.context.i64_type().into(),
            Type::Float => self.context.f64_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::String => self.known_types.string.ptr_type(AddressSpace::Generic).into(),
            Type::Array(_) => self.known_types.array.ptr_type(AddressSpace::Generic).into(),
            Type::Fn(_) => todo!(),
            _ => todo!()
        }
    }

    fn llvm_fn_type(&self, fn_type: &FnType) -> FunctionType<'ctx> {
        let ret_type = self.llvm_type(&fn_type.ret_type);
        let arg_types = fn_type.arg_types.iter()
            .map(|(_, t, _)| self.llvm_type(t).into())
            .collect::<Vec<BasicMetadataTypeEnum<'ctx>>>();

        ret_type.fn_type(arg_types.as_slice(), false)
    }

    fn null_ptr(&self) -> BasicValueEnum<'ctx> {
        self.str_type().const_zero()
    }

    fn const0(&self) -> IntValue<'ctx> {
        self.context.i64_type().const_int(0, false)
    }

    fn const_int(&self, int: i64) -> IntValue<'ctx> {
        self.context.i64_type().const_int(int as u64, false)
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
            &[self.known_types.string.size_of().unwrap().into()],
            "str_mem",
        ).try_as_basic_value().left().unwrap().into_pointer_value();
        let str = self.builder.build_pointer_cast(str_mem, self.known_types.string.ptr_type(AddressSpace::Generic), "str");
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

    fn alloc_array_obj_with_length(&self, length_val: IntValue<'ctx>) -> PointerValue<'ctx> {
        let arr_mem = self.builder.build_call(
            self.known_fns.malloc,
            &[self.known_types.array.size_of().unwrap().into()],
            "arr_mem",
        ).try_as_basic_value().left().unwrap().into_pointer_value();
        let arr = self.builder.build_pointer_cast(arr_mem, self.known_types.array.ptr_type(AddressSpace::Generic), "arr");

        let item_size_val = self.context.i64_type().size_of();
        let items_len = self.builder.build_int_mul(
            item_size_val,
            self.builder.build_int_cast(length_val, self.context.i64_type(), ""),
            "items_len"
        );
        let items_mem = self.builder.build_call(self.known_fns.malloc, &[items_len.into()], "items_mem")
            .try_as_basic_value().left().unwrap().into_pointer_value();
        let items_mem = self.builder.build_pointer_cast(items_mem, self.context.i64_type().ptr_type(AddressSpace::Generic), "");

        let arr_length = self.builder.build_struct_gep(arr, 0, "arr_length").unwrap();
        self.builder.build_store(arr_length, length_val);
        let arr_capacity = self.builder.build_struct_gep(arr, 1, "arr_capacity").unwrap();
        self.builder.build_store(arr_capacity, length_val);
        let arr_items = self.builder.build_struct_gep(arr, 2, "arr_items").unwrap();
        self.builder.build_store(arr_items, items_mem);

        arr
    }

    fn array_obj_insert(&self, array_val: PointerValue<'ctx>, index_val: IntValue<'ctx>, item_val: BasicValueEnum<'ctx>) {
        let arr_items = self.builder.build_struct_gep(array_val, 2, "arr_items").unwrap();

        let arr_item_slot = unsafe { self.builder.build_gep(arr_items, &[index_val], "") };
        let arr_item_slot = self.builder.build_pointer_cast(arr_item_slot, self.context.i64_type().ptr_type(AddressSpace::Generic), "");
        let cast_item_val = if item_val.is_float_value() {
            self.builder.build_call(
                self.known_fns.transmute_double_int64,
                &[item_val.into()],
                ""
            ).try_as_basic_value().left().unwrap()
        } else if item_val.get_type() == self.context.bool_type().into() {
            self.builder.build_int_cast(item_val.into_int_value(), self.context.i64_type(), "").into()
        } else if item_val.is_int_value() {
            item_val
        } else {
            self.builder.build_cast(InstructionOpcode::PtrToInt, item_val, self.context.i64_type(), "")
        };
        self.builder.build_store(arr_item_slot.into(), cast_item_val);
    }

    fn array_obj_get(&self, inner_type: &Type, array_val: PointerValue<'ctx>, index_val: IntValue<'ctx>) -> BasicValueEnum<'ctx> {
        let arr_items = self.builder.build_struct_gep(array_val, 2, "arr_items").unwrap();
        let arr_item_slot = unsafe { self.builder.build_gep(arr_items, &[index_val], "") };
        let arr_item_slot = self.builder.build_pointer_cast(arr_item_slot, self.context.i64_type().ptr_type(AddressSpace::Generic), "");
        let arr_item = self.builder.build_load(arr_item_slot, "");
        match inner_type {
            Type::Int | Type::Unknown => arr_item,
            Type::Bool => self.builder.build_int_cast(arr_item.into_int_value(), self.context.bool_type(), "").into(),
            Type::Float => {
                self.builder.build_call(
                    self.known_fns.transmute_int64_double,
                    &[arr_item.into()],
                    ""
                ).try_as_basic_value().left().unwrap()
            }
            Type::String => self.builder.build_cast(InstructionOpcode::IntToPtr, arr_item, self.known_types.string.ptr_type(AddressSpace::Generic), ""),
            Type::Array(_) => self.builder.build_cast(InstructionOpcode::IntToPtr, arr_item, self.known_types.array.ptr_type(AddressSpace::Generic), ""),
            _ => todo!(),
        }
    }
}

impl<'a, 'ctx> TypedAstVisitor<BasicValueEnum<'ctx>, CompilerError> for Compiler<'a, 'ctx> {
    fn visit_literal(&mut self, _token: Token, node: TypedLiteralNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let value = match node {
            TypedLiteralNode::IntLiteral(v) => self.context.i64_type().const_int(v as u64, false).into(),
            TypedLiteralNode::FloatLiteral(v) => self.context.f64_type().const_float(v).into(),
            TypedLiteralNode::StringLiteral(v) => self.alloc_const_string_obj(v.as_str()).into(),
            TypedLiteralNode::BoolLiteral(v) => self.context.bool_type().const_int(if v { 1 } else { 0 }, false).into(),
        };

        Ok(value)
    }

    fn visit_unary(&mut self, _token: Token, node: TypedUnaryNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let value = match node.op {
            UnaryOp::Minus => {
                let is_int = if node.expr.get_type() == Type::Int { true } else { false };
                let value = self.visit(*node.expr)?;
                if is_int {
                    self.builder.build_int_neg(value.into_int_value(), "").into()
                } else {
                    self.builder.build_float_neg(value.into_float_value(), "").into()
                }
            }
            UnaryOp::Negate => {
                let value = self.visit(*node.expr)?;
                self.builder.build_not(value.into_int_value(), "").into()
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
                let right = self.visit(*node.right)?;

                match op {
                    BinaryOp::Add => self.builder.build_int_add(left.into_int_value(), right.into_int_value(), "").into(),
                    BinaryOp::Sub => self.builder.build_int_sub(left.into_int_value(), right.into_int_value(), "").into(),
                    BinaryOp::Mul => self.builder.build_int_mul(left.into_int_value(), right.into_int_value(), "").into(),
                    BinaryOp::Div => {
                        let left = self.builder.build_signed_int_to_float(left.into_int_value(), self.context.f64_type(), "left");
                        let right = self.builder.build_signed_int_to_float(right.into_int_value(), self.context.f64_type(), "right");

                        self.builder.build_float_div(left, right, "").into()
                    },
                    BinaryOp::Mod => self.builder.build_int_signed_rem(left.into_int_value(), right.into_int_value(), "").into(),
                    BinaryOp::Pow => {
                        let left = self.builder.build_signed_int_to_float(left.into_int_value(), self.context.f64_type(), "left");
                        let right = self.builder.build_signed_int_to_float(right.into_int_value(), self.context.f64_type(), "right");

                        self.builder.build_call(
                            self.known_fns.powf64,
                            &[left.into(), right.into()],
                            "",
                        ).try_as_basic_value().left().unwrap()
                    }
                    BinaryOp::Eq => self.builder.build_int_compare(IntPredicate::EQ, left.into_int_value(), right.into_int_value(), "").into(),
                    BinaryOp::Neq => self.builder.build_int_compare(IntPredicate::NE, left.into_int_value(), right.into_int_value(), "").into(),
                    BinaryOp::Gt => self.builder.build_int_compare(IntPredicate::SGT, left.into_int_value(), right.into_int_value(), "").into(),
                    BinaryOp::Gte => self.builder.build_int_compare(IntPredicate::SGE, left.into_int_value(), right.into_int_value(), "").into(),
                    BinaryOp::Lt => self.builder.build_int_compare(IntPredicate::SLT, left.into_int_value(), right.into_int_value(), "").into(),
                    BinaryOp::Lte => self.builder.build_int_compare(IntPredicate::SLE, left.into_int_value(), right.into_int_value(), "").into(),
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
                    self.builder.build_signed_int_to_float(left.into_int_value(), self.context.f64_type(), "left")
                } else { left.into_float_value() };
                let right = if rtype == Type::Int {
                    self.builder.build_signed_int_to_float(right.into_int_value(), self.context.f64_type(), "left")
                } else { right.into_float_value() };

                match op {
                    BinaryOp::Add => self.builder.build_float_add(left, right, "").into(),
                    BinaryOp::Sub => self.builder.build_float_sub(left, right, "").into(),
                    BinaryOp::Mul => self.builder.build_float_mul(left, right, "").into(),
                    BinaryOp::Div => self.builder.build_float_div(left, right, "").into(),
                    BinaryOp::Mod => self.builder.build_float_rem(left, right, "").into(),
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
                        phi.as_basic_value()
                    }
                    BinaryOp::Eq => self.builder.build_float_compare(FloatPredicate::OEQ, left, right, "").into(),
                    BinaryOp::Neq => self.builder.build_float_compare(FloatPredicate::ONE, left, right, "").into(),
                    BinaryOp::Gt => self.builder.build_float_compare(FloatPredicate::OGT, left, right, "").into(),
                    BinaryOp::Gte => self.builder.build_float_compare(FloatPredicate::OGE, left, right, "").into(),
                    BinaryOp::Lt => self.builder.build_float_compare(FloatPredicate::OLT, left, right, "").into(),
                    BinaryOp::Lte => self.builder.build_float_compare(FloatPredicate::OLE, left, right, "").into(),
                    BinaryOp::Coalesce => unreachable!("Coalesce op does not apply to 2 non-optionals"),
                    BinaryOp::And | BinaryOp::Or | BinaryOp::Xor | BinaryOp::AndEq | BinaryOp::OrEq => unreachable!("No boolean ops apply to numbers"),
                    BinaryOp::AddEq | BinaryOp::SubEq | BinaryOp::MulEq | BinaryOp::DivEq | BinaryOp::ModEq | BinaryOp::CoalesceEq => unreachable!("Assign ops are handled separately")
                }
            }
            (Type::Bool, Type::Bool, op) => {
                let left = self.visit(*node.left)?;
                let right = self.visit(*node.right)?;

                match op {
                    BinaryOp::Eq => self.builder.build_int_compare(IntPredicate::EQ, left.into_int_value(), right.into_int_value(), "").into(),
                    BinaryOp::Neq => self.builder.build_int_compare(IntPredicate::NE, left.into_int_value(), right.into_int_value(), "").into(),
                    BinaryOp::And | BinaryOp::Or => unreachable!("`and` & `or` handled as if-expressions for short-circuiting"),
                    BinaryOp::Xor => self.builder.build_xor(left.into_int_value(), right.into_int_value(), "").into(),
                    BinaryOp::Coalesce => unreachable!("Coalesce op does not apply to 2 non-optionals"),
                    BinaryOp::AddEq | BinaryOp::SubEq | BinaryOp::MulEq | BinaryOp::DivEq | BinaryOp::ModEq | BinaryOp::AndEq | BinaryOp::OrEq | BinaryOp::CoalesceEq => unreachable!("Assign ops are handled separately"),
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod | BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte | BinaryOp::Pow => unreachable!("No arithmetic ops apply to boolean values"),
                }
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

                self.alloc_string_obj(length, mem).into()
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
        let arr = self.alloc_array_obj_with_length(length_val);

        for (idx, item) in node.items.into_iter().enumerate() {
            let index_val = self.context.i32_type().const_int(idx as u64, false);
            let item_val = self.visit(item)?;
            self.array_obj_insert(arr, index_val, item_val);
        }

        Ok(arr.into())
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

        let fn_type = self.llvm_fn_type(&node.fn_type);
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

        let res = self.const0();
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
        let fn_value = if let TypedAstNode::Identifier(_, TypedIdentifierNode { name, .. }) = &*node.target {
            self.scopes.iter()
                .rev()
                .find_map(|s| s.fns.get(name))
                .and_then(|name| self.module.get_function(name))
        } else { unimplemented!() };
        let fn_value = fn_value.unwrap();

        let mut args = vec![];
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
        Ok(self.const0().into())
    }
}
