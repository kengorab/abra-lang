use inkwell::{AddressSpace, IntPredicate};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, IntValue};
use abra_core::common::typed_ast_visitor::TypedAstVisitor;
use abra_core::lexer::tokens::Token;
use abra_core::parser::ast::UnaryOp;
use abra_core::typechecker::typechecker::TypedModule;
use abra_core::typechecker::typed_ast::{TypedAccessorNode, TypedArrayNode, TypedAssignmentNode, TypedBinaryNode, TypedBindingDeclNode, TypedEnumDeclNode, TypedForLoopNode, TypedFunctionDeclNode, TypedGroupedNode, TypedIdentifierNode, TypedIfNode, TypedImportNode, TypedIndexingNode, TypedInstantiationNode, TypedInvocationNode, TypedLambdaNode, TypedLiteralNode, TypedMapNode, TypedMatchNode, TypedReturnNode, TypedSetNode, TypedTupleNode, TypedTypeDeclNode, TypedUnaryNode, TypedWhileLoopNode};
use abra_core::typechecker::types::Type;

#[derive(Debug)]
pub enum CompilerError {}

pub struct Compiler<'a, 'ctx> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    cur_fn: Option<FunctionValue<'ctx>>,
}

#[cfg(not(test))]
pub type ModEntryFn = unsafe extern "C" fn() -> ();
#[cfg(test)]
pub type ModEntryFn = unsafe extern "C" fn() -> *const cty::c_char;

pub const ENTRY_FN_NAME: &str = "__mod_entry";

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile_module(context: &'ctx Context, typed_module: TypedModule, test_mode: bool) -> Result<Module<'ctx>, CompilerError> {
        let builder = context.create_builder();
        let module = context.create_module("__main");

        let mut compiler = Compiler {
            context: &context,
            builder: &builder,
            module: &module,
            cur_fn: None,
        };
        compiler.init(test_mode);

        let mut last_item = compiler.const0().as_basic_value_enum();
        let mut last_item_type = Type::Unit;
        for node in typed_module.typed_nodes {
            last_item_type = node.get_type();
            last_item = compiler.visit(node)?;
        }

        compiler.finalize(test_mode, last_item_type, last_item);

        // module.print_to_stderr();
        Ok(module)
    }

    fn init(&mut self, test_mode: bool) {
        let snprintf_type = self.context.i64_type().fn_type(&[self.str_type().into(), self.context.i8_type().into(), self.str_type().into()], true);
        self.module.add_function("snprintf", snprintf_type, None);
        let malloc_type = self.str_type().fn_type(&[self.context.i64_type().into()], false);
        self.module.add_function("malloc", malloc_type, None);
        let printf_type = self.context.i64_type().fn_type(&[self.str_type().into()], false);
        self.module.add_function("printf", printf_type, None);

        // If `test_mode` is true, the entrypoint function will return the last value in the module
        // as a string, rather than only printing that string to stdout.
        let entry_fn_type = if test_mode {
            self.str_type().fn_type(&[], false)
        } else {
            self.context.void_type().fn_type(&[], false)
        };
        let entry_fn = self.module.add_function(ENTRY_FN_NAME, entry_fn_type, None);
        let entry_fn_bb = self.context.append_basic_block(entry_fn, "entry_fn_bb");
        self.builder.position_at_end(entry_fn_bb);
        self.cur_fn = Some(entry_fn);
    }

    fn finalize(&self, test_mode: bool, last_item_type: Type, last_item: BasicValueEnum) {
        let result = match last_item_type {
            Type::Int | Type::Float => {
                let fmt = match last_item_type {
                    Type::Int => "%lld",
                    Type::Float => "%f",
                    _ => unreachable!()
                };
                let fmt_str = self.builder.build_global_string_ptr(fmt, "fmt").as_basic_value_enum();

                // Print formatted value, using extra snprintf to determine required buffer length
                //   int len = snprintf(NULL, 0, fmt, value);
                //   char *result = malloc(len + 1);
                //   snprintf(result, len + 1, fmt, value);
                let snprintf_fn = self.get_function("snprintf");
                let len = self.builder.build_call(
                    snprintf_fn,
                    &[self.null_ptr().into(), self.const0().into(), fmt_str.into(), last_item.into()],
                    "len",
                ).try_as_basic_value().left().unwrap();
                let len_plus_1 = self.builder.build_int_add(len.into_int_value(), self.const_int(1).into(), "len");
                let result = self.builder.build_call(
                    self.get_function("malloc"),
                    &[len_plus_1.into()],
                    "result",
                ).try_as_basic_value().left().unwrap();
                let result = self.builder.build_pointer_cast(result.into_pointer_value(), self.str_type().into_pointer_type(), "result");
                self.builder.build_call(
                    snprintf_fn,
                    &[result.into(), len_plus_1.into(), fmt_str.into(), last_item.into()],
                    "",
                ).try_as_basic_value().left().unwrap();
                result.as_basic_value_enum()
            }
            Type::Bool => {
                let cond = self.builder.build_int_compare(IntPredicate::EQ, last_item.into_int_value(), self.const0().into(), "cond");

                let function = self.cur_fn.unwrap();
                let then_bb = self.context.append_basic_block(function, "then");
                let else_bb = self.context.append_basic_block(function, "else");
                let cont_bb = self.context.append_basic_block(function, "cont");
                self.builder.build_conditional_branch(cond, then_bb, else_bb);

                self.builder.position_at_end(then_bb);
                let then_val = self.builder.build_global_string_ptr("false", "");
                self.builder.build_unconditional_branch(cont_bb);
                let then_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(else_bb);
                let else_val = self.builder.build_global_string_ptr("true", "");
                self.builder.build_unconditional_branch(cont_bb);
                let else_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(cont_bb);
                let phi = self.builder.build_phi(self.str_type(), "");
                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
                phi.as_basic_value()
            }
            _ => todo!()
        };

        // If `test_mode` is true, the entrypoint function will return the last value in the module
        // as a string, rather than only printing that string to stdout.
        let ret_val = if test_mode {
            result.as_basic_value_enum()
        } else {
            let printf_fn = self.get_function("printf");

            self.builder.build_call(
                printf_fn,
                &[self.builder.build_global_string_ptr("%s\n", "fmt").as_basic_value_enum().into(), result.into()],
                "",
            );
            self.const0().as_basic_value_enum()
        };
        self.builder.build_return(Some(&ret_val));
    }

    fn null_ptr(&self) -> BasicValueEnum {
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

    fn get_function<S: AsRef<str>>(&self, name: S) -> FunctionValue<'ctx> {
        self.module.get_function(name.as_ref()).expect(&format!("Expected function {} to exist", name.as_ref()))
    }
}

impl<'a, 'ctx> TypedAstVisitor<BasicValueEnum<'ctx>, CompilerError> for Compiler<'a, 'ctx> {
    fn visit_literal(&mut self, _token: Token, node: TypedLiteralNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let value = match node {
            TypedLiteralNode::IntLiteral(v) => self.context.i64_type().const_int(v as u64, false).into(),
            TypedLiteralNode::FloatLiteral(v) => self.context.f64_type().const_float(v).into(),
            TypedLiteralNode::StringLiteral(_) => todo!(),
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
                debug_assert!(node.typ == Type::Bool);
                let value = self.visit(*node.expr)?;
                self.builder.build_not(value.into_int_value(), "").into()
            }
        };

        Ok(value)
    }

    fn visit_binary(&mut self, _token: Token, _node: TypedBinaryNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_grouped(&mut self, _token: Token, _node: TypedGroupedNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
    }

    fn visit_array(&mut self, _token: Token, _node: TypedArrayNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
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

    fn visit_function_decl(&mut self, _token: Token, _node: TypedFunctionDeclNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
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

    fn visit_invocation(&mut self, _token: Token, _node: TypedInvocationNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        todo!()
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
        todo!()
    }
}
