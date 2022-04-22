use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, IntValue};
use abra_core::common::typed_ast_visitor::TypedAstVisitor;
use abra_core::lexer::tokens::Token;
use abra_core::parser::ast::{BinaryOp, UnaryOp};
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
        let powf64_type = self.context.f64_type().fn_type(&[self.context.f64_type().into(), self.context.f64_type().into()], false);
        self.module.add_function("llvm.pow.f64", powf64_type, None);

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

                        let powf64_fn = self.get_function("llvm.pow.f64");
                        self.builder.build_call(powf64_fn, &[left.into(), right.into()], "").try_as_basic_value().left().unwrap()
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
                        let powf64_fn = self.get_function("llvm.pow.f64");

                        // if a < 0 { -(-a ** b) } else { a ** b }
                        let cond = self.builder.build_float_compare(FloatPredicate::OLT, left, self.context.f64_type().const_float(0.0).into(), "cond");
                        let function = self.cur_fn.unwrap();
                        let then_bb = self.context.append_basic_block(function, "then");
                        let else_bb = self.context.append_basic_block(function, "else");
                        let cont_bb = self.context.append_basic_block(function, "cont");
                        self.builder.build_conditional_branch(cond, then_bb, else_bb);

                        self.builder.position_at_end(then_bb);
                        let then_val = {
                            let left = self.builder.build_float_mul(left, self.context.f64_type().const_float(-1.0), "");
                            let left = self.builder.build_call(powf64_fn, &[left.into(), right.into()], "").try_as_basic_value().left().unwrap().into_float_value();
                            self.builder.build_float_mul(left, self.context.f64_type().const_float(-1.0), "")
                        };
                        self.builder.build_unconditional_branch(cont_bb);
                        let then_bb = self.builder.get_insert_block().unwrap();

                        self.builder.position_at_end(else_bb);
                        let else_val = self.builder.build_call(powf64_fn, &[left.into(), right.into()], "").try_as_basic_value().left().unwrap();
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
            _ => todo!()
        };

        Ok(value)
    }

    fn visit_grouped(&mut self, _token: Token, node: TypedGroupedNode) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        self.visit(*node.expr)
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
