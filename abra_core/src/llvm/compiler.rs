use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{BasicValue, FunctionValue};
use crate::common::typed_ast_visitor::TypedAstVisitor;
use crate::lexer::tokens::Token;
use crate::typechecker::typed_ast::{TypedAccessorNode, TypedArrayNode, TypedAssignmentNode, TypedBinaryNode, TypedBindingDeclNode, TypedEnumDeclNode, TypedForLoopNode, TypedFunctionDeclNode, TypedGroupedNode, TypedIdentifierNode, TypedIfNode, TypedImportNode, TypedIndexingNode, TypedInstantiationNode, TypedInvocationNode, TypedLambdaNode, TypedLiteralNode, TypedMapNode, TypedMatchNode, TypedReturnNode, TypedSetNode, TypedTupleNode, TypedTypeDeclNode, TypedUnaryNode, TypedWhileLoopNode};
use crate::TypedModule;

#[derive(Debug)]
pub enum CompilerError {}

pub struct Compiler<'a, 'ctx> {
    _context: &'ctx Context,
    _builder: &'a Builder<'ctx>,
    _module: &'a Module<'ctx>,
    _cur_fn: FunctionValue<'ctx>,
}

pub type ModEntryFn = unsafe extern "C" fn() -> i64;
pub const ENTRY_FN_NAME: &str = "__mod_entry";

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile_module(context: &'ctx Context, typed_module: TypedModule) -> Result<Module<'ctx>, CompilerError> {
        let builder = context.create_builder();
        let module = context.create_module("__test");

        let entry_fn = module.add_function(ENTRY_FN_NAME, context.i64_type().fn_type(&[], false), None);
        let entry_fn_bb = context.append_basic_block(entry_fn, "entry_fn_bb");
        builder.position_at_end(entry_fn_bb);

        let mut _compiler = Compiler {
            _context: &context,
            _builder: &builder,
            _module: &module,
            _cur_fn: entry_fn,
        };

        for _node in typed_module.typed_nodes {
        //     compiler.visit(node)?;
        }

        builder.build_return(Some(&context.i64_type().const_int(69, false).as_basic_value_enum()));

        Ok(module)
    }
}

impl<'a, 'ctx> TypedAstVisitor<(), CompilerError> for Compiler<'a, 'ctx> {
    fn visit_literal(&mut self, _token: Token, _node: TypedLiteralNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_unary(&mut self, _token: Token, _node: TypedUnaryNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_binary(&mut self, _token: Token, _node: TypedBinaryNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_grouped(&mut self, _token: Token, _node: TypedGroupedNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_array(&mut self, _token: Token, _node: TypedArrayNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_tuple(&mut self, _token: Token, _node: TypedTupleNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_map(&mut self, _token: Token, _node: TypedMapNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_set(&mut self, _token: Token, _node: TypedSetNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_lambda(&mut self, _token: Token, _node: TypedLambdaNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_binding_decl(&mut self, _token: Token, _node: TypedBindingDeclNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_function_decl(&mut self, _token: Token, _node: TypedFunctionDeclNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_type_decl(&mut self, _token: Token, _node: TypedTypeDeclNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_enum_decl(&mut self, _token: Token, _node: TypedEnumDeclNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_identifier(&mut self, _token: Token, _node: TypedIdentifierNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_assignment(&mut self, _token: Token, _node: TypedAssignmentNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_indexing(&mut self, _token: Token, _node: TypedIndexingNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_if_statement(&mut self, _is_stmt: bool, _token: Token, _node: TypedIfNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_if_expression(&mut self, _token: Token, _node: TypedIfNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_invocation(&mut self, _token: Token, _node: TypedInvocationNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_instantiation(&mut self, _token: Token, _node: TypedInstantiationNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_accessor(&mut self, _token: Token, _node: TypedAccessorNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_for_loop(&mut self, _token: Token, _node: TypedForLoopNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_while_loop(&mut self, _token: Token, _node: TypedWhileLoopNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_break(&mut self, _token: Token) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_continue(&mut self, _token: Token) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_return(&mut self, _token: Token, _node: TypedReturnNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_match_statement(&mut self, _is_stmt: bool, _token: Token, _node: TypedMatchNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_match_expression(&mut self, _token: Token, _node: TypedMatchNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_import_statement(&mut self, _token: Token, _node: TypedImportNode) -> Result<(), CompilerError> {
        todo!()
    }

    fn visit_nil(&mut self, _token: Token) -> Result<(), CompilerError> {
        todo!()
    }
}
