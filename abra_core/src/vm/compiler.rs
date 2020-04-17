use crate::builtins::native_fns::{NATIVE_FNS_MAP, NativeFn};
use crate::common::typed_ast_visitor::TypedAstVisitor;
use crate::lexer::tokens::Token;
use crate::parser::ast::{UnaryOp, BinaryOp, IndexingMode};
use crate::vm::opcode::Opcode;
use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedUnaryNode, TypedBinaryNode, TypedArrayNode, TypedBindingDeclNode, TypedAssignmentNode, TypedIndexingNode, TypedGroupedNode, TypedIfNode, TypedFunctionDeclNode, TypedIdentifierNode, TypedInvocationNode, TypedWhileLoopNode, TypedForLoopNode, TypedTypeDeclNode, TypedMapNode, TypedAccessorNode, TypedInstantiationNode};
use crate::typechecker::types::Type;
use crate::vm::value::{Value, Obj};
use crate::vm::prelude::Prelude;
use crate::builtins::native_types;

#[derive(Debug, PartialEq)]
pub struct Local {
    name: String,
    depth: usize,
    is_captured: bool,
    is_closed: bool,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum UpvalueCaptureKind {
    Local { local_idx: usize },
    Upvalue { upvalue_idx: usize },
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Upvalue {
    pub capture_kind: UpvalueCaptureKind,
    depth: usize,
}

#[derive(Debug, PartialEq)]
enum ScopeKind { Root, If, Func, Loop, Block }

#[derive(Debug, PartialEq)]
struct Scope {
    kind: ScopeKind,
    num_locals: usize,
    first_local_idx: Option<usize>,
}

pub struct Compiler {
    code: Vec<u8>,
    constants: Vec<Value>,
    scopes: Vec<Scope>,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
    interrupt_offset_slots: Vec<usize>,
    metadata: Metadata,
    prelude: Prelude,
}

#[derive(Debug, Default, PartialEq)]
pub struct Metadata {
    pub loads: Vec<String>,
    pub stores: Vec<String>,
    pub uv_loads: Vec<String>,
    pub uv_stores: Vec<String>,
    pub field_gets: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub constants: Vec<Value>,
    pub code: Vec<u8>,
}

pub fn compile(ast: Vec<TypedAstNode>) -> Result<(Module, Metadata), ()> {
    let metadata = Metadata::default();
    let root_scope = Scope { kind: ScopeKind::Root, num_locals: 0, first_local_idx: None };

    let mut compiler = Compiler {
        code: Vec::new(),
        constants: Vec::new(),
        scopes: vec![root_scope],
        locals: Vec::new(),
        upvalues: Vec::new(),
        interrupt_offset_slots: Vec::new(),
        metadata,
        prelude: Prelude::new(),
    };

    let len = ast.len();
    let mut last_line = 0;
    for (idx, node) in ast.into_iter().enumerate() {
        let line = node.get_token().get_position().line;
        let should_pop = should_pop_after_node(&node);
        compiler.visit(node).unwrap();

        if idx != len - 1 && should_pop {
            compiler.write_opcode(Opcode::Pop, line);
        }
        last_line = line
    }
    compiler.write_opcode(Opcode::Return, last_line + 1);

    let module = Module { constants: compiler.constants, code: compiler.code };
    Ok((module, compiler.metadata))
}

fn should_pop_after_node(node: &TypedAstNode) -> bool {
    // Really this function could be `is_expression`, with the exception of invocations: invocation
    // of Unit invokables results in no value pushed to the stack, so there is nothing to pop
    match node {
        TypedAstNode::BindingDecl(_, _) |
        TypedAstNode::FunctionDecl(_, _) |
        TypedAstNode::TypeDecl(_, _) |
        TypedAstNode::IfStatement(_, _) |
        TypedAstNode::Break(_) | // This is here for completeness; the return type for this node should never matter
        TypedAstNode::ForLoop(_, _) |
        TypedAstNode::WhileLoop(_, _) => false,
        TypedAstNode::Invocation(_, TypedInvocationNode { typ, .. }) => typ != &Type::Unit,
        _ => true
    }
}

impl Compiler {
    #[inline]
    fn write_opcode(&mut self, opcode: Opcode, line: usize) {
        self.write_byte(opcode as u8, line);
    }

    #[inline]
    fn write_byte(&mut self, byte: u8, _line: usize) { // TODO: Fix lines
        self.code.push(byte);
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.get_constant_index(&value)
            .unwrap_or_else(|| {
                self.constants.push(value);
                (self.constants.len() - 1) as u8
            })
    }

    fn get_constant_index(&self, value: &Value) -> Option<u8> {
        self.constants.iter()
            .position(|v| v == value)
            .map(|v| v as u8)
    }

    fn write_constant(&mut self, value: Value, line: usize) -> u8 {
        let const_idx = self.add_constant(value);
        self.write_opcode(Opcode::Constant, line);
        self.write_byte(const_idx, line);
        const_idx
    }

    #[inline] // This is could eventually stand to be deprecated
    fn get_native_fn(name: &str) -> NativeFn {
        NATIVE_FNS_MAP.get(name).unwrap().clone().clone()
    }

    fn write_pops(&mut self, num_pops: usize, line: usize) {
        if num_pops == 1 {
            self.write_opcode(Opcode::Pop, line);
        } else if num_pops != 0 {
            self.write_opcode(Opcode::PopN, line);
            self.write_byte(num_pops as u8, line);
        }
    }

    fn write_pops_for_closure(&mut self, popped_locals: Vec<Local>, line: usize) {
        for local in popped_locals.iter().rev() {
            if local.name == "<ret>".to_string() { continue; }

            if local.is_captured && !local.is_closed {
                self.write_opcode(Opcode::CloseUpvalueAndPop, line);
            } else {
                self.write_opcode(Opcode::Pop, line);
            }
        }
    }

    fn write_int_constant(&mut self, number: u32, line: usize) {
        let ops = vec![Opcode::IConst0, Opcode::IConst1, Opcode::IConst2, Opcode::IConst3, Opcode::IConst4];
        match ops.get(number as usize) {
            Some(op) => self.write_opcode((*op).clone(), line),
            None => {
                self.write_constant(Value::Int(number as i64), line);
            }
        }
    }

    fn write_store_local_instr(&mut self, stack_slot: usize, line: usize) {
        let ops = vec![Opcode::LStore0, Opcode::LStore1, Opcode::LStore2, Opcode::LStore3, Opcode::LStore4];
        match ops.get(stack_slot) {
            Some(op) => self.write_opcode((*op).clone(), line),
            None => {
                self.write_opcode(Opcode::LStore, line);
                self.write_byte(stack_slot as u8, line);
            }
        }
    }

    fn write_load_local_instr(&mut self, stack_slot: usize, line: usize) {
        let ops = vec![Opcode::LLoad0, Opcode::LLoad1, Opcode::LLoad2, Opcode::LLoad3, Opcode::LLoad4];
        match ops.get(stack_slot) {
            Some(op) => self.write_opcode((*op).clone(), line),
            None => {
                self.write_opcode(Opcode::LLoad, line);
                self.write_byte(stack_slot as u8, line);
            }
        }
    }

    fn write_store_upvalue_instr(&mut self, upvalue_idx: usize, line: usize) {
        let ops = vec![Opcode::UStore0, Opcode::UStore1, Opcode::UStore2, Opcode::UStore3, Opcode::UStore4];
        match ops.get(upvalue_idx) {
            Some(op) => self.write_opcode((*op).clone(), line),
            None => {
                self.write_opcode(Opcode::UStore, line);
                self.write_byte(upvalue_idx as u8, line);
            }
        }
    }

    fn write_load_upvalue_instr(&mut self, upvalue_idx: usize, line: usize) {
        let ops = vec![Opcode::ULoad0, Opcode::ULoad1, Opcode::ULoad2, Opcode::ULoad3, Opcode::ULoad4];
        match ops.get(upvalue_idx) {
            Some(op) => self.write_opcode((*op).clone(), line),
            None => {
                self.write_opcode(Opcode::ULoad, line);
                self.write_byte(upvalue_idx as u8, line);
            }
        }
    }

    // A local is resolved wrt the current function scope - any other notion of 'depth'
    // (ie. loops/ifs/etc) is irrelevant. If a local is present in an upper func scope, then it
    // should be captured as an `upvalue`.
    fn resolve_local<S: AsRef<str>>(&mut self, ident: S, depth: usize) -> Option<(&mut Local, usize)> {
        let locals_at_depth = self.locals.iter_mut()
            .filter(|Local { depth: local_depth, .. }| local_depth == &depth)
            .rev()
            .collect::<Vec<&mut Local>>();

        let mut idx = locals_at_depth.len();
        for local in locals_at_depth {
            idx -= 1;
            if local.name == ident.as_ref().to_string() {
                return Some((local, idx));
            }
        }

        return None;
    }

    fn resolve_upvalue<S: AsRef<str>>(&mut self, ident: S, depth: i64) -> Option<usize> {
        if depth < 0 { return None; }
        let depth = depth as usize;

        #[inline]
        fn add_upvalue(zelf: &mut Compiler, depth: usize, kind: UpvalueCaptureKind) -> usize {
            for (idx, upvalue) in zelf.upvalues.iter().enumerate() {
                if upvalue.capture_kind == kind && upvalue.depth == depth {
                    return match upvalue.capture_kind {
                        UpvalueCaptureKind::Local { .. } => idx,
                        UpvalueCaptureKind::Upvalue { upvalue_idx } => upvalue_idx
                    };
                }
            }

            zelf.upvalues.push(Upvalue { capture_kind: kind, depth });
            zelf.upvalues.iter().filter(|uv| uv.depth == depth).count() - 1
        }

        match self.resolve_local(&ident, depth) {
            Some((local, local_idx)) => {
                local.is_captured = true;

                let capture_kind = UpvalueCaptureKind::Local { local_idx };
                let upvalue_idx = add_upvalue(self, depth, capture_kind);
                Some(upvalue_idx)
            }
            None => match self.resolve_upvalue(ident, depth as i64 - 1) {
                None => None,
                Some(upvalue_idx) => {
                    let capture_kind = UpvalueCaptureKind::Upvalue { upvalue_idx };
                    let upvalue_idx = add_upvalue(self, depth, capture_kind);
                    Some(upvalue_idx)
                }
            }
        }
    }

    fn get_fn_depth(&self) -> usize {
        self.scopes.iter().filter(|s| s.kind == ScopeKind::Func).count()
    }

    fn push_local<S: AsRef<str>>(&mut self, name: S) -> usize {
        self.locals.push(Local {
            name: name.as_ref().to_string(),
            depth: self.get_fn_depth(),
            is_captured: false,
            is_closed: false,
        });

        let mut scope = self.scopes.last_mut().unwrap();
        scope.num_locals += 1;
        if scope.first_local_idx == None {
            scope.first_local_idx = Some(self.locals.len() - 1);
        }

        self.locals.len() - 1
    }

    fn push_scope(&mut self, kind: ScopeKind) {
        self.scopes.push(Scope { kind, num_locals: 0, first_local_idx: None });
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn pop_scope_locals(&mut self) -> Vec<Local> {
        let &Scope { num_locals, .. } = self.current_scope();
        let split_idx = ((self.locals.len() as i64) - (num_locals as i64)) as usize;
        self.locals.split_off(split_idx)
    }

    fn current_scope(&self) -> &Scope {
        self.scopes.last().expect("There should be at least 1 scope")
    }

    // Called from visit_for_loop and visit_while_loop, but it has to be up here since it's
    // not part of the TypedAstVisitor trait.
    fn visit_loop_body(
        &mut self,
        body: Vec<TypedAstNode>,
        cond_slot_idx: usize, // The slot representing the start of the loop conditional
        cond_jump_offset_slot_idx: usize, // The slot representing the end of the loop
    ) -> Result<usize, ()> {
        let body_len = body.len();
        let mut last_line = 0;
        for (idx, node) in body.into_iter().enumerate() {
            let line = node.get_token().get_position().line;
            last_line = line;
            let is_last_node = idx == body_len - 1;

            let should_pop = should_pop_after_node(&node);
            let is_interrupt = match &node {
                TypedAstNode::Break(_) => true,
                _ => false
            };
            self.visit(node)?;

            if should_pop {
                self.write_opcode(Opcode::Pop, line);
            }

            // In an interrupt (ie. a break) the local-popping and jumping bytecode
            // will already have been emitted in `visit_break`; all that needs to happen here is
            // for the compiler to no longer care about the locals in this current scope.
            // We also break out of the loop, since it's unnecessary to compile further than a break.
            if is_interrupt {
                self.pop_scope_locals();
                break;
            }

            if is_last_node {
                let num_pops = self.pop_scope_locals().len();
                self.write_pops(num_pops, line);
            }
        }

        // Calculate the number of bytes needed to jump back in order to evaluate the cond again
        let offset_to_cond = self.code.len()
            .checked_sub(cond_slot_idx)
            .expect("conditional offset slot should be <= end of loop body");
        let offset_to_cond = offset_to_cond + 2; // Account for JumpB <imm>
        self.write_opcode(Opcode::JumpB, last_line);
        self.write_byte(offset_to_cond as u8, last_line);

        // Calculate the number of bytes needed to initially skip over body, if cond was false
        let code = &mut self.code;
        let body_len_bytes = code.len()
            .checked_sub(cond_jump_offset_slot_idx)
            .expect("jump offset slot should be <= end of loop body");
        *code.get_mut(cond_jump_offset_slot_idx - 1).unwrap() = body_len_bytes as u8;

        // Fill in any break-jump slots that have been accrued during compilation of this loop
        // Note: for nested loops, break-jumps will break out of inner loop only
        let interrupt_offset_slots = self.interrupt_offset_slots.drain(..).collect::<Vec<usize>>();
        for slot_idx in interrupt_offset_slots {
            let code = &mut self.code;
            let break_jump_offset = code.len()
                .checked_sub(slot_idx)
                .expect("break jump offset slots should be <= end of loop body");
            *code.get_mut(slot_idx - 1).unwrap() = break_jump_offset as u8;
        }
        Ok(last_line)
    }
}

impl TypedAstVisitor<(), ()> for Compiler {
    fn visit_literal(&mut self, token: Token, node: TypedLiteralNode) -> Result<(), ()> {
        let line = token.get_position().line;

        if let TypedLiteralNode::BoolLiteral(val) = node {
            let opcode = if val { Opcode::T } else { Opcode::F };
            self.write_opcode(opcode, line);
            return Ok(());
        } else if let TypedLiteralNode::IntLiteral(val) = node {
            self.write_int_constant(val as u32, line);
            return Ok(());
        }

        let const_idx = match node {
            TypedLiteralNode::FloatLiteral(val) =>
                self.add_constant(Value::Float(val)),
            TypedLiteralNode::StringLiteral(val) =>
                self.add_constant(Value::Obj(Obj::StringObj { value: Box::new(val) })),
            TypedLiteralNode::IntLiteral(_) | TypedLiteralNode::BoolLiteral(_) => unreachable!() // Handled in if-let above
        };

        self.write_opcode(Opcode::Constant, line);
        self.write_byte(const_idx, line);
        Ok(())
    }

    fn visit_unary(&mut self, token: Token, node: TypedUnaryNode) -> Result<(), ()> {
        let line = token.get_position().line;

        self.visit(*node.expr)?;
        match node.op {
            UnaryOp::Minus => self.write_opcode(Opcode::Invert, line),
            UnaryOp::Negate => self.write_opcode(Opcode::Negate, line),
        }
        Ok(())
    }

    fn visit_binary(&mut self, token: Token, node: TypedBinaryNode) -> Result<(), ()> {
        if node.op == BinaryOp::Coalesce {
            let left = *node.left;
            let right = *node.right;
            let line = left.get_token().get_position().line;

            // Duplicate coalesce's lval on stack, and compare against Nil. If it's Nil, pop off the
            // duplicated value and eval the rval. If it's not, skip over that logic and do nothing.
            // Top-of-stack will be the lval.
            self.visit(left)?;
            self.write_opcode(Opcode::Dup, line);
            self.write_opcode(Opcode::Nil, line);
            self.write_opcode(Opcode::Eq, line);
            self.write_opcode(Opcode::JumpIfF, line);
            self.write_byte(0, line); // <- Replaced after compiling if-block
            let jump_offset_slot_idx = self.code.len();

            self.write_opcode(Opcode::Pop, line);
            self.visit(right)?;

            let code = &mut self.code;
            let if_block_len = code.len().checked_sub(jump_offset_slot_idx)
                .expect("jump offset slot should be <= end of if-block");
            *code.get_mut(jump_offset_slot_idx - 1).unwrap() = if_block_len as u8;

            return Ok(());
        }

        let node_type = &node.typ;

        let opcode = match (node.op, node_type) {
            (BinaryOp::Add, Type::String) => Opcode::StrConcat,
            (BinaryOp::And, Type::Bool) |
            (BinaryOp::Or, Type::Bool) => unreachable!("&& and || get transformed to if-exprs"),
            (BinaryOp::Lt, Type::Bool) => Opcode::LT,
            (BinaryOp::Lte, Type::Bool) => Opcode::LTE,
            (BinaryOp::Gt, Type::Bool) => Opcode::GT,
            (BinaryOp::Gte, Type::Bool) => Opcode::GTE,
            (BinaryOp::Eq, _) => Opcode::Eq,
            (BinaryOp::Neq, _) => Opcode::Neq,
            (BinaryOp::Coalesce, _) => unreachable!("Coalesce is handled in the if-block above"),

            (BinaryOp::Add, Type::Int) => Opcode::IAdd,
            (BinaryOp::Add, Type::Float) => Opcode::FAdd,
            (BinaryOp::Sub, Type::Int) => Opcode::ISub,
            (BinaryOp::Sub, Type::Float) => Opcode::FSub,
            (BinaryOp::Mul, Type::Int) => Opcode::IMul,
            (BinaryOp::Mul, Type::Float) => Opcode::FMul,
            (BinaryOp::Div, Type::Int) => Opcode::IDiv,
            (BinaryOp::Div, Type::Float) => Opcode::FDiv,
            (BinaryOp::Mod, Type::Int) => Opcode::IMod,
            (BinaryOp::Mod, Type::Float) => Opcode::FMod,
            _ => unreachable!()
        };

        let left = *node.left;
        let right = *node.right;

        let line = left.get_token().get_position().line;
        let ltype = left.get_type();
        self.visit(left)?;
        match (node_type, ltype) {
            (Type::Int, Type::Float) => self.write_opcode(Opcode::F2I, line),
            (Type::Float, Type::Int) => self.write_opcode(Opcode::I2F, line),
            _ => {}
        };

        let line = right.get_token().get_position().line;
        let rtype = right.get_type();
        self.visit(right)?;
        match (node_type, rtype) {
            (Type::Int, Type::Float) => self.write_opcode(Opcode::F2I, line),
            (Type::Float, Type::Int) => self.write_opcode(Opcode::I2F, line),
            _ => {}
        };

        self.write_opcode(opcode, token.get_position().line);
        Ok(())
    }

    fn visit_grouped(&mut self, _token: Token, node: TypedGroupedNode) -> Result<(), ()> {
        let TypedGroupedNode { expr, .. } = node;
        self.visit(*expr)
    }

    fn visit_array(&mut self, token: Token, node: TypedArrayNode) -> Result<(), ()> {
        let num_items = node.items.len();
        for arr_item in node.items {
            self.visit(*arr_item)?;
        }

        let line = token.get_position().line;

        self.write_opcode(Opcode::ArrMk, line);
        self.write_byte(num_items as u8, line);
        Ok(())
    }

    fn visit_map(&mut self, token: Token, node: TypedMapNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let num_items = node.items.len();
        for (key, value) in node.items {
            let key = Token::get_ident_name(&key).clone();
            self.write_constant(Value::Obj(Obj::StringObj { value: Box::new(key) }), line);
            self.visit(value)?;
        }

        self.write_opcode(Opcode::MapMk, line);
        self.write_byte(num_items as u8, line);
        Ok(())
    }

    fn visit_binding_decl(&mut self, token: Token, node: TypedBindingDeclNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedBindingDeclNode { ident, expr, .. } = node;
        let ident = Token::get_ident_name(&ident).clone();

        if self.current_scope().kind == ScopeKind::Root { // If it's a global...
            if let Some(node) = expr {
                self.visit(*node)?;
            } else {
                self.write_opcode(Opcode::Nil, line);
            }
            self.write_constant(Value::Obj(Obj::StringObj { value: Box::new(ident) }), line);
            self.write_opcode(Opcode::GStore, line);
        } else { // ...otherwise, it's a local
            if let Some(node) = expr {
                self.visit(*node)?;
            } else {
                self.write_opcode(Opcode::Nil, line);
            }
            self.push_local(ident);
        }
        Ok(())
    }

    fn visit_nil(&mut self, token: Token) -> Result<(), ()> {
        self.write_opcode(Opcode::Nil, token.get_position().line);
        Ok(())
    }

    fn visit_function_decl(&mut self, token: Token, node: TypedFunctionDeclNode) -> Result<(), ()> {
        let TypedFunctionDeclNode { name, args, body, ret_type, scope_depth, is_recursive } = node;
        let func_name = Token::get_ident_name(&name);

        let line = token.get_position().line;

        if self.current_scope().kind == ScopeKind::Root { // If it's a global...
            self.write_int_constant(0, line);
            self.write_constant(Value::Obj(Obj::StringObj { value: Box::new(func_name.clone()) }), line);
            self.write_opcode(Opcode::GStore, line);
        } else if is_recursive {
            self.write_int_constant(0, line);
            self.push_local(func_name);
        }

        let prev_code = self.code.clone();
        self.code = Vec::new();
        // TODO: std::mem::swap?

        self.push_scope(ScopeKind::Func);

        // Push return slot as local idx 0, if return value exists
        if ret_type != Type::Unit {
            self.push_local("<ret>");
        }

        // Track function arguments in local bindings, also track # optional args.
        // Argument values will already be on the stack.
        for (arg_token, _, default_value) in args.iter() {
            let ident = Token::get_ident_name(arg_token);
            self.push_local(ident);

            // This basically adds, for each default-valued parameter `p`:
            // if (p == nil) { p = defaultValueForP }
            match default_value {
                None => continue,
                Some(default_value_node) => {
                    let pos = default_value_node.get_token().get_position();
                    let typ = default_value_node.get_type();

                    let ident_node = TypedAstNode::Identifier(
                        arg_token.clone(),
                        TypedIdentifierNode { typ: typ.clone(), name: ident.clone(), is_mutable: true, scope_depth },
                    );
                    let nil_expr = TypedAstNode::_Nil(Token::Ident(pos.clone(), "nil".to_string()));
                    let default_param_value_node = TypedAstNode::IfStatement(
                        Token::If(pos.clone()),
                        TypedIfNode {
                            typ: Type::Unit,
                            condition: Box::new(
                                TypedAstNode::Binary(
                                    Token::Eq(pos.clone()),
                                    TypedBinaryNode {
                                        typ: Type::Bool,
                                        right: Box::new(ident_node.clone()),
                                        op: BinaryOp::Eq,
                                        left: Box::new(nil_expr),
                                    },
                                )
                            ),
                            if_block: vec![
                                TypedAstNode::Assignment(
                                    Token::Assign(pos.clone()),
                                    TypedAssignmentNode {
                                        typ: typ.clone(),
                                        target: Box::new(ident_node),
                                        expr: Box::new(default_value_node.clone()),
                                    },
                                )
                            ],
                            else_block: None,
                        },
                    );
                    self.visit(default_param_value_node)?;
                }
            }
        }

        let body_len = body.len();
        let mut last_line = 0;
        for (idx, node) in body.into_iter().enumerate() {
            last_line = node.get_token().get_position().line;
            let is_last_line = idx == body_len - 1;
            let should_pop = should_pop_after_node(&node);
            self.visit(node)?;

            // Handle bare expressions
            if !is_last_line && should_pop {
                self.write_opcode(Opcode::Pop, line);
            }
            if is_last_line {
                let popped_locals = self.pop_scope_locals();

                let should_handle_return = ret_type != Type::Unit;
                if should_handle_return {
                    self.write_store_local_instr(0, line);
                    self.metadata.stores.push("<ret>".to_string());
                }
                self.write_pops_for_closure(popped_locals, line);
            }
        }
        self.write_opcode(Opcode::Return, last_line);
        self.pop_scope();

        let code = self.code.clone();
        self.code = prev_code;
        // TODO: std::mem::swap?

        let fn_upvalues = self.upvalues.iter().rev()
            .take_while(|uv| uv.depth == self.get_fn_depth())
            .map(|uv| uv.clone())
            .collect::<Vec<Upvalue>>();
        self.upvalues.truncate(self.upvalues.len() - fn_upvalues.len());

        let const_idx = self.add_constant(Value::Fn {
            name: func_name.clone(),
            code,
            upvalues: fn_upvalues.clone(),
        });

        self.write_opcode(Opcode::Constant, line);
        self.write_byte(const_idx, line);
        if !fn_upvalues.is_empty() {
            self.write_opcode(Opcode::ClosureMk, line);
        }

        if self.current_scope().kind == ScopeKind::Root { // If it's a global...
            self.write_constant(Value::Obj(Obj::StringObj { value: Box::new(func_name.clone()) }), line);
            self.write_opcode(Opcode::GStore, line);
        } else if is_recursive {
            let scope_depth = self.get_fn_depth();
            let (local, fn_local_idx) = self.resolve_local(func_name, scope_depth)
                .expect("There should have been a function pre-defined with this name");
            local.is_closed = true;
            self.write_store_local_instr(fn_local_idx, line);
            self.metadata.stores.push(func_name.clone());
            self.write_opcode(Opcode::CloseUpvalue, line);
        } else {
            self.push_local(func_name);
        }

        Ok(())
    }

    fn visit_type_decl(&mut self, token: Token, node: TypedTypeDeclNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let TypedTypeDeclNode { name, .. } = node;

        let type_name = Token::get_ident_name(&name);
        let const_idx = self.add_constant(Value::Type(type_name.clone()));
        self.write_opcode(Opcode::Constant, line);
        self.write_byte(const_idx, line);

        if self.current_scope().kind == ScopeKind::Root { // If it's a global...
            self.write_constant(Value::Obj(Obj::StringObj { value: Box::new(type_name.clone()) }), line);
            self.write_opcode(Opcode::GStore, line);
        } else { // ...otherwise, it's a local
            self.push_local(type_name);
        }

        Ok(())
    }

    fn visit_identifier(&mut self, token: Token, node: TypedIdentifierNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let ident = node.name;

        let scope_depth = self.get_fn_depth();
        match self.resolve_local(&ident, scope_depth) {
            Some((_, local_idx)) => { // Load local at index
                self.write_load_local_instr(local_idx, line);
                self.metadata.loads.push(ident.clone());
            }
            None => { // Otherwise, if there's no local...
                let upper_scope_depth = self.get_fn_depth() as i64 - 1;
                match self.resolve_upvalue(&ident, upper_scope_depth) {
                    Some(upvalue_idx) => { // Load upvalue from upper scope
                        self.write_load_upvalue_instr(upvalue_idx, line);
                        self.metadata.uv_loads.push(ident.clone());
                    }
                    None => { // Otherwise, if there's no upvalue...
                        let const_idx = self.get_constant_index(&Value::Obj(Obj::StringObj { value: Box::new(ident.clone()) }));
                        match const_idx {
                            Some(const_idx) => { // Load global by name
                                self.write_opcode(Opcode::Constant, line);
                                self.write_byte(const_idx, line);
                                self.write_opcode(Opcode::GLoad, line);
                            }
                            None => { // Otherwise, if there's no global...
                                // Load the value from the prelude
                                let value = self.prelude.resolve_ident(&ident)
                                    .expect(format!("There was no prelude value for identifier {}", ident).as_str());
                                self.write_constant(value, line);
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn visit_assignment(&mut self, token: Token, node: TypedAssignmentNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedAssignmentNode { target, expr, .. } = node;
        let ident = match *target {
            TypedAstNode::Identifier(ident, _) => Token::get_ident_name(&ident).clone(),
            _ => unreachable!() // We can assume it's an Identifier; typechecking would have failed otherwise
        };

        self.visit(*expr)?;

        let scope_depth = self.get_fn_depth();
        match self.resolve_local(&ident, scope_depth) {
            Some((_, local_idx)) => { // Store to local at index
                self.write_store_local_instr(local_idx, line);
                self.metadata.stores.push(ident.clone());
                self.write_load_local_instr(local_idx, line);
                self.metadata.loads.push(ident.clone());
            }
            None => {
                let upper_scope_depth = self.get_fn_depth() as i64 - 1;
                match self.resolve_upvalue(&ident, upper_scope_depth) {
                    Some(upvalue_idx) => { // Store to upvalue at index
                        self.write_store_upvalue_instr(upvalue_idx, line);
                        self.metadata.uv_stores.push(ident.clone());
                        self.write_load_upvalue_instr(upvalue_idx, line);
                        self.metadata.uv_loads.push(ident.clone());
                    }
                    None => { // Store to global by name
                        let const_idx = self.get_constant_index(&Value::Obj(Obj::StringObj { value: Box::new(ident.clone()) }));
                        let const_idx = const_idx.unwrap();

                        self.write_opcode(Opcode::Constant, line);
                        self.write_byte(const_idx, line);
                        self.write_opcode(Opcode::GStore, line);
                        self.write_opcode(Opcode::Constant, line);
                        self.write_byte(const_idx, line);
                        self.write_opcode(Opcode::GLoad, line);
                    }
                }
            }
        }
        Ok(())
    }

    fn visit_indexing(&mut self, token: Token, node: TypedIndexingNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedIndexingNode { target, index, .. } = node;
        let is_map_target = if let Type::Map(_, _) = &target.get_type() { true } else { false };

        self.visit(*target)?;

        match index {
            IndexingMode::Index(idx) => {
                self.visit(*idx)?;
                if is_map_target {
                    self.write_opcode(Opcode::MapLoad, line);
                } else {
                    self.write_opcode(Opcode::ArrLoad, line);
                }
            }
            IndexingMode::Range(start, end) => {
                if let Some(start) = start {
                    self.visit(*start)?;
                } else {
                    self.write_int_constant(0, line);
                }
                if let Some(end) = end {
                    self.visit(*end)?;
                } else {
                    // Jank: Use the Nil opcode as a placeholder to signify that there is no value,
                    // and that the end of the range will need to be determined at runtime
                    self.write_opcode(Opcode::Nil, line);
                }

                self.write_opcode(Opcode::ArrSlc, line);
            }
        };
        Ok(())
    }

    fn visit_if_statement(&mut self, is_stmt: bool, token: Token, node: TypedIfNode) -> Result<(), ()> {
        #[inline]
        fn compile_block(compiler: &mut Compiler, block: Vec<TypedAstNode>, is_stmt: bool) -> Result<(), ()> {
            compiler.push_scope(ScopeKind::If);

            let block_len = block.len();
            for (idx, node) in block.into_iter().enumerate() {
                let line = node.get_token().get_position().line;
                let is_last_line = idx == block_len - 1;

                let should_pop = should_pop_after_node(&node);
                let is_interrupt = match &node {
                    TypedAstNode::Break(_) => true,
                    _ => false
                };
                compiler.visit(node)?;

                // If we're in a statement and we should pop, then pop
                // If we're in an expression and we should pop AND IT'S NOT THE LAST LINE, then pop
                if (is_stmt && should_pop) || (!is_stmt && !is_last_line && should_pop) {
                    compiler.write_opcode(Opcode::Pop, line);
                }

                // In an interrupt (ie. a break within a loop) the local-popping and jumping bytecode
                // will already have been emitted in `visit_break`; all that needs to happen here is
                // for the compiler to no longer care about the locals in this current scope.
                // We also break out of the loop, since it's unnecessary to compile further than a break.
                if is_interrupt {
                    compiler.pop_scope_locals();
                    break;
                }

                // This is documented in #35
                if is_last_line {
                    let mut num_pops = compiler.pop_scope_locals().len();

                    if !is_stmt {
                        let first_local_idx = compiler.current_scope().first_local_idx;
                        if let Some(idx) = first_local_idx {
                            compiler.write_store_local_instr(idx, line);

                            // Push an empty string into metadata since this isn't a "real" store
                            compiler.metadata.stores.push("".to_string());
                        }
                        if num_pops != 0 {
                            num_pops -= 1;
                        }
                    }
                    compiler.write_pops(num_pops, line);
                }
            }

            compiler.pop_scope();
            Ok(())
        }

        let line = token.get_position().line;

        let TypedIfNode { condition, if_block, else_block, .. } = node;

        self.visit(*condition)?;
        self.write_opcode(Opcode::JumpIfF, line);
        self.write_byte(0, line); // <- Replaced after compiling if-block
        let jump_offset_slot_idx = self.code.len();

        compile_block(self, if_block, is_stmt)?;
        if else_block.is_some() {
            self.write_opcode(Opcode::Jump, line);
            self.write_byte(0, line); // <- Replaced after compiling else-block
        }

        let code = &mut self.code;
        let if_block_len = code.len().checked_sub(jump_offset_slot_idx)
            .expect("jump offset slot should be <= end of if-block");
        *code.get_mut(jump_offset_slot_idx - 1).unwrap() = if_block_len as u8;

        let jump_offset_slot_idx = code.len();

        if let Some(else_block) = else_block {
            compile_block(self, else_block, is_stmt)?;
            let code = &mut self.code;
            let else_block_len = code.len().checked_sub(jump_offset_slot_idx)
                .expect("jump offset slot should be <= end of else-block");
            *code.get_mut(jump_offset_slot_idx - 1).unwrap() = else_block_len as u8;
        }
        Ok(())
    }

    fn visit_if_expression(&mut self, token: Token, node: TypedIfNode) -> Result<(), ()> {
        self.visit_if_statement(false, token, node)
    }

    fn visit_invocation(&mut self, token: Token, node: TypedInvocationNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let TypedInvocationNode { target, args, .. } = node;

        let typ = target.get_type();
        let (arity, has_return) = match typ {
            Type::Fn(args, ret) => (args.len(), *ret != Type::Unit),
            _ => unreachable!() // This should have been caught during typechecking
        };

        if has_return {
            self.write_opcode(Opcode::Nil, line);
        }

        for arg in args {
            match arg {
                None => self.write_opcode(Opcode::Nil, line),
                Some(arg) => self.visit(arg)?
            }
        }

        self.visit(*target)?;

        self.write_opcode(Opcode::Invoke, line);
        self.write_byte(arity as u8, line);
        let incl_ret_slot_op = if has_return { 1 } else { 0 };
        self.write_byte(incl_ret_slot_op, line);
        Ok(())
    }

    fn visit_instantiation(&mut self, token: Token, node: TypedInstantiationNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let TypedInstantiationNode { target, fields, .. } = node;

        self.visit(*target)?;

        let num_fields = fields.len();
        for (_, field_value) in fields.into_iter().rev() {
            self.visit(field_value)?;
        }

        self.write_opcode(Opcode::New, line);
        self.write_byte(num_fields as u8, line);

        // TODO: Emit Init opcode, for initializing methods

        Ok(())
    }

    fn visit_accessor(&mut self, token: Token, node: TypedAccessorNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let TypedAccessorNode { target, field_name, field_idx, .. } = node;

        self.visit(*target)?;

        self.metadata.field_gets.push(field_name);

        self.write_opcode(Opcode::GetField, line);
        self.write_byte(field_idx as u8, line);

        Ok(())
    }

    fn visit_for_loop(&mut self, token: Token, node: TypedForLoopNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedForLoopNode { iteratee, index_ident, iterator, body } = node;

        // Push intrinsic variables $idx and $iter
        self.push_scope(ScopeKind::Loop); // Create wrapper scope to hold invisible variables
        self.write_opcode(Opcode::IConst0, line); // Local 0 is iterator index ($idx)
        self.push_local("$idx");
        self.visit(*iterator)?; // Local 1 is the iterator
        self.push_local("$iter");

        #[inline]
        fn load_intrinsic(compiler: &mut Compiler, name: &str, line: usize) {
            let (_, slot) = compiler.resolve_local(&name.to_string(), compiler.get_fn_depth()).unwrap();
            compiler.write_load_local_instr(slot, line);
            compiler.metadata.loads.push(name.to_string());
        }

        #[inline]
        fn store_intrinsic(compiler: &mut Compiler, name: &str, line: usize) {
            let (_, slot) = compiler.resolve_local(&name.to_string(), compiler.get_fn_depth()).unwrap();
            compiler.write_store_local_instr(slot, line);
            compiler.metadata.stores.push(name.to_string());
        }

        // Essentially: if $idx >= arrayLen($iter) { break }
        let cond_slot_idx = self.code.len();
        load_intrinsic(self, "$idx", line);
        self.write_opcode(Opcode::Nil, line); // <-- Load <ret> slot for ~arrayLen builtin // TODO: Fix this shameful garbage
        load_intrinsic(self, "$iter", line);
        self.write_constant(Value::NativeFn(Self::get_native_fn("arrayLen")), line);
        self.write_opcode(Opcode::Invoke, line);
        self.write_byte(1, line);
        self.write_byte(1, line); // <-- 1 = has_return is true; See comment above about garbage
        self.write_opcode(Opcode::LT, line);
        self.write_opcode(Opcode::JumpIfF, line);
        self.write_byte(0, line); // <- Replaced after compiling loop body
        let cond_jump_offset_slot_idx = self.code.len();

        // Insert iteratee (bound to $iter[$idx]) and index bindings (if indexer expected) into loop scope
        self.push_scope(ScopeKind::Block);
        load_intrinsic(self, "$iter", line);
        load_intrinsic(self, "$idx", line);
        self.write_opcode(Opcode::ArrLoad, line);
        self.push_local(Token::get_ident_name(&iteratee));
        if let Some(ident) = index_ident {
            let (_, slot) = self.resolve_local(&"$idx".to_string(), self.get_fn_depth()).unwrap();
            self.write_load_local_instr(slot, line); // Load $idx
            self.metadata.loads.push("$idx".to_string());
            self.push_local(Token::get_ident_name(&ident));
        }
        load_intrinsic(self, "$idx", line);
        self.write_opcode(Opcode::IConst1, line);
        self.write_opcode(Opcode::IAdd, line);
        store_intrinsic(self, "$idx", line);

        let last_line = self.visit_loop_body(body, cond_slot_idx, cond_jump_offset_slot_idx)?;

        self.pop_scope();
        self.write_opcode(Opcode::Pop, last_line); // Pop $iter
        self.locals.pop(); // Remove $iter from compiler's locals vector
        self.write_opcode(Opcode::Pop, last_line); // Pop $idx
        self.locals.pop(); // Remove $idx from compiler's locals vector

        self.pop_scope();
        Ok(())
    }

    fn visit_while_loop(&mut self, token: Token, node: TypedWhileLoopNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedWhileLoopNode { condition, body } = node;
        let cond_slot_idx = self.code.len();
        self.visit(*condition)?;

        self.write_opcode(Opcode::JumpIfF, line);
        self.write_byte(0, line); // <- Replaced after compiling loop body
        let cond_jump_offset_slot_idx = self.code.len();

        self.push_scope(ScopeKind::Loop);
        self.visit_loop_body(body, cond_slot_idx, cond_jump_offset_slot_idx)?;
        self.pop_scope();

        Ok(())
    }

    fn visit_break(&mut self, token: Token) -> Result<(), ()> {
        let line = token.get_position().line;

        // Emit bytecode to pop locals from stack. The scope in which the break statement lives
        // takes care of making sure the compiler's `locals` vec is in the correct state; here we
        // just need to emit the runtime popping. It's worth noting that locals in scopes deeper than
        // the loop also need to be popped
        let mut num_locals_to_pop = 0;
        for scope in self.scopes.iter().rev() {
            num_locals_to_pop += scope.num_locals;
            if scope.kind == ScopeKind::Loop {
                break;
            }
        }
        self.write_pops(num_locals_to_pop, line);

        self.write_opcode(Opcode::Jump, line);
        self.write_byte(0, line); // <- Replaced after compiling loop body (see visit_while_loop)
        let offset_slot = self.code.len();
        self.interrupt_offset_slots.push(offset_slot);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lexer::tokenize;
    use crate::parser::parser::parse;
    use crate::typechecker::typechecker::typecheck;

    fn compile(input: &str) -> Module {
        let tokens = tokenize(&input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        let (_, typed_ast) = typecheck(ast).unwrap();

        super::compile(typed_ast).unwrap().0
    }

    #[test]
    fn compile_empty() {
        let chunk = compile("");
        let expected = Module {
            code: vec![
                Opcode::Return as u8
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_literals() {
        let chunk = compile("1 2.3 4 5.6 \"hello\" true false");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::Pop as u8,
                Opcode::Constant as u8, 0,
                Opcode::Pop as u8,
                Opcode::IConst4 as u8,
                Opcode::Pop as u8,
                Opcode::Constant as u8, 1,
                Opcode::Pop as u8,
                Opcode::Constant as u8, 2,
                Opcode::Pop as u8,
                Opcode::T as u8,
                Opcode::Pop as u8,
                Opcode::F as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Float(2.3),
                Value::Float(5.6),
                Value::Obj(Obj::StringObj { value: Box::new("hello".to_string()) })
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_unary() {
        let chunk = compile("-5");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Invert as u8,
                Opcode::Return as u8
            ],
            constants: vec![Value::Int(5)],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("-2.3");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Invert as u8,
                Opcode::Return as u8
            ],
            constants: vec![Value::Float(2.3)],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("!false");
        let expected = Module {
            code: vec![
                Opcode::F as u8,
                Opcode::Negate as u8,
                Opcode::Return as u8
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_numeric() {
        let chunk = compile("5 + 6");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::IAdd as u8,
                Opcode::Return as u8
            ],
            constants: vec![Value::Int(5), Value::Int(6)],
        };
        assert_eq!(expected, chunk);

        // Testing i2f and order of ops
        let chunk = compile("1 - -5 * 3.4 / 5");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::I2F as u8,
                Opcode::Constant as u8, 0,
                Opcode::Invert as u8,
                Opcode::I2F as u8,
                Opcode::Constant as u8, 1,
                Opcode::FMul as u8,
                Opcode::Constant as u8, 0,
                Opcode::I2F as u8,
                Opcode::FDiv as u8,
                Opcode::FSub as u8,
                Opcode::Return as u8
            ],
            constants: vec![Value::Int(5), Value::Float(3.4)],
        };
        assert_eq!(expected, chunk);

        // Testing %, along with i2f
        let chunk = compile("3.4 % 2.4 % 5");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::FMod as u8,
                Opcode::Constant as u8, 2,
                Opcode::I2F as u8,
                Opcode::FMod as u8,
                Opcode::Return as u8
            ],
            constants: vec![Value::Float(3.4), Value::Float(2.4), Value::Int(5)],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_grouped() {
        let chunk = compile("(1 + 2) * 3");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::IAdd as u8,
                Opcode::IConst3 as u8,
                Opcode::IMul as u8,
                Opcode::Return as u8
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_str_concat() {
        let chunk = compile("\"abc\" + \"def\"");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::StrConcat as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("abc".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("def".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("1 + \"a\" + 3.4");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, 0,
                Opcode::StrConcat as u8,
                Opcode::Constant as u8, 1,
                Opcode::StrConcat as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Float(3.4)
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_boolean() {
        let chunk = compile("true && true || false");
        let expected = Module {
            code: vec![
                Opcode::T as u8,
                Opcode::JumpIfF as u8, 3,
                Opcode::T as u8,
                Opcode::Jump as u8, 1,
                Opcode::F as u8,
                Opcode::JumpIfF as u8, 3,
                Opcode::T as u8,
                Opcode::Jump as u8, 1,
                Opcode::F as u8,
                Opcode::Return as u8
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_comparisons() {
        let chunk = compile("1 <= 5 == 3.4 >= 5.6");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, 0,
                Opcode::LTE as u8,
                Opcode::Constant as u8, 1,
                Opcode::Constant as u8, 2,
                Opcode::GTE as u8,
                Opcode::Eq as u8,
                Opcode::Return as u8
            ],
            constants: vec![Value::Int(5), Value::Float(3.4), Value::Float(5.6)],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"a\" < \"b\" != 4");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::LT as u8,
                Opcode::IConst4 as u8,
                Opcode::Neq as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) })
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_coalesce() {
        let chunk = compile("[\"a\", \"b\"][2] ?: \"c\"");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::ArrMk as u8, 2,
                Opcode::IConst2 as u8,
                Opcode::ArrLoad as u8,
                Opcode::Dup as u8,
                Opcode::Nil as u8,
                Opcode::Eq as u8,
                Opcode::JumpIfF as u8, 3,
                Opcode::Pop as u8,
                Opcode::Constant as u8, 2,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("c".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_array_primitives() {
        let chunk = compile("[1, 2]");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::ArrMk as u8, 2,
                Opcode::Return as u8
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("[\"a\", \"b\", \"c\"]");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::Constant as u8, 2,
                Opcode::ArrMk as u8, 3,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("c".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_array_nested() {
        let chunk = compile("[[1, 2], [3, 4, 5]]");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::ArrMk as u8, 2,
                Opcode::IConst3 as u8,
                Opcode::IConst4 as u8,
                Opcode::Constant as u8, 0,
                Opcode::ArrMk as u8, 3,
                Opcode::ArrMk as u8, 2,
                Opcode::Return as u8
            ],
            constants: vec![Value::Int(5)],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_map_literal() {
        let chunk = compile("{ a: 1, b: \"c\", d: true }");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, 1,
                Opcode::Constant as u8, 2,
                Opcode::Constant as u8, 3,
                Opcode::T as u8,
                Opcode::MapMk as u8, 3,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("c".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("d".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binding_decl() {
        let chunk = compile("val abc = 123");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: vec![Value::Int(123), Value::Obj(Obj::StringObj { value: Box::new("abc".to_string()) })],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("var unset: Bool\nvar set = true");
        let expected = Module {
            code: vec![
                Opcode::Nil as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::T as u8,
                Opcode::Constant as u8, 1,
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("unset".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("set".to_string()) })
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("val abc = \"a\" + \"b\"\nval def = 5");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::StrConcat as u8,
                Opcode::Constant as u8, 2,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 3,
                Opcode::Constant as u8, 4,
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("abc".to_string()) }),
                Value::Int(5),
                Value::Obj(Obj::StringObj { value: Box::new("def".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binding_decl_struct_type() {
        let chunk = compile("\
          type Person { name: String }\n\
          val meg = Person(name: \"Meg\")\
        ");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 1,
                Opcode::GLoad as u8,
                Opcode::Constant as u8, 2,
                Opcode::New as u8, 1,
                Opcode::Constant as u8, 3,
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Type("Person".to_string()),
                Value::Obj(Obj::StringObj { value: Box::new("Person".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("Meg".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("meg".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);

        // Test assignment with default field values
        let chunk = compile("\
          type Person { name: String, age: Int = 0 }\n\
          val someBaby = Person(name: \"Unnamed\")\n\
          val anAdult = Person(name: \"Some Name\", age: 29)\n\
        ");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 1,
                Opcode::GLoad as u8,
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, 2,
                Opcode::New as u8, 2,
                Opcode::Constant as u8, 3,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 1,
                Opcode::GLoad as u8,
                Opcode::Constant as u8, 4,
                Opcode::Constant as u8, 5,
                Opcode::New as u8, 2,
                Opcode::Constant as u8, 6,
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Type("Person".to_string()),
                Value::Obj(Obj::StringObj { value: Box::new("Person".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("Unnamed".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("someBaby".to_string()) }),
                Value::Int(29),
                Value::Obj(Obj::StringObj { value: Box::new("Some Name".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("anAdult".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_ident() {
        let chunk = compile("val abc = 123\nabc");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 1,
                Opcode::GLoad as u8,
                Opcode::Return as u8
            ],
            constants: vec![Value::Int(123), Value::Obj(Obj::StringObj { value: Box::new("abc".to_string()) })],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_ident_upvalues() {
        let chunk = compile("func a(i: Int) {\nval b = 3\nfunc c() { b + 1 }\n}");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 2,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Return as u8,
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Fn {
                    name: "c".to_string(),
                    code: vec![
                        Opcode::ULoad0 as u8,
                        Opcode::IConst1 as u8,
                        Opcode::IAdd as u8,
                        Opcode::LStore0 as u8,
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![
                        Upvalue {
                            capture_kind: UpvalueCaptureKind::Local { local_idx: 1 },
                            depth: 1,
                        }
                    ],
                },
                Value::Fn {
                    name: "a".to_string(),
                    code: vec![
                        Opcode::IConst3 as u8,
                        Opcode::Constant as u8, 1,
                        Opcode::ClosureMk as u8,
                        Opcode::Pop as u8,
                        Opcode::CloseUpvalueAndPop as u8,
                        Opcode::Pop as u8,
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![],
                },
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_ident_upvalues_skip_level() {
        let chunk = compile("func a(i: Int) {\nval b = 3\nfunc c() { func d() { b + 1 }\n}\n}");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 3,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Return as u8,
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Fn {
                    name: "d".to_string(),
                    code: vec![
                        Opcode::ULoad0 as u8,
                        Opcode::IConst1 as u8,
                        Opcode::IAdd as u8,
                        Opcode::LStore0 as u8,
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![
                        Upvalue {
                            capture_kind: UpvalueCaptureKind::Upvalue { upvalue_idx: 0 },
                            depth: 2,
                        }
                    ],
                },
                Value::Fn {
                    name: "c".to_string(),
                    code: vec![
                        Opcode::Constant as u8, 1,
                        Opcode::ClosureMk as u8,
                        Opcode::Pop as u8,
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![
                        Upvalue {
                            capture_kind: UpvalueCaptureKind::Local { local_idx: 1 },
                            depth: 1,
                        }
                    ],
                },
                Value::Fn {
                    name: "a".to_string(),
                    code: vec![
                        Opcode::IConst3 as u8,
                        Opcode::Constant as u8, 2,
                        Opcode::ClosureMk as u8,
                        Opcode::Pop as u8,
                        Opcode::CloseUpvalueAndPop as u8,
                        Opcode::Pop as u8,
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![],
                },
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_assignment() {
        let chunk = compile("var a = 1\nvar b = 2\nval c = b = a = 3");
        let expected = Module {
            code: vec![
                // var a = 1
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                // var b = 2
                Opcode::IConst2 as u8,
                Opcode::Constant as u8, 1,
                Opcode::GStore as u8,

                // val c = b = a = 3
                //   a = 3
                Opcode::IConst3 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 0,
                Opcode::GLoad as u8,
                //  b = <a = 3>
                Opcode::Constant as u8, 1,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 1,
                Opcode::GLoad as u8,
                //  c = <b = <a = 3>>
                Opcode::Constant as u8, 2,
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("c".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("var a = 1\na = 2\nval b = 3");
        let expected = Module {
            code: vec![
                // var a = 1
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                // a = 2
                Opcode::IConst2 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 0,
                Opcode::GLoad as u8,
                Opcode::Pop as u8, // <- This test verifies that the intermediate 2 gets popped
                // val b = 3
                Opcode::IConst3 as u8,
                Opcode::Constant as u8, 1,
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_assignment_globals() {
        let chunk = compile("var a = 1\nfunc abc() { a = 3 }");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, 1,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 2,
                Opcode::Constant as u8, 1,
                Opcode::GStore as u8,
                Opcode::Return as u8,
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("abc".to_string()) }),
                Value::Fn {
                    name: "abc".to_string(),
                    code: vec![
                        Opcode::IConst3 as u8,
                        Opcode::Constant as u8, 0,
                        Opcode::GStore as u8,
                        Opcode::Constant as u8, 0,
                        Opcode::GLoad as u8,
                        Opcode::LStore0 as u8,
                        Opcode::Return as u8
                    ],
                    upvalues: vec![],
                },
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_assignment_upvalues() {
        let chunk = compile("func outer() {\nvar a = 1\nfunc inner() { a = 3 }\n}");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 2,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Return as u8,
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("outer".to_string()) }),
                Value::Fn {
                    name: "inner".to_string(),
                    code: vec![
                        Opcode::IConst3 as u8,
                        Opcode::UStore0 as u8,
                        Opcode::ULoad0 as u8,
                        Opcode::LStore0 as u8,
                        Opcode::Return as u8
                    ],
                    upvalues: vec![
                        Upvalue {
                            capture_kind: UpvalueCaptureKind::Local { local_idx: 0 },
                            depth: 1,
                        }
                    ],
                },
                Value::Fn {
                    name: "outer".to_string(),
                    code: vec![
                        Opcode::IConst1 as u8,
                        Opcode::Constant as u8, 1,
                        Opcode::ClosureMk as u8,
                        Opcode::Pop as u8,
                        Opcode::CloseUpvalueAndPop as u8,
                        Opcode::Return as u8
                    ],
                    upvalues: vec![],
                },
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_indexing() {
        let chunk = compile("[1, 2, 3, 4, 5][3 + 1]");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::IConst3 as u8,
                Opcode::IConst4 as u8,
                Opcode::Constant as u8, 0,
                Opcode::ArrMk as u8, 5,
                Opcode::IConst3 as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::ArrLoad as u8,
                Opcode::Return as u8
            ],
            constants: vec![Value::Int(5)],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"some string\"[1 + 1:]");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::IConst1 as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::Nil as u8,
                Opcode::ArrSlc as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("some string".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"some string\"[-1:4]");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::IConst1 as u8,
                Opcode::Invert as u8,
                Opcode::IConst4 as u8,
                Opcode::ArrSlc as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("some string".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"some string\"[:1 + 1]");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::IConst0 as u8,
                Opcode::IConst1 as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::ArrSlc as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("some string".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("{ a: 1, b: 2 }[\"a\"]");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, 1,
                Opcode::IConst2 as u8,
                Opcode::MapMk as u8, 2,
                Opcode::Constant as u8, 0,
                Opcode::MapLoad as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_if_else_statements() {
        let chunk = compile("if (1 == 2) 123 else 456");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::Eq as u8,
                Opcode::JumpIfF as u8, 5,
                Opcode::Constant as u8, 0,
                Opcode::Pop as u8,
                Opcode::Jump as u8, 3,
                Opcode::Constant as u8, 1,
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: vec![Value::Int(123), Value::Int(456)],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("if (1 == 2) 123");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::Eq as u8,
                Opcode::JumpIfF as u8, 3,
                Opcode::Constant as u8, 0,
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: vec![Value::Int(123)],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("if (1 == 2) { } else { 456 }");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::Eq as u8,
                Opcode::JumpIfF as u8, 2,
                Opcode::Jump as u8, 3,
                Opcode::Constant as u8, 0,
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: vec![Value::Int(456)],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("if (1 == 2) 123 else if (3 < 4) 456 else 789");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::Eq as u8,
                Opcode::JumpIfF as u8, 5,
                Opcode::Constant as u8, 0,
                Opcode::Pop as u8,
                Opcode::Jump as u8, 13,
                Opcode::IConst3 as u8,
                Opcode::IConst4 as u8,
                Opcode::LT as u8,
                Opcode::JumpIfF as u8, 5,
                Opcode::Constant as u8, 1,
                Opcode::Pop as u8,
                Opcode::Jump as u8, 3,
                Opcode::Constant as u8, 2,
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: vec![Value::Int(123), Value::Int(456), Value::Int(789)],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\
          val a = 123
          if (true) {\
            val a = 456\
            a + 1\
          }\
        ");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::GStore as u8,
                Opcode::T as u8,
                Opcode::JumpIfF as u8, 7,
                Opcode::Constant as u8, 2,
                Opcode::LLoad0 as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::Pop as u8,
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Int(123),
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Int(456),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_function_declaration() {
        let chunk = compile("\
          val a = 1\n\
          val b = 2\n\
          val c = 3\n\
          func abc(b: Int) {\n\
            val a1 = a\n\
            val c = b + a1\n\
            c\n\
          }\
        ");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::IConst2 as u8,
                Opcode::Constant as u8, 1,
                Opcode::GStore as u8,
                Opcode::IConst3 as u8,
                Opcode::Constant as u8, 2,
                Opcode::GStore as u8,
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, 3,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 4,
                Opcode::Constant as u8, 3,
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("a".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("b".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("c".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("abc".to_string()) }),
                Value::Fn {
                    name: "abc".to_string(),
                    code: vec![
                        Opcode::Constant as u8, 0,
                        Opcode::GLoad as u8,
                        Opcode::LLoad1 as u8,
                        Opcode::LLoad2 as u8,
                        Opcode::IAdd as u8,
                        Opcode::LLoad3 as u8,
                        Opcode::LStore0 as u8,
                        Opcode::Pop as u8,
                        Opcode::Pop as u8,
                        Opcode::Pop as u8,
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![],
                }
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_function_declaration_returns_unit_type() {
        let chunk = compile("\
          func abc() {\n\
            val a = 1\n\
            println(\"hello\")\n\
          }\
        ");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 3,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("abc".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("hello".to_string()) }),
                Value::NativeFn(Compiler::get_native_fn("println")),
                Value::Fn {
                    name: "abc".to_string(),
                    code: vec![
                        Opcode::IConst1 as u8,
                        Opcode::Constant as u8, 1,
                        Opcode::Constant as u8, 2,
                        Opcode::Invoke as u8, 1, 0,
                        Opcode::Pop as u8, // Pop off `a`; note, there is no LStore0, since the return is Unit
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![],
                },
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_function_declaration_default_args() {
        let chunk = compile("func add(a: Int, b = 2) = a + b\nadd(1)\nadd(1, 2)");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 1,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Nil as u8,
                Opcode::IConst1 as u8,
                Opcode::Nil as u8,
                Opcode::Constant as u8, 0,
                Opcode::GLoad as u8,
                Opcode::Invoke as u8, 2, 1,
                Opcode::Pop as u8,
                Opcode::Nil as u8,
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GLoad as u8,
                Opcode::Invoke as u8, 2, 1,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("add".to_string()) }),
                Value::Fn {
                    name: "add".to_string(),
                    code: vec![
                        Opcode::Nil as u8,
                        Opcode::LLoad2 as u8,
                        Opcode::Eq as u8,
                        Opcode::JumpIfF as u8, 4,
                        Opcode::IConst2 as u8,
                        Opcode::LStore2 as u8,
                        Opcode::LLoad2 as u8,
                        Opcode::Pop as u8,
                        Opcode::LLoad1 as u8,
                        Opcode::LLoad2 as u8,
                        Opcode::IAdd as u8,
                        Opcode::LStore0 as u8,
                        Opcode::Pop as u8,
                        Opcode::Pop as u8,
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![],
                },
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_function_declaration_with_inner() {
        let chunk = compile("\
          func abc(b: Int) {\n\
            func def(g: Int) { g + 1 }
            val c = b + def(b)\n\
            c\n\
          }\
        ");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 2,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("abc".to_string()) }),
                Value::Fn {
                    name: "def".to_string(),
                    code: vec![
                        Opcode::LLoad1 as u8,
                        Opcode::IConst1 as u8,
                        Opcode::IAdd as u8,
                        Opcode::LStore0 as u8,
                        Opcode::Pop as u8,
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![],
                },
                Value::Fn {
                    name: "abc".to_string(),
                    code: vec![
                        Opcode::Constant as u8, 1,
                        Opcode::LLoad1 as u8,
                        Opcode::Nil as u8,
                        Opcode::LLoad1 as u8,
                        Opcode::LLoad2 as u8,
                        Opcode::Invoke as u8, 1, 1,
                        Opcode::IAdd as u8,
                        Opcode::LLoad3 as u8,
                        Opcode::LStore0 as u8,
                        Opcode::Pop as u8,
                        Opcode::Pop as u8,
                        Opcode::Pop as u8,
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![],
                }
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_function_invocation() {
        let chunk = compile("\
          val one = 1\n\
          func inc(number: Int) {\n\
            number + 1\n\
          }\n
          val two = inc(number: one)\
        ");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, 1,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 2,
                Opcode::Constant as u8, 1,
                Opcode::GStore as u8,
                Opcode::Nil as u8,
                Opcode::Constant as u8, 0,
                Opcode::GLoad as u8,
                Opcode::Constant as u8, 1,
                Opcode::GLoad as u8,
                Opcode::Invoke as u8, 1, 1,
                Opcode::Constant as u8, 3,
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("one".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("inc".to_string()) }),
                Value::Fn {
                    name: "inc".to_string(),
                    code: vec![
                        Opcode::LLoad1 as u8,
                        Opcode::IConst1 as u8,
                        Opcode::IAdd as u8,
                        Opcode::LStore0 as u8,
                        Opcode::Pop as u8,
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![],
                },
                Value::Obj(Obj::StringObj { value: Box::new("two".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_while_loop() {
        let chunk = compile("\
          var i = 0\n\
          while i < 1 {\n\
            i = i + 1\n\
          }\
        ");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 0,
                Opcode::GLoad as u8,
                Opcode::IConst1 as u8,
                Opcode::LT as u8,
                Opcode::JumpIfF as u8, 14,
                Opcode::Constant as u8, 0,
                Opcode::GLoad as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 0,
                Opcode::GLoad as u8,
                Opcode::Pop as u8,
                Opcode::JumpB as u8, 21,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("i".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_while_loop_with_local() {
        let chunk = compile("\
          var i = 0\n\
          while i < 1 {\n\
            val newI = i + 1\n\
            i = newI\n\
          }\
        ");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 0,
                Opcode::GLoad as u8,
                Opcode::IConst1 as u8,
                Opcode::LT as u8,
                Opcode::JumpIfF as u8, 16,
                Opcode::Constant as u8, 0,
                Opcode::GLoad as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::LLoad0 as u8,
                Opcode::Constant as u8, 0,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 0,
                Opcode::GLoad as u8,
                Opcode::Pop as u8,
                Opcode::Pop as u8,
                Opcode::JumpB as u8, 23,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("i".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_while_loop_with_break() {
        let chunk = compile("\
          while true {\n\
            val i = 1\n\
            break\n\
          }\
        ");
        let expected = Module {
            code: vec![
                Opcode::T as u8,
                Opcode::JumpIfF as u8, 6,
                Opcode::IConst1 as u8,
                Opcode::Pop as u8,      // <
                Opcode::Jump as u8, 2,  // < These 3 instrs are generated by the break
                Opcode::JumpB as u8, 9, // These 2 get falsely attributed to the break, because of #32
                Opcode::Return as u8
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\
          while true {\n\
            val i = 1\n\
            if i == 1 {\n\
              val a = 2\n\
              break\n\
            }\n\
          }\
        ");
        let expected = Module {
            code: vec![
                Opcode::T as u8,
                Opcode::JumpIfF as u8, 14,
                Opcode::IConst1 as u8,
                Opcode::LLoad0 as u8,
                Opcode::IConst1 as u8,
                Opcode::Eq as u8,
                Opcode::JumpIfF as u8, 5,
                Opcode::IConst2 as u8,
                Opcode::PopN as u8, 2,  // <
                Opcode::Jump as u8, 3,  // < These 3 instrs are generated by the break
                Opcode::Pop as u8,      // This instr is where the if jumps to if false (we still need to clean up locals in the loop)
                Opcode::JumpB as u8, 17,
                Opcode::Return as u8
            ],
            constants: vec![],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_for_loop() {
        let chunk = compile("\
          val msg = \"Row: \"\n\
          val arr = [1, 2]\n\
          for a in arr {\n\
            println(msg + a)\n\
          }\
        ");
        let expected = Module {
            code: vec![
                // val msg = "Row: "
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::GStore as u8,

                // val arr = [1, 2]
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::ArrMk as u8, 2,
                Opcode::Constant as u8, 2,
                Opcode::GStore as u8,

                // val $idx = 0
                // val $iter = arr
                // if $idx < arrayLen($iter) {
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, 2,
                Opcode::GLoad as u8,
                Opcode::LLoad0 as u8,
                Opcode::Nil as u8,
                Opcode::LLoad1 as u8,
                Opcode::Constant as u8, 3,
                Opcode::Invoke as u8, 1, 1,
                Opcode::LT as u8,
                Opcode::JumpIfF as u8, 20,

                // a = $iter[$idx]
                Opcode::LLoad1 as u8,
                Opcode::LLoad0 as u8,
                Opcode::ArrLoad as u8,

                // $idx += 1
                Opcode::LLoad0 as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::LStore0 as u8,

                // println(msg + a)
                // <recur>
                Opcode::Constant as u8, 1,
                Opcode::GLoad as u8,
                Opcode::LLoad2 as u8,
                Opcode::StrConcat as u8,
                Opcode::Constant as u8, 4,
                Opcode::Invoke as u8, 1, 0,
                Opcode::Pop as u8,
                Opcode::JumpB as u8, 31,

                // Cleanup/end
                Opcode::Pop as u8,
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("Row: ".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("msg".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("arr".to_string()) }),
                Value::NativeFn(Compiler::get_native_fn("arrayLen")),
                Value::NativeFn(Compiler::get_native_fn("println")),
            ],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_accessor() {
        // Accessing fields of structs
        let chunk = compile("\
          type Person { name: String }\n\
          val ken = Person(name: \"Ken\")\n\
          ken.name\n\
        ");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::Constant as u8, 1,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 1,
                Opcode::GLoad as u8,
                Opcode::Constant as u8, 2,
                Opcode::New as u8, 1,
                Opcode::Constant as u8, 3,
                Opcode::GStore as u8,
                Opcode::Constant as u8, 3,
                Opcode::GLoad as u8,
                Opcode::GetField as u8, 0,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Type("Person".to_string()),
                Value::Obj(Obj::StringObj { value: Box::new("Person".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("Ken".to_string()) }),
                Value::Obj(Obj::StringObj { value: Box::new("ken".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);

        // Accessing fields of structs
        let chunk = compile("\"hello\".length");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, 0,
                Opcode::GetField as u8, 0,
                Opcode::Return as u8
            ],
            constants: vec![
                Value::Obj(Obj::StringObj { value: Box::new("hello".to_string()) }),
            ],
        };
        assert_eq!(expected, chunk);
    }
}
