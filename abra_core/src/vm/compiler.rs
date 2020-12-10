use crate::common::typed_ast_visitor::TypedAstVisitor;
use crate::lexer::tokens::Token;
use crate::parser::ast::{UnaryOp, BinaryOp, IndexingMode, TypeIdentifier};
use crate::vm::opcode::Opcode;
use crate::typechecker::typed_ast::{TypedAstNode, TypedLiteralNode, TypedUnaryNode, TypedBinaryNode, TypedArrayNode, TypedBindingDeclNode, TypedAssignmentNode, TypedIndexingNode, TypedGroupedNode, TypedIfNode, TypedFunctionDeclNode, TypedIdentifierNode, TypedInvocationNode, TypedWhileLoopNode, TypedForLoopNode, TypedTypeDeclNode, TypedMapNode, TypedAccessorNode, TypedInstantiationNode, AssignmentTargetKind, TypedLambdaNode, TypedEnumDeclNode, EnumVariantKind, TypedMatchNode, TypedTupleNode, TypedSetNode};
use crate::typechecker::types::{Type, FnType, EnumVariantType};
use crate::vm::value::{Value, FnValue, TypeValue, EnumValue, EnumVariantObj};
use crate::vm::prelude::{PRELUDE_BINDINGS, PRELUDE_BINDING_VALUES};
use crate::builtins::native_types::{NativeArray, NativeType, NativeSet, NativeMap};
use crate::common::util::random_string;
use crate::common::compiler_util::get_anon_name;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct Local {
    name: String,
    depth: usize,
    is_captured: bool,
    is_closed: bool,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, PartialOrd)]
pub enum UpvalueCaptureKind {
    Local { local_idx: usize },
    Upvalue { upvalue_idx: usize },
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, PartialOrd)]
pub struct Upvalue {
    pub capture_kind: UpvalueCaptureKind,
    depth: usize,
}

#[derive(Clone, Debug, PartialEq)]
enum ScopeKind { Root, If, Func, Loop, Block }

#[derive(Clone, Debug, PartialEq)]
struct Scope {
    kind: ScopeKind,
    num_locals: usize,
    first_local_idx: Option<usize>,
}

pub struct Compiler {
    code: Vec<u8>,
    constants: Vec<Value>,
    constant_indexes: HashMap<String, usize>,
    scopes: Vec<Scope>,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
    interrupt_offset_slots: Vec<usize>,
    metadata: Metadata,
}

#[derive(Debug, Default, PartialEq)]
pub struct Metadata {
    pub loads: Vec<String>,
    pub stores: Vec<String>,
    pub uv_loads: Vec<String>,
    pub uv_stores: Vec<String>,
    pub field_gets: Vec<String>,
    pub local_marks: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub constants: Vec<Value>,
    pub code: Vec<u8>,
}

pub fn compile(ast: Vec<TypedAstNode>) -> Result<(Module, Metadata), ()> {
    let metadata = Metadata::default();
    let root_scope = Scope { kind: ScopeKind::Root, num_locals: 0, first_local_idx: None };

    let constants = PRELUDE_BINDING_VALUES.with(|values| values.clone());
    let constant_indexes = PRELUDE_BINDINGS.with(|bindings|
        bindings.iter().enumerate()
            .map(|(idx, (name, _))| (name.clone(), idx))
            .collect()
    );

    let mut compiler = Compiler {
        code: Vec::new(),
        constants,
        constant_indexes,
        scopes: vec![root_scope],
        locals: Vec::new(),
        upvalues: Vec::new(),
        interrupt_offset_slots: Vec::new(),
        metadata,
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
        TypedAstNode::EnumDecl(_, _) |
        TypedAstNode::IfStatement(_, _) |
        TypedAstNode::MatchStatement(_, _) |
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
        self.get_constant_index(&value).unwrap_or_else(|| {
            self.constants.push(value);
            (self.constants.len() - 1) as u8
        })
    }

    fn get_constant_index(&self, value: &Value) -> Option<u8> {
        self.constants.iter()
            .position(|v| v == value)
            .map(|v| v as u8)
    }

    fn get_type_constant_index(&mut self, type_name: &String) -> u8 {
        let idx = if let Some(idx) = self.constant_indexes.get(type_name) {
            idx.clone()
        } else {
            let pos = self.constants.iter().position(|v| {
                if let Value::Type(TypeValue { name, .. }) = v {
                    name == type_name
                } else { false }
            });
            let pos = pos.expect(format!("There should be a Type constant with name {}", type_name).as_str());
            self.constant_indexes.insert(type_name.clone(), pos);
            pos
        };
        idx as u8
    }

    fn write_constant(&mut self, value: Value, line: usize) -> u8 {
        let const_idx = self.add_constant(value);
        self.write_opcode(Opcode::Constant, line);
        self.write_byte(const_idx, line);
        const_idx
    }

    fn write_pops(&mut self, popped_locals: &Vec<Local>, line: usize) {
        let ops = popped_locals.iter()
            .map(|local| if local.is_captured && !local.is_closed { Opcode::CloseUpvalueAndPop } else { Opcode::Pop })
            .collect::<Vec<_>>();

        let mut num_conseq_pops = 0;
        for op in ops {
            if op == Opcode::Pop {
                num_conseq_pops += 1;
            } else { // op = CloseUpvalueAndPop
                if num_conseq_pops == 1 {
                    self.write_opcode(Opcode::Pop, line);
                } else if num_conseq_pops != 0 {
                    self.write_opcode(Opcode::PopN, line);
                    self.write_byte(num_conseq_pops, line);
                }
                num_conseq_pops = 0;

                self.write_opcode(Opcode::CloseUpvalueAndPop, line);
            }
        }
        if num_conseq_pops == 1 {
            self.write_opcode(Opcode::Pop, line);
        } else if num_conseq_pops != 0 {
            self.write_opcode(Opcode::PopN, line);
            self.write_byte(num_conseq_pops, line);
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

    fn resolve_identifier<S: AsRef<str>>(&mut self, ident: S, line: usize) {
        let scope_depth = self.get_fn_depth();
        let ident_str = ident.as_ref().to_string();
        match self.resolve_local(&ident, scope_depth) {
            Some((_, local_idx)) => { // Load local at index
                self.write_load_local_instr(local_idx, line);
                self.metadata.loads.push(ident_str);
            }
            None => { // Otherwise, if there's no local...
                let upper_scope_depth = self.get_fn_depth() as i64 - 1;
                match self.resolve_upvalue(&ident, upper_scope_depth) {
                    Some(upvalue_idx) => { // Load upvalue from upper scope
                        self.write_load_upvalue_instr(upvalue_idx, line);
                        self.metadata.uv_loads.push(ident_str);
                    }
                    None => { // Otherwise, if there's no upvalue...
                        let const_idx = self.get_constant_index(&Value::Str(ident_str.clone()));
                        match const_idx {
                            Some(const_idx) => { // Load global by name
                                self.write_opcode(Opcode::Constant, line);
                                self.write_byte(const_idx, line);
                                self.write_opcode(Opcode::GLoad, line);
                            }
                            None => { // Otherwise, if there's no global...
                                // Load the value from the pre-loaded constant_indexes (from prelude)
                                let const_idx = self.constant_indexes.get(&ident_str)
                                    .expect("All prelude constants should be eagerly loaded ahead of time")
                                    .clone();
                                self.write_opcode(Opcode::Constant, line);
                                self.write_byte(const_idx as u8, line);
                            }
                        }
                    }
                }
            }
        }
    }

    fn get_fn_depth(&self) -> usize {
        self.scopes.iter().filter(|s| s.kind == ScopeKind::Func).count()
    }

    fn push_local<S: AsRef<str>>(&mut self, name: S, line: usize, mark_local: bool) {
        let depth = self.get_fn_depth();
        self.locals.push(Local {
            name: name.as_ref().to_string(),
            depth,
            is_captured: false,
            is_closed: false,
        });

        let mut scope = self.scopes.last_mut().unwrap();
        scope.num_locals += 1;
        if scope.first_local_idx == None {
            let first_local_idx = self.locals.iter().filter(|l| l.depth == depth).count();
            scope.first_local_idx = Some(first_local_idx - 1);
        }

        if mark_local {
            let (_, local_idx) = self.resolve_local(name.as_ref(), depth).unwrap();
            self.metadata.local_marks.push(name.as_ref().to_string());
            self.write_opcode(Opcode::MarkLocal, line);
            self.write_byte(local_idx as u8, line);
        }
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
                let pops = self.pop_scope_locals();
                self.write_pops(&pops, line);
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

    fn compile_function_decl(
        &mut self,
        token: Token,
        name: Option<Token>,
        args: Vec<(Token, Type, Option<TypedAstNode>)>,
        ret_type: Type,
        body: Vec<TypedAstNode>,
        scope_depth: usize,
    ) -> Result<FnValue, ()> {
        let func_name = match name {
            Some(name) => Token::get_ident_name(&name),
            None => get_anon_name()
        };

        let line = token.get_position().line;

        let prev_code = self.code.clone();
        self.code = Vec::new();
        // TODO: std::mem::swap?

        self.push_scope(ScopeKind::Func);

        let has_return = ret_type != Type::Unit;

        // Push return slot as local idx 0, if return value exists
        if has_return {
            // We do NOT want to mark function parameters (or return values) as locals, since they're
            // pushed onto the stack before the function's call frame starts, so, the entry
            // in the frame's local_addrs would be incorrect. See the handling of Opcode::Invoke in the VM
            self.push_local("<ret>", line, false);
        }

        // Track function arguments in local bindings, also track # optional args.
        // Argument values will already be on the stack.
        for (arg_token, _, default_value) in args.iter() {
            let ident = Token::get_ident_name(arg_token);

            // See comment above about not marking locals
            self.push_local(&ident, line, false);

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
                            condition_binding: None,
                            if_block: vec![
                                TypedAstNode::Assignment(
                                    Token::Assign(pos.clone()),
                                    TypedAssignmentNode {
                                        kind: AssignmentTargetKind::ArrayIndex,
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

                if has_return {
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

        Ok(FnValue { name: func_name.clone(), code, upvalues: fn_upvalues.clone(), receiver: None, has_return })
    }

    #[inline]
    fn visit_block(&mut self, block: Vec<TypedAstNode>, is_stmt: bool) -> Result<(), ()> {
        let block_len = block.len();
        for (idx, node) in block.into_iter().enumerate() {
            let line = node.get_token().get_position().line;
            let is_last_line = idx == block_len - 1;

            let should_pop = should_pop_after_node(&node);
            let is_interrupt = match &node {
                TypedAstNode::Break(_) => true,
                _ => false
            };
            self.visit(node)?;

            // If we're in a statement and we should pop, then pop
            // If we're in an expression and we should pop AND IT'S NOT THE LAST LINE, then pop
            if (is_stmt && should_pop) || (!is_stmt && !is_last_line && should_pop) {
                self.write_opcode(Opcode::Pop, line);
            }

            // In an interrupt (ie. a break within a loop) the local-popping and jumping bytecode
            // will already have been emitted in `visit_break`; all that needs to happen here is
            // for the compiler to no longer care about the locals in this current scope.
            // We also break out of the loop, since it's unnecessary to compile further than a break.
            if is_interrupt {
                self.pop_scope_locals();
                break;
            }

            // This is documented in #35
            if is_last_line {
                let mut pops = self.pop_scope_locals();

                if !is_stmt {
                    let first_local_idx = self.current_scope().first_local_idx;
                    if let Some(idx) = first_local_idx {
                        self.write_store_local_instr(idx, line);

                        // Push an empty string into metadata since this isn't a "real" store
                        self.metadata.stores.push("".to_string());
                    }
                    if pops.len() != 0 {
                        pops.remove(0);
                    }
                }
                self.write_pops(&pops, line);
            }
        }
        Ok(())
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

        let value = match node {
            TypedLiteralNode::FloatLiteral(val) => Value::Float(val),
            TypedLiteralNode::StringLiteral(val) => Value::new_string_obj(val),
            TypedLiteralNode::IntLiteral(_) | TypedLiteralNode::BoolLiteral(_) => unreachable!() // Handled in if-let above
        };
        let const_idx = self.add_constant(value);

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

            // When compiling a coalesce, expand it out to an if-expr with condition binding:
            //   a ?: b  =>  if a |$left_rand| $left_rand else b
            // This means that the compilation of coalescence and if-exprs generates similar bytecode
            let name = format!("$left_{}", random_string(3));
            let if_expr = TypedAstNode::IfExpression(
                Token::If(token.get_position()),
                TypedIfNode {
                    typ: Type::Placeholder, // Type doesn't matter
                    condition: Box::new(left),
                    condition_binding: Some(Token::Ident(token.get_position(), name.clone())),
                    if_block: vec![
                        TypedAstNode::Identifier(
                            Token::Ident(token.get_position(), name.clone()),
                            TypedIdentifierNode {
                                typ: Type::Placeholder, // Type doesn't matter
                                name,
                                is_mutable: false,
                                scope_depth: 0, // Doesn't matter
                            },
                        )
                    ],
                    else_block: Some(vec![right]),
                },
            );

            self.visit(if_expr)?;
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
            self.visit(arr_item)?;
        }

        let line = token.get_position().line;

        self.write_opcode(Opcode::ArrMk, line);
        self.write_byte(num_items as u8, line);
        Ok(())
    }

    fn visit_tuple(&mut self, token: Token, node: TypedTupleNode) -> Result<(), ()> {
        let num_items = node.items.len();
        for item in node.items {
            self.visit(item)?;
        }

        let line = token.get_position().line;

        self.write_opcode(Opcode::TupleMk, line);
        self.write_byte(num_items as u8, line);
        Ok(())
    }

    fn visit_map(&mut self, token: Token, node: TypedMapNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let num_items = node.items.len();
        for (key, value) in node.items {
            let key = Token::get_ident_name(&key).clone();
            self.write_constant(Value::Str(key), line);
            self.visit(value)?;
        }

        self.write_opcode(Opcode::MapMk, line);
        self.write_byte(num_items as u8, line);
        Ok(())
    }

    fn visit_set(&mut self, token: Token, node: TypedSetNode) -> Result<(), ()> {
        let num_items = node.items.len();
        for arr_item in node.items {
            self.visit(arr_item)?;
        }

        let line = token.get_position().line;

        self.write_opcode(Opcode::SetMk, line);
        self.write_byte(num_items as u8, line);
        Ok(())
    }

    fn visit_lambda(&mut self, token: Token, node: TypedLambdaNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let ret_type = if let Type::Fn(FnType { ret_type, .. }) = node.typ { *ret_type } else { unreachable!() };
        let body = node.typed_body.unwrap();
        let scope_depth = self.get_fn_depth();
        let fn_value = self.compile_function_decl(token, None, node.args, ret_type, body, scope_depth)?;

        let has_upvalues = !&fn_value.upvalues.is_empty();
        let const_idx = self.add_constant(Value::Fn(fn_value));

        self.write_opcode(Opcode::Constant, line);
        self.write_byte(const_idx, line);
        if has_upvalues {
            self.write_opcode(Opcode::ClosureMk, line);
        }

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
            self.write_constant(Value::Str(ident), line);
            self.write_opcode(Opcode::GStore, line);
        } else { // ...otherwise, it's a local
            if let Some(node) = expr {
                self.visit(*node)?;
            } else {
                self.write_opcode(Opcode::Nil, line);
            }
            self.push_local(ident, line, true);
        }
        Ok(())
    }

    fn visit_nil(&mut self, token: Token) -> Result<(), ()> {
        self.write_opcode(Opcode::Nil, token.get_position().line);
        Ok(())
    }

    fn visit_function_decl(&mut self, token: Token, node: TypedFunctionDeclNode) -> Result<(), ()> {
        let func_name = Token::get_ident_name(&node.name);
        let is_recursive = node.is_recursive;
        let line = token.get_position().line;

        if self.current_scope().kind == ScopeKind::Root { // If it's a global...
            self.write_int_constant(0, line);
            self.write_constant(Value::Str(func_name.clone()), line);
            self.write_opcode(Opcode::GStore, line);
        } else if is_recursive {
            self.write_int_constant(0, line);
            self.push_local(&func_name, line, true);
        }

        let fn_value = self.compile_function_decl(
            token,
            Some(node.name),
            node.args,
            node.ret_type,
            node.body,
            node.scope_depth,
        )?;

        let has_upvalues = !&fn_value.upvalues.is_empty();
        let const_idx = self.add_constant(Value::Fn(fn_value));

        self.write_opcode(Opcode::Constant, line);
        self.write_byte(const_idx, line);
        if has_upvalues {
            self.write_opcode(Opcode::ClosureMk, line);
        }

        if self.current_scope().kind == ScopeKind::Root { // If it's a global...
            self.write_constant(Value::Str(func_name.clone()), line);
            self.write_opcode(Opcode::GStore, line);
        } else if is_recursive {
            let scope_depth = self.get_fn_depth();
            let (local, fn_local_idx) = self.resolve_local(&func_name, scope_depth)
                .expect("There should have been a function pre-defined with this name");
            local.is_closed = true;
            self.write_store_local_instr(fn_local_idx, line);
            self.metadata.stores.push(func_name.clone());
            self.write_opcode(Opcode::CloseUpvalue, line);
        } else {
            self.push_local(func_name, line, true);
        }

        Ok(())
    }

    fn visit_type_decl(&mut self, token: Token, node: TypedTypeDeclNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let TypedTypeDeclNode { name, methods, static_fields, .. } = node;

        let type_name = Token::get_ident_name(&name);

        // To handle self-referencing types, initially store `0` as a placeholder
        if self.current_scope().kind == ScopeKind::Root { // If it's a global...
            self.write_int_constant(0, line);
            self.write_constant(Value::Str(type_name.clone()), line);
            self.write_opcode(Opcode::GStore, line);
        } else { // ...otherwise, it's a local
            unreachable!("Type declarations are only allowed at the root scope");
        }

        let mut compiled_methods = Vec::with_capacity(methods.len());
        for (method_name, method_node) in methods {
            let (method_tok, method_node) = match method_node {
                TypedAstNode::FunctionDecl(tok, node) => (tok, node),
                _ => unreachable!()
            };

            let method = self.compile_function_decl(
                method_tok,
                Some(method_node.name),
                method_node.args,
                method_node.ret_type,
                method_node.body,
                method_node.scope_depth,
            )?;
            compiled_methods.push((method_name, method));
        }

        let mut compiled_static_fields = Vec::new();
        for (_, _, value) in static_fields {
            if let Some(TypedAstNode::FunctionDecl(method_tok, method_node)) = value {
                let method_name = Token::get_ident_name(&method_node.name).clone();
                let method = self.compile_function_decl(
                    method_tok,
                    Some(method_node.name),
                    method_node.args,
                    method_node.ret_type,
                    method_node.body,
                    method_node.scope_depth,
                )?;
                compiled_static_fields.push((method_name, Value::Fn(method)));
            }
        }

        let type_value = Value::Type(TypeValue {
            name: type_name.clone(),
            methods: compiled_methods,
            static_fields: compiled_static_fields,
        });
        let const_idx = self.add_constant(type_value);
        self.write_opcode(Opcode::Constant, line);
        self.write_byte(const_idx, line);

        // Overwrite placeholder created at start
        if self.current_scope().kind == ScopeKind::Root { // If it's a global...
            self.write_constant(Value::Str(type_name.clone()), line);
            self.write_opcode(Opcode::GStore, line);
        } else { // ...otherwise, it's a local
            unreachable!("Type declarations are only allowed at the root scope");
        }

        Ok(())
    }

    fn visit_enum_decl(&mut self, token: Token, node: TypedEnumDeclNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let TypedEnumDeclNode { name, variants, static_fields, methods, .. } = node;

        let enum_name = Token::get_ident_name(&name);

        // To handle self-referencing enums, initially store `0` as a placeholder
        if self.current_scope().kind == ScopeKind::Root { // If it's a global...
            self.write_int_constant(0, line);
            self.write_constant(Value::Str(enum_name.clone()), line);
            self.write_opcode(Opcode::GStore, line);
        } else { // ...otherwise, it's a local
            unreachable!("Enum declarations are only allowed at the root scope");
        }

        let variants = variants.into_iter().enumerate()
            .map(|(idx, (ident_tok, _typ, variant_kind))| {
                let name = Token::get_ident_name(&ident_tok);
                let arity = match variant_kind {
                    EnumVariantKind::Basic => 0,
                    EnumVariantKind::Constructor(args) => args.len()
                };

                (name.clone(), EnumVariantObj { enum_name: enum_name.clone(), name, idx, arity, methods: vec![], values: None })
            })
            .collect();

        let mut compiled_methods = Vec::with_capacity(methods.len());
        for (method_name, method_node) in methods {
            let (method_tok, method_node) = match method_node {
                TypedAstNode::FunctionDecl(tok, node) => (tok, node),
                _ => unreachable!()
            };

            let method = self.compile_function_decl(
                method_tok,
                Some(method_node.name),
                method_node.args,
                method_node.ret_type,
                method_node.body,
                method_node.scope_depth,
            )?;
            compiled_methods.push((method_name, method));
        }

        let mut compiled_static_fields = Vec::new();
        for (_, _, value) in static_fields {
            if let Some(TypedAstNode::FunctionDecl(method_tok, method_node)) = value {
                let method_name = Token::get_ident_name(&method_node.name).clone();
                let method = self.compile_function_decl(
                    method_tok,
                    Some(method_node.name),
                    method_node.args,
                    method_node.ret_type,
                    method_node.body,
                    method_node.scope_depth,
                )?;
                compiled_static_fields.push((method_name, Value::Fn(method)));
            }
        }

        let enum_value = Value::Enum(EnumValue {
            name: enum_name.clone(),
            variants,
            methods: compiled_methods,
            static_fields: compiled_static_fields,
        });
        let const_idx = self.add_constant(enum_value);
        self.write_opcode(Opcode::Constant, line);
        self.write_byte(const_idx, line);

        // Overwrite placeholder created at start
        if self.current_scope().kind == ScopeKind::Root { // If it's a global...
            self.write_constant(Value::Str(enum_name.clone()), line);
            self.write_opcode(Opcode::GStore, line);
        } else { // ...otherwise, it's a local
            unreachable!("Type declarations are only allowed at the root scope");
        }

        Ok(())
    }

    fn visit_identifier(&mut self, token: Token, node: TypedIdentifierNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let ident = node.name;

        self.resolve_identifier(ident, line);
        Ok(())
    }

    fn visit_assignment(&mut self, token: Token, node: TypedAssignmentNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedAssignmentNode { target, expr, .. } = node;
        match *target {
            TypedAstNode::Indexing(_, TypedIndexingNode { target, index, .. }) => {
                let typ = target.get_type();

                self.visit(*target)?;
                match index {
                    IndexingMode::Index(idx) => self.visit(*idx)?,
                    _ => unreachable!()
                };
                self.visit(*expr)?;

                let opcode = match typ {
                    Type::Array(_) => Opcode::ArrStore,
                    Type::Tuple(_) => Opcode::TupleStore,
                    Type::Map(_, _) => Opcode::MapStore,
                    _ => unreachable!()
                };
                self.write_opcode(opcode, line);
            }
            TypedAstNode::Accessor(_, TypedAccessorNode { target, field_idx, .. }) => {
                self.visit(*expr)?;
                self.visit(*target)?;

                self.write_opcode(Opcode::SetField, line);
                self.write_byte(field_idx as u8, line);
            }
            TypedAstNode::Identifier(ident, _) => {
                let ident = Token::get_ident_name(&ident).clone();

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
                                let const_idx = self.get_constant_index(&Value::Str(ident.clone()));
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
            }
            _ => todo!()
        };

        Ok(())
    }

    fn visit_indexing(&mut self, token: Token, node: TypedIndexingNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedIndexingNode { target, index, .. } = node;

        let opcode = match &target.get_type() {
            Type::Map(_, _) => Opcode::MapLoad,
            Type::Array(_) | Type::String => Opcode::ArrLoad,
            Type::Tuple(_) => Opcode::TupleLoad,
            _ => unreachable!()
        };
        self.visit(*target)?;

        match index {
            IndexingMode::Index(idx) => {
                self.visit(*idx)?;
                self.write_opcode(opcode, line);
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
        let line = token.get_position().line;

        let TypedIfNode { condition, condition_binding, if_block, else_block, .. } = node;

        let is_opt = if let Type::Option(_) = condition.get_type() { true } else { false };
        self.visit(*condition)?;
        if let Some(_) = &condition_binding {
            // If there is a condition binding, duplicate the condition value. After the condition
            // is evaluated in the JumpIfF (passing through the != nil check if it's an option type)
            // it will remain on the stack. Once the If scope is the current scope...
            self.write_opcode(Opcode::Dup, line);
        }
        if is_opt {
            self.write_opcode(Opcode::Nil, line);
            self.write_opcode(Opcode::Neq, line);
        }

        self.write_opcode(Opcode::JumpIfF, line);
        self.write_byte(0, line); // <- Replaced after compiling if-block
        let jump_offset_slot_idx = self.code.len();

        self.push_scope(ScopeKind::If);
        if let Some(ident) = &condition_binding {
            // ...this value becomes the condition binding local. Since it's pushed within the If
            // scope, it'll be properly popped when the scope ends. Note that there is that value
            // floating on the stack, which is fine here since it's a local, but when we instead go
            // to the else branch...
            self.push_local(Token::get_ident_name(ident), line, true);
        }
        self.visit_block(if_block, is_stmt)?;
        self.pop_scope();

        // ...we need to pop that floating value off of the stack. Each block correctly cleans
        // up its locals (via the pop_scope() call), but since this value is placed on the stack
        // _before_ the blocks (and only (optionally) captured as a local in _one_ of them), we need
        // to make sure we pop it within the else-block too.
        // But if there IS no else-block to compile, we still need to properly emit this pop, and we
        // need to ensure this pop is only reachable if the if-block isn't taken; let's make a dummy
        // else-block so the jumps are handled properly.
        let else_block = match else_block {
            Some(block) => Some(block),
            None => if condition_binding.is_some() { Some(vec![]) } else { None }
        };
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
            // Pop the floating condition binding value off the stack, if present. See comment above
            // for how we know we can always do this here (tl;dr there will _always_ be an else-block
            // if there is a condition binding).
            if condition_binding.is_some() {
                self.write_opcode(Opcode::Pop, line);
            }

            self.push_scope(ScopeKind::If);
            self.visit_block(else_block, is_stmt)?;
            self.pop_scope();
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

    fn visit_match_statement(&mut self, is_stmt: bool, token: Token, node: TypedMatchNode) -> Result<(), ()> {
        let TypedMatchNode { target, branches, .. } = node;

        self.visit(*target)?;

        let mut end_match_jump_indexes = Vec::new();

        for (branch_type, branch_type_ident, binding, block, args) in branches {
            self.push_scope(ScopeKind::Block);

            let is_wildcard_case = branch_type != Type::Unknown && branch_type_ident.is_none();
            if is_wildcard_case { // Handle `_ => ...` case
                if let Some(binding_name) = &binding {
                    self.push_local(binding_name, token.get_position().line, true);
                } else {
                    // Pop match target if it's not bound as a local
                    self.write_opcode(Opcode::Pop, token.get_position().line);
                }
                self.visit_block(block, is_stmt)?;

                self.pop_scope();
                continue;
            }

            if branch_type == Type::Unknown { // Handle `None => ...` case
                self.write_opcode(Opcode::Dup, token.get_position().line);
                self.write_opcode(Opcode::Nil, token.get_position().line);
            } else if let Type::EnumVariant(_, EnumVariantType { variant_idx, .. }, _) = branch_type {
                self.write_opcode(Opcode::Dup, token.get_position().line);
                self.write_int_constant(variant_idx as u32, token.get_position().line);
            } else if let Some(TypeIdentifier::Normal { ident, .. }) = branch_type_ident { // Handle `Int => ...` case
                let type_name = Token::get_ident_name(&ident);
                let type_const_idx = self.get_type_constant_index(&type_name);

                self.write_opcode(Opcode::Dup, token.get_position().line);
                self.write_opcode(Opcode::Typeof, token.get_position().line);
                self.write_opcode(Opcode::Constant, token.get_position().line);
                self.write_byte(type_const_idx, token.get_position().line);
            } else {
                unimplemented!()
            }

            self.write_opcode(Opcode::Eq, token.get_position().line);
            self.write_opcode(Opcode::JumpIfF, token.get_position().line);
            self.write_byte(0, token.get_position().line); // <- Replaced after compiling block
            let next_case_jump_idx = self.code.len();

            // Push a local representing the match target still left on the stack (from the previous DUP).
            // If there is a name supplied in source to ascribe to that value we use it, otherwise use the intrinsic $match_target.
            // This value will be inaccessible if there is no label for it in source, but is necessary either way
            // to facilitate destructuring (if destructured_args are present in this match case).
            let binding_name = binding.unwrap_or("$match_target".to_string());
            self.push_local(&binding_name, token.get_position().line, true);
            if let Some(destructured_args) = &args {
                let depth = self.get_fn_depth();
                for (idx, arg_tok) in destructured_args.iter().enumerate() {
                    let (_, slot) = self.resolve_local(&binding_name, depth).unwrap();
                    self.write_load_local_instr(slot, token.get_position().line);
                    self.metadata.loads.push(binding_name.clone());

                    self.write_opcode(Opcode::GetField, token.get_position().line);
                    self.write_byte(idx as u8, token.get_position().line);
                    let arg_name = Token::get_ident_name(arg_tok);
                    self.push_local(arg_name, token.get_position().line, true);
                }
            }

            self.visit_block(block, is_stmt)?;

            self.write_opcode(Opcode::Jump, token.get_position().line);
            self.write_byte(0, token.get_position().line); // <- Replaced after compiling else-block
            end_match_jump_indexes.push(self.code.len());

            let code = &mut self.code;
            let case_block_len = code.len().checked_sub(next_case_jump_idx)
                .expect("jump offset slot should be <= end of the case block");
            *code.get_mut(next_case_jump_idx - 1).unwrap() = case_block_len as u8;

            self.pop_scope();
        }

        for idx in end_match_jump_indexes {
            let code = &mut self.code;
            let match_len = code.len().checked_sub(idx)
                .expect("jump offset slot should be <= end of all the match blocks");
            *code.get_mut(idx - 1).unwrap() = match_len as u8;
        }

        Ok(())
    }

    fn visit_match_expression(&mut self, token: Token, node: TypedMatchNode) -> Result<(), ()> {
        self.visit_match_statement(false, token, node)
    }

    fn visit_invocation(&mut self, token: Token, node: TypedInvocationNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let TypedInvocationNode { target, args, .. } = node;

        let typ = target.get_type();
        let (arity, has_return) = match typ {
            Type::Fn(FnType { arg_types, ret_type, .. }) => (arg_types.len(), *ret_type != Type::Unit),
            Type::EnumVariant(_, EnumVariantType { arg_types, .. }, _) => {
                let arity = arg_types.as_ref().map(|ts| ts.len()).expect("Typechecking should have caught invocation of non-constructor enum variants");
                (arity, true)
            }
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
        Ok(())
    }

    fn visit_instantiation(&mut self, token: Token, node: TypedInstantiationNode) -> Result<(), ()> {
        let line = token.get_position().line;
        let TypedInstantiationNode { target, fields, .. } = node;

        self.visit(*target)?;
        self.write_opcode(Opcode::Dup, line);

        let num_fields = fields.len();
        for (_, field_value) in fields.into_iter().rev() {
            self.visit(field_value)?;
        }

        self.write_opcode(Opcode::New, line);
        self.write_byte(num_fields as u8, line);

        self.write_opcode(Opcode::Init, line);

        Ok(())
    }

    fn visit_accessor(&mut self, token: Token, node: TypedAccessorNode) -> Result<(), ()> {
        let line = token.get_position().line;

        if node.is_opt_safe {
            // If we're in an opt-safe accessor (usage of the `?.` operator), we emit bytecode
            // for a transformed AST. We transform an expression like `a.b?.c?.d` to
            //   if a.b |$0| {
            //     if $0.c |$1| {
            //       if $1.d |$2| {
            //         $2
            //       }
            //     }
            //   }
            // to take advantage of the if-stmt/expr's condition binding. To achieve this, we first
            // must "unwind" the expression until we reach a non-opt-safe accessor, and then...
            let mut unwound_path = vec![];

            let this_node = Box::new(TypedAstNode::Accessor(token.clone(), node));
            let mut next = &this_node;
            loop {
                match &**next {
                    node @ TypedAstNode::Accessor(_, _) => {
                        if let TypedAstNode::Accessor(_, TypedAccessorNode { target, is_opt_safe, .. }) = &node {
                            unwound_path.push(node);
                            if *is_opt_safe { next = target } else { break; }
                        }
                    }
                    node @ _ => break unwound_path.push(node)
                }
            }

            fn make_dummy_ident_node(tok: &Token, name: String) -> TypedAstNode {
                let ident = Token::Ident(tok.get_position(), name.clone());
                // The `typ` and `scope_depth` fields don't matter here
                TypedAstNode::Identifier(ident, TypedIdentifierNode { typ: Type::Placeholder, name, is_mutable: false, scope_depth: 0 })
            }

            // ...we iterate through that path, starting with the innermost if-expr, building outward
            // until we reach the first non-opt accessor expr.
            let path_len = unwound_path.len();
            let mut layer_number = path_len;
            let mut if_node = None;
            for (idx, segment) in unwound_path.into_iter().map(|n| n.clone()).enumerate() {
                let is_last = idx == path_len - 1;
                let condition = if is_last {
                    segment
                } else {
                    match segment {
                        TypedAstNode::Accessor(tok, node) => {
                            let prev_cond_binding_name = format!("${}", layer_number - 1);
                            let target = Box::new(make_dummy_ident_node(&token, prev_cond_binding_name));
                            TypedAstNode::Accessor(tok, TypedAccessorNode { typ: node.typ, target, field_name: node.field_name, field_idx: node.field_idx, is_opt_safe: false })
                        }
                        _ => unimplemented!()
                    }
                };

                let cond_binding_name = format!("${}", layer_number);
                if_node = Some(TypedAstNode::IfExpression(
                    Token::If(token.get_position()),
                    TypedIfNode {
                        typ: Type::Placeholder, // Type doesn't matter
                        condition: Box::new(condition),
                        condition_binding: Some(Token::Ident(token.get_position(), cond_binding_name.clone())),
                        if_block: vec![
                            match if_node {
                                None => make_dummy_ident_node(&token, cond_binding_name),
                                Some(if_node) => if_node
                            }
                        ],
                        else_block: Some(vec![make_dummy_ident_node(&token, "None".to_string())]),
                    },
                ));
                layer_number -= 1;
            }
            let if_node = if_node.expect("It should be Some after the unwinding is through");

            self.visit(if_node)?;
        } else {
            let TypedAccessorNode { target, field_name, field_idx, .. } = node;
            self.metadata.field_gets.push(field_name);

            self.visit(*target)?;
            self.write_opcode(Opcode::GetField, line);
            self.write_byte(field_idx as u8, line);
        }

        Ok(())
    }

    fn visit_for_loop(&mut self, token: Token, node: TypedForLoopNode) -> Result<(), ()> {
        let line = token.get_position().line;

        let TypedForLoopNode { iteratee, index_ident, iterator, body } = node;
        let iterator_type = iterator.get_type();

        // Push intrinsic variable $idx, to track position in $iter
        self.push_scope(ScopeKind::Loop); // Create wrapper scope to hold invisible variables
        self.write_opcode(Opcode::IConst0, line); // Local 0 is iterator index ($idx)
        self.push_local("$idx", line, true);

        // $iter = <iterator>.enumerate()
        self.write_opcode(Opcode::Nil, line);
        self.visit(*iterator)?;
        let enumerate_method_idx = match iterator_type {
            Type::Array(_) => NativeArray::get_field_idx("enumerate"),
            Type::Set(_) => NativeSet::get_field_idx("enumerate"),
            Type::Map(_, _) => NativeMap::get_field_idx("enumerate"),
            _ => unreachable!("Should have been caught during typechecking")
        };
        self.write_opcode(Opcode::GetField, line);
        self.metadata.field_gets.push("enumerate".to_string());
        self.write_byte(enumerate_method_idx as u8, line);
        self.write_opcode(Opcode::Invoke, line);
        self.write_byte(0, line);
        self.push_local("$iter", line, true); // Local 1 is the iterator

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

        // Essentially: if $idx >= $iter.length { break }
        let cond_slot_idx = self.code.len();
        load_intrinsic(self, "$idx", line);
        load_intrinsic(self, "$iter", line);
        self.write_opcode(Opcode::GetField, line);
        self.metadata.field_gets.push("length".to_string());
        self.write_byte(NativeArray::get_field_idx("length") as u8, line);
        self.write_opcode(Opcode::LT, line);
        self.write_opcode(Opcode::JumpIfF, line);
        self.write_byte(0, line); // <- Replaced after compiling loop body
        let cond_jump_offset_slot_idx = self.code.len();

        // Insert iteratee and index bindings (if indexer expected) into loop scope
        //   $iter[$idx] is a tuple, of (<iteratee>, <index>)
        //   So, iteratee = $iter[$idx][0]
        self.push_scope(ScopeKind::Block);
        load_intrinsic(self, "$iter", line);
        load_intrinsic(self, "$idx", line);
        self.write_opcode(Opcode::ArrLoad, line);
        self.write_opcode(Opcode::IConst0, line);
        self.write_opcode(Opcode::TupleLoad, line);
        self.push_local(Token::get_ident_name(&iteratee), line, true);
        if let Some(ident) = index_ident {
            // If present, index = $iter[$idx][1]
            load_intrinsic(self, "$iter", line);
            load_intrinsic(self, "$idx", line);
            self.write_opcode(Opcode::ArrLoad, line);
            self.write_opcode(Opcode::IConst1, line);
            self.write_opcode(Opcode::TupleLoad, line);
            self.push_local(Token::get_ident_name(&ident), line, true);
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

        let TypedWhileLoopNode { condition, condition_binding, body } = node;
        let cond_slot_idx = self.code.len();

        self.push_scope(ScopeKind::Loop);
        let is_opt = if let Type::Option(_) = condition.get_type() { true } else { false };
        if let Some(ident) = &condition_binding {
            // If there is a condition binding, create a new local with initial value of Nil
            self.write_opcode(Opcode::Nil, line);
            self.push_local(Token::get_ident_name(ident), line, true);
        }

        self.visit(*condition)?;
        if let Some(ident) = &condition_binding {
            // If there is a condition binding, duplicate the condition value and store into the reserved local.
            self.write_opcode(Opcode::Dup, line);

            let ident = Token::get_ident_name(ident);
            let (_, slot) = self.resolve_local(&ident, self.get_fn_depth()).unwrap();
            self.write_store_local_instr(slot, line); // Store into condition binding
            self.metadata.stores.push(ident);
        }
        if is_opt {
            self.write_opcode(Opcode::Nil, line);
            self.write_opcode(Opcode::Neq, line);
        }

        self.write_opcode(Opcode::JumpIfF, line);
        self.write_byte(0, line); // <- Replaced after compiling loop body
        let cond_jump_offset_slot_idx = self.code.len();

        self.visit_loop_body(body, cond_slot_idx, cond_jump_offset_slot_idx)?;

        if let Some(_) = condition_binding {
            // If there was a condition binding, we need to pop it off the stack
            self.write_opcode(Opcode::Pop, line);
        }

        self.pop_scope();

        Ok(())
    }

    fn visit_break(&mut self, token: Token) -> Result<(), ()> {
        let line = token.get_position().line;

        // Emit bytecode to pop locals from stack. The scope in which the break statement lives
        // takes care of making sure the compiler's `locals` vec is in the correct state; here we
        // just need to emit the runtime popping. It's worth noting that locals in scopes deeper than
        // the loop also need to be popped
        let mut scopes_iter = self.scopes.iter().rev();
        let mut split_idx = self.locals.len() as i64;
        loop {
            let Scope { num_locals, kind, .. } = scopes_iter.next().unwrap();
            split_idx -= *num_locals as i64;
            if kind == &ScopeKind::Loop {
                break;
            }
        }
        let mut locals_to_pop = self.locals.split_off(split_idx as usize);
        self.write_pops(&locals_to_pop, line);
        self.locals.append(&mut locals_to_pop);

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
    use crate::common::compiler_util::ANON_IDX;
    use crate::vm::prelude::{PRELUDE_NUM_CONSTS, PRELUDE_PRINTLN_INDEX, PRELUDE_INT_INDEX};

    fn with_prelude_const_offset(const_idx: u8) -> u8 {
        PRELUDE_NUM_CONSTS.with(|n| *n + const_idx)
    }

    fn with_prelude_consts(constants: Vec<Value>) -> Vec<Value> {
        PRELUDE_BINDING_VALUES.with(|values| vec![values.clone(), constants].concat())
    }

    fn new_string_obj(string: &str) -> Value {
        Value::new_string_obj(string.to_string())
    }

    fn compile(input: &str) -> Module {
        // Yuck: need to reset this global so tests will be repeatable. Gross
        ANON_IDX.store(0, std::sync::atomic::Ordering::Relaxed);

        let tokens = tokenize(&input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        let (_, typed_ast) = typecheck(ast).unwrap();

        super::compile(typed_ast).unwrap().0
    }

    #[test]
    fn compile_empty() {
        let chunk = compile("");
        let expected = Module {
            code: vec![Opcode::Return as u8],
            constants: with_prelude_consts(vec![]),
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
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Pop as u8,
                Opcode::IConst4 as u8,
                Opcode::Pop as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Pop as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::Pop as u8,
                Opcode::T as u8,
                Opcode::Pop as u8,
                Opcode::F as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Float(2.3),
                Value::Float(5.6),
                new_string_obj("hello")
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_unary() {
        let chunk = compile("-5");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Invert as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![Value::Int(5)]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("-2.3");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Invert as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![Value::Float(2.3)]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("!false");
        let expected = Module {
            code: vec![
                Opcode::F as u8,
                Opcode::Negate as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_numeric() {
        let chunk = compile("5 + 6");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::IAdd as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![Value::Int(5), Value::Int(6)]),
        };
        assert_eq!(expected, chunk);

        // Testing i2f and order of ops
        let chunk = compile("1 - -5 * 3.4 / 5");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::I2F as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Invert as u8,
                Opcode::I2F as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::FMul as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::I2F as u8,
                Opcode::FDiv as u8,
                Opcode::FSub as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![Value::Int(5), Value::Float(3.4)]),
        };
        assert_eq!(expected, chunk);

        // Testing %, along with i2f
        let chunk = compile("3.4 % 2.4 % 5");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::FMod as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::I2F as u8,
                Opcode::FMod as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![Value::Float(3.4), Value::Float(2.4), Value::Int(5)]),
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
            constants: with_prelude_consts(vec![]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_str_concat() {
        let chunk = compile("\"abc\" + \"def\"");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::StrConcat as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                new_string_obj("abc"),
                new_string_obj("def"),
            ]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("1 + \"a\" + 3.4");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::StrConcat as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::StrConcat as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                new_string_obj("a"),
                Value::Float(3.4)
            ]),
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
            constants: with_prelude_consts(vec![]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_comparisons() {
        let chunk = compile("1 <= 5 == 3.4 >= 5.6");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::LTE as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::GTE as u8,
                Opcode::Eq as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![Value::Int(5), Value::Float(3.4), Value::Float(5.6)]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"a\" < \"b\" != 4");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::LT as u8,
                Opcode::IConst4 as u8,
                Opcode::Neq as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                new_string_obj("a"),
                new_string_obj("b")
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binary_coalesce() {
        let chunk = compile("[\"a\", \"b\"][2] ?: \"c\"");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::ArrMk as u8, 2,
                Opcode::IConst2 as u8,
                Opcode::ArrLoad as u8,
                Opcode::Dup as u8,
                Opcode::Nil as u8,
                Opcode::Neq as u8,
                Opcode::JumpIfF as u8, 6,
                Opcode::MarkLocal as u8, 0,
                Opcode::LLoad0 as u8,
                Opcode::LStore0 as u8,
                Opcode::Jump as u8, 3,
                Opcode::Pop as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                new_string_obj("a"),
                new_string_obj("b"),
                new_string_obj("c"),
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_array_literal() {
        let chunk = compile("[1, 2]");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::ArrMk as u8, 2,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("[\"a\", \"b\", \"c\"]");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::ArrMk as u8, 3,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                new_string_obj("a"),
                new_string_obj("b"),
                new_string_obj("c"),
            ]),
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
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::ArrMk as u8, 3,
                Opcode::ArrMk as u8, 2,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![Value::Int(5)]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_set_literal() {
        let chunk = compile("#{1, 2}");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::SetMk as u8, 2,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("#{\"a\", \"b\", \"c\"}");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::SetMk as u8, 3,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                new_string_obj("a"),
                new_string_obj("b"),
                new_string_obj("c"),
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_map_literal() {
        let chunk = compile("{ a: 1, b: \"c\", d: true }");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::T as u8,
                Opcode::MapMk as u8, 3,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("a".to_string()),
                Value::Str("b".to_string()),
                new_string_obj("c"),
                Value::Str("d".to_string()),
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_binding_decl() {
        let chunk = compile("val abc = 123");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![Value::Int(123), Value::Str("abc".to_string())]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("var unset: Bool\nvar set = true");
        let expected = Module {
            code: vec![
                Opcode::Nil as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::T as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("unset".to_string()),
                Value::Str("set".to_string())
            ]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("val abc = \"a\" + \"b\"\nval def = 5");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::StrConcat as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::Constant as u8, with_prelude_const_offset(4),
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                new_string_obj("a"),
                new_string_obj("b"),
                Value::Str("abc".to_string()),
                Value::Int(5),
                Value::Str("def".to_string()),
            ]),
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
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::Dup as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::New as u8, 1,
                Opcode::Init as u8,
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("Person".to_string()),
                Value::Type(TypeValue {
                    name: "Person".to_string(),
                    methods: vec![],
                    static_fields: vec![],
                }),
                new_string_obj("Meg"),
                Value::Str("meg".to_string()),
            ]),
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
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::Dup as u8,
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::New as u8, 2,
                Opcode::Init as u8,
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::Dup as u8,
                Opcode::Constant as u8, with_prelude_const_offset(4),
                Opcode::Constant as u8, with_prelude_const_offset(5),
                Opcode::New as u8, 2,
                Opcode::Init as u8,
                Opcode::Constant as u8, with_prelude_const_offset(6),
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("Person".to_string()),
                Value::Type(TypeValue { name: "Person".to_string(), methods: vec![], static_fields: vec![] }),
                new_string_obj("Unnamed"),
                Value::Str("someBaby".to_string()),
                Value::Int(29),
                new_string_obj("Some Name"),
                Value::Str("anAdult".to_string()),
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_ident() {
        let chunk = compile("val abc = 123\nabc");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GLoad as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![Value::Int(123), Value::Str("abc".to_string())]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_ident_upvalues() {
        let chunk = compile("func a(i: Int) {\nval b = 3\nfunc c() { b + 1 }\n}");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Return as u8,
            ],
            constants: with_prelude_consts(vec![
                Value::Str("a".to_string()),
                Value::Fn(FnValue {
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
                    receiver: None,
                    has_return: true,
                }),
                Value::Fn(FnValue {
                    name: "a".to_string(),
                    code: vec![
                        Opcode::IConst3 as u8,
                        Opcode::MarkLocal as u8, 1,
                        Opcode::Constant as u8, with_prelude_const_offset(1),
                        Opcode::ClosureMk as u8,
                        Opcode::MarkLocal as u8, 2,
                        Opcode::Pop as u8,
                        Opcode::CloseUpvalueAndPop as u8,
                        Opcode::Pop as u8,
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![],
                    receiver: None,
                    has_return: false,
                }),
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_ident_upvalues_skip_level() {
        let chunk = compile("func a(i: Int) {\nval b = 3\nfunc c() { func d() { b + 1 }\n}\n}");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Return as u8,
            ],
            constants: with_prelude_consts(vec![
                Value::Str("a".to_string()),
                Value::Fn(FnValue {
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
                    receiver: None,
                    has_return: true,
                }),
                Value::Fn(FnValue {
                    name: "c".to_string(),
                    code: vec![
                        Opcode::Constant as u8, with_prelude_const_offset(1),
                        Opcode::ClosureMk as u8,
                        Opcode::MarkLocal as u8, 0,
                        Opcode::Pop as u8,
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![
                        Upvalue {
                            capture_kind: UpvalueCaptureKind::Local { local_idx: 1 },
                            depth: 1,
                        }
                    ],
                    receiver: None,
                    has_return: false,
                }),
                Value::Fn(FnValue {
                    name: "a".to_string(),
                    code: vec![
                        Opcode::IConst3 as u8,
                        Opcode::MarkLocal as u8, 1,
                        Opcode::Constant as u8, with_prelude_const_offset(2),
                        Opcode::ClosureMk as u8,
                        Opcode::MarkLocal as u8, 2,
                        Opcode::Pop as u8,
                        Opcode::CloseUpvalueAndPop as u8,
                        Opcode::Pop as u8,
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![],
                    receiver: None,
                    has_return: false,
                }),
            ]),
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
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                // var b = 2
                Opcode::IConst2 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GStore as u8,

                // val c = b = a = 3
                //   a = 3
                Opcode::IConst3 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                //  b = <a = 3>
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GLoad as u8,
                //  c = <b = <a = 3>>
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("a".to_string()),
                Value::Str("b".to_string()),
                Value::Str("c".to_string()),
            ]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("var a = 1\na = 2\nval b = 3");
        let expected = Module {
            code: vec![
                // var a = 1
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                // a = 2
                Opcode::IConst2 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::Pop as u8, // <- This test verifies that the intermediate 2 gets popped
                // val b = 3
                Opcode::IConst3 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("a".to_string()),
                Value::Str("b".to_string()),
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_assignment_globals() {
        let chunk = compile("var a = 1\nfunc abc() { a = 3 }");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GStore as u8,
                Opcode::Return as u8,
            ],
            constants: with_prelude_consts(vec![
                Value::Str("a".to_string()),
                Value::Str("abc".to_string()),
                Value::Fn(FnValue {
                    name: "abc".to_string(),
                    code: vec![
                        Opcode::IConst3 as u8,
                        Opcode::Constant as u8, with_prelude_const_offset(0),
                        Opcode::GStore as u8,
                        Opcode::Constant as u8, with_prelude_const_offset(0),
                        Opcode::GLoad as u8,
                        Opcode::LStore0 as u8,
                        Opcode::Return as u8
                    ],
                    upvalues: vec![],
                    receiver: None,
                    has_return: true,
                }),
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_assignment_upvalues() {
        let chunk = compile("func outer() {\nvar a = 1\nfunc inner() { a = 3 }\n}");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Return as u8,
            ],
            constants: with_prelude_consts(vec![
                Value::Str("outer".to_string()),
                Value::Fn(FnValue {
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
                    receiver: None,
                    has_return: true,
                }),
                Value::Fn(FnValue {
                    name: "outer".to_string(),
                    code: vec![
                        Opcode::IConst1 as u8,
                        Opcode::MarkLocal as u8, 0,
                        Opcode::Constant as u8, with_prelude_const_offset(1),
                        Opcode::ClosureMk as u8,
                        Opcode::MarkLocal as u8, 1,
                        Opcode::Pop as u8,
                        Opcode::CloseUpvalueAndPop as u8,
                        Opcode::Return as u8
                    ],
                    upvalues: vec![],
                    receiver: None,
                    has_return: false,
                }),
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_assignment_indexing() {
        let chunk = compile("val a = [1]\na[0] = 0");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::ArrMk as u8, 1,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::IConst0 as u8,
                Opcode::IConst0 as u8,
                Opcode::ArrStore as u8,
                Opcode::Return as u8,
            ],
            constants: with_prelude_consts(vec![
                Value::Str("a".to_string()),
            ]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("val a = {b:1}\na[\"b\"] = 0");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::IConst1 as u8,
                Opcode::MapMk as u8, 1,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GLoad as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::IConst0 as u8,
                Opcode::MapStore as u8,
                Opcode::Return as u8,
            ],
            constants: with_prelude_consts(vec![
                Value::Str("b".to_string()),
                Value::Str("a".to_string()),
                new_string_obj("b")
            ]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("val a = (1, 2)\na[0] = 0");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::TupleMk as u8, 2,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::IConst0 as u8,
                Opcode::IConst0 as u8,
                Opcode::TupleStore as u8,
                Opcode::Return as u8,
            ],
            constants: with_prelude_consts(vec![
                Value::Str("a".to_string())
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_assignment_field_accessor() {
        let chunk = compile("\
          type Person { name: String }\n\
          val p = Person(name: \"Ken\")\n\
          p.name = \"Meg\"\
        ");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::Dup as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::New as u8, 1,
                Opcode::Init as u8,
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(4),
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::GLoad as u8,
                Opcode::SetField as u8, 0,
                Opcode::Return as u8,
            ],
            constants: with_prelude_consts(vec![
                Value::Str("Person".to_string()),
                Value::Type(TypeValue { name: "Person".to_string(), methods: vec![], static_fields: vec![] }),
                new_string_obj("Ken"),
                Value::Str("p".to_string()),
                new_string_obj("Meg"),
            ]),
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
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::ArrMk as u8, 5,
                Opcode::IConst3 as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::ArrLoad as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![Value::Int(5)]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"some string\"[1 + 1:]");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::IConst1 as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::Nil as u8,
                Opcode::ArrSlc as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                new_string_obj("some string"),
            ]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"some string\"[-1:4]");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::IConst1 as u8,
                Opcode::Invert as u8,
                Opcode::IConst4 as u8,
                Opcode::ArrSlc as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                new_string_obj("some string"),
            ]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\"some string\"[:1 + 1]");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::IConst0 as u8,
                Opcode::IConst1 as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::ArrSlc as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                new_string_obj("some string"),
            ]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("{ a: 1, b: 2 }[\"a\"]");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::IConst1 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::IConst2 as u8,
                Opcode::MapMk as u8, 2,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::MapLoad as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("a".to_string()),
                Value::Str("b".to_string()),
                new_string_obj("a"),
            ]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("(1, true, 3)[2]");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::T as u8,
                Opcode::IConst3 as u8,
                Opcode::TupleMk as u8, 3,
                Opcode::IConst2 as u8,
                Opcode::TupleLoad as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![]),
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
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Pop as u8,
                Opcode::Jump as u8, 3,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![Value::Int(123), Value::Int(456)]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("if (1 == 2) 123");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::Eq as u8,
                Opcode::JumpIfF as u8, 3,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![Value::Int(123)]),
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
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![Value::Int(456)]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("if (1 == 2) 123 else if (3 < 4) 456 else 789");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::Eq as u8,
                Opcode::JumpIfF as u8, 5,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Pop as u8,
                Opcode::Jump as u8, 13,
                Opcode::IConst3 as u8,
                Opcode::IConst4 as u8,
                Opcode::LT as u8,
                Opcode::JumpIfF as u8, 5,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Pop as u8,
                Opcode::Jump as u8, 3,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![Value::Int(123), Value::Int(456), Value::Int(789)]),
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
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GStore as u8,
                Opcode::T as u8,
                Opcode::JumpIfF as u8, 9,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::MarkLocal as u8, 0,
                Opcode::LLoad0 as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::Pop as u8,
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Int(123),
                Value::Str("a".to_string()),
                Value::Int(456),
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_if_else_statements_option_condition() {
        let chunk = compile("if ([1, 2][0]) 123 else 456");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::ArrMk as u8, 2,
                Opcode::IConst0 as u8,
                Opcode::ArrLoad as u8,
                Opcode::Nil as u8,
                Opcode::Neq as u8,
                Opcode::JumpIfF as u8, 5,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Pop as u8,
                Opcode::Jump as u8, 3,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![Value::Int(123), Value::Int(456)]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_if_else_statements_with_condition_binding() {
        let chunk = compile("if [1, 2][0] |item| item else 456");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::ArrMk as u8, 2,
                Opcode::IConst0 as u8,
                Opcode::ArrLoad as u8,
                Opcode::Dup as u8,
                Opcode::Nil as u8,
                Opcode::Neq as u8,
                Opcode::JumpIfF as u8, 7,
                Opcode::MarkLocal as u8, 0,
                Opcode::LLoad0 as u8,
                Opcode::Pop as u8,
                Opcode::Pop as u8,
                Opcode::Jump as u8, 4,
                Opcode::Pop as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![Value::Int(456)]),
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
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::IConst2 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GStore as u8,
                Opcode::IConst3 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::GStore as u8,
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(4),
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("a".to_string()),
                Value::Str("b".to_string()),
                Value::Str("c".to_string()),
                Value::Str("abc".to_string()),
                Value::Fn(FnValue {
                    name: "abc".to_string(),
                    code: vec![
                        Opcode::Constant as u8, with_prelude_const_offset(0),
                        Opcode::GLoad as u8,
                        Opcode::MarkLocal as u8, 2,
                        Opcode::LLoad1 as u8,
                        Opcode::LLoad2 as u8,
                        Opcode::IAdd as u8,
                        Opcode::MarkLocal as u8, 3,
                        Opcode::LLoad3 as u8,
                        Opcode::LStore0 as u8,
                        Opcode::Pop as u8,
                        Opcode::Pop as u8,
                        Opcode::Pop as u8,
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![],
                    receiver: None,
                    has_return: true,
                })
            ]),
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
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("abc".to_string()),
                new_string_obj("hello"),
                // Value::NativeFn(get_native_fn("println")),
                Value::Fn(FnValue {
                    name: "abc".to_string(),
                    code: vec![
                        Opcode::IConst1 as u8,
                        Opcode::MarkLocal as u8, 0,
                        Opcode::Constant as u8, with_prelude_const_offset(1),
                        Opcode::Constant as u8, PRELUDE_PRINTLN_INDEX,
                        Opcode::Invoke as u8, 1,
                        Opcode::Pop as u8, // Pop off `a`; note, there is no LStore0, since the return is Unit
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![],
                    receiver: None,
                    has_return: false,
                }),
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_function_declaration_default_args() {
        let chunk = compile("func add(a: Int, b = 2) = a + b\nadd(1)\nadd(1, 2)");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Nil as u8,
                Opcode::IConst1 as u8,
                Opcode::Nil as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::Invoke as u8, 2,
                Opcode::Pop as u8,
                Opcode::Nil as u8,
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::Invoke as u8, 2,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("add".to_string()),
                Value::Fn(FnValue {
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
                    receiver: None,
                    has_return: true,
                }),
            ]),
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
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("abc".to_string()),
                Value::Fn(FnValue {
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
                    receiver: None,
                    has_return: true,
                }),
                Value::Fn(FnValue {
                    name: "abc".to_string(),
                    code: vec![
                        Opcode::Constant as u8, with_prelude_const_offset(1),
                        Opcode::MarkLocal as u8, 2,
                        Opcode::LLoad1 as u8,
                        Opcode::Nil as u8,
                        Opcode::LLoad1 as u8,
                        Opcode::LLoad2 as u8,
                        Opcode::Invoke as u8, 1,
                        Opcode::IAdd as u8,
                        Opcode::MarkLocal as u8, 3,
                        Opcode::LLoad3 as u8,
                        Opcode::LStore0 as u8,
                        Opcode::Pop as u8,
                        Opcode::Pop as u8,
                        Opcode::Pop as u8,
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![],
                    receiver: None,
                    has_return: true,
                })
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_type_decl_struct_type_methods() {
        let chunk = compile("\
          type Person {\n\
            name: String\n\
            func getName(self): String = self.name\n\
            func getName2(self): String = self.getName()\n\
          }\
        ");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("Person".to_string()),
                Value::Type(TypeValue {
                    name: "Person".to_string(),
                    methods: vec![
                        ("getName".to_string(), FnValue {
                            name: "getName".to_string(),
                            code: vec![
                                Opcode::LLoad1 as u8,
                                Opcode::GetField as u8, 0,
                                Opcode::LStore0 as u8,
                                Opcode::Pop as u8,
                                Opcode::Return as u8
                            ],
                            upvalues: vec![],
                            receiver: None,
                            has_return: true,
                        }),
                        ("getName2".to_string(), FnValue {
                            name: "getName2".to_string(),
                            code: vec![
                                Opcode::Nil as u8,
                                Opcode::LLoad1 as u8,
                                Opcode::GetField as u8, 1,
                                Opcode::Invoke as u8, 0,
                                Opcode::LStore0 as u8,
                                Opcode::Pop as u8,
                                Opcode::Return as u8
                            ],
                            upvalues: vec![],
                            receiver: None,
                            has_return: true,
                        }),
                    ],
                    static_fields: vec![],
                }),
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_enum_decl_variants() {
        let chunk = compile("enum Status { On, Off }");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("Status".to_string()),
                Value::Enum(EnumValue {
                    name: "Status".to_string(),
                    methods: vec![],
                    static_fields: vec![],
                    variants: vec![
                        (
                            "On".to_string(),
                            EnumVariantObj {
                                enum_name: "Status".to_string(),
                                name: "On".to_string(),
                                idx: 0,
                                methods: vec![],
                                arity: 0,
                                values: None,
                            }
                        ),
                        (
                            "Off".to_string(),
                            EnumVariantObj {
                                enum_name: "Status".to_string(),
                                name: "Off".to_string(),
                                idx: 1,
                                methods: vec![],
                                arity: 0,
                                values: None,
                            }
                        )
                    ],
                }),
            ]),
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
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GStore as u8,
                Opcode::Nil as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GLoad as u8,
                Opcode::Invoke as u8, 1,
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("one".to_string()),
                Value::Str("inc".to_string()),
                Value::Fn(FnValue {
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
                    receiver: None,
                    has_return: true,
                }),
                Value::Str("two".to_string()),
            ]),
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
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::IConst1 as u8,
                Opcode::LT as u8,
                Opcode::JumpIfF as u8, 14,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::Pop as u8,
                Opcode::JumpB as u8, 21,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("i".to_string()),
            ]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("while ([1, 2][0]) { 123 }");
        let expected = Module {
            code: vec![
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::ArrMk as u8, 2,
                Opcode::IConst0 as u8,
                Opcode::ArrLoad as u8,
                Opcode::Nil as u8,
                Opcode::Neq as u8,
                Opcode::JumpIfF as u8, 5,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Pop as u8,
                Opcode::JumpB as u8, 15,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Int(123),
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_while_loop_with_condition_binding() {
        let chunk = compile("while ([1, 2][0]) |item| { item }");
        let expected = Module {
            code: vec![
                Opcode::Nil as u8,
                Opcode::MarkLocal as u8, 0,
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::ArrMk as u8, 2,
                Opcode::IConst0 as u8,
                Opcode::ArrLoad as u8,
                Opcode::Dup as u8,
                Opcode::LStore0 as u8,
                Opcode::Nil as u8,
                Opcode::Neq as u8,
                Opcode::JumpIfF as u8, 5,
                Opcode::LLoad0 as u8,
                Opcode::Pop as u8,
                Opcode::Pop as u8,
                Opcode::JumpB as u8, 20,
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![]),
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
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::IConst1 as u8,
                Opcode::LT as u8,
                Opcode::JumpIfF as u8, 18,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::MarkLocal as u8, 0,
                Opcode::LLoad0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::Pop as u8,
                Opcode::Pop as u8,
                Opcode::JumpB as u8, 25,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("i".to_string()),
            ]),
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
                Opcode::JumpIfF as u8, 8,
                Opcode::IConst1 as u8,
                Opcode::MarkLocal as u8, 0,
                Opcode::Pop as u8,       // <
                Opcode::Jump as u8, 2,   // < These 3 instrs are generated by the break
                Opcode::JumpB as u8, 11, // These 2 get falsely attributed to the break, because of #32
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![]),
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
                Opcode::JumpIfF as u8, 18,
                Opcode::IConst1 as u8,
                Opcode::MarkLocal as u8, 0,
                Opcode::LLoad0 as u8,
                Opcode::IConst1 as u8,
                Opcode::Eq as u8,
                Opcode::JumpIfF as u8, 7,
                Opcode::IConst2 as u8,
                Opcode::MarkLocal as u8, 1,
                Opcode::PopN as u8, 2,  // <
                Opcode::Jump as u8, 3,  // < These 3 instrs are generated by the break
                Opcode::Pop as u8,      // This instr is where the if jumps to if false (we still need to clean up locals in the loop)
                Opcode::JumpB as u8, 21,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_for_loop() {
        let chunk = compile("\
          val msg = \"Row: \"\n\
          val arr = [1, 2]\n\
          for a, i in arr {\n\
            println(msg + a + i)\n\
          }\
        ");
        let expected = Module {
            code: vec![
                // val msg = "Row: "
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GStore as u8,

                // val arr = [1, 2]
                Opcode::IConst1 as u8,
                Opcode::IConst2 as u8,
                Opcode::ArrMk as u8, 2,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::GStore as u8,

                // val $idx = 0
                // val $iter = arr.enumerate()
                // if $idx < $iter.length {
                Opcode::IConst0 as u8,
                Opcode::MarkLocal as u8, 0,
                Opcode::Nil as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::GLoad as u8,
                Opcode::GetField as u8, 2, // .length
                Opcode::Invoke as u8, 0,
                Opcode::MarkLocal as u8, 1,
                Opcode::LLoad0 as u8,
                Opcode::LLoad1 as u8,
                Opcode::GetField as u8, 0,
                Opcode::LT as u8,
                Opcode::JumpIfF as u8, 33,

                // a = $iter[$idx][0]
                Opcode::LLoad1 as u8,
                Opcode::LLoad0 as u8,
                Opcode::ArrLoad as u8,
                Opcode::IConst0 as u8,
                Opcode::TupleLoad as u8,
                Opcode::MarkLocal as u8, 2,

                // i = $iter[$idx][1]
                Opcode::LLoad1 as u8,
                Opcode::LLoad0 as u8,
                Opcode::ArrLoad as u8,
                Opcode::IConst1 as u8,
                Opcode::TupleLoad as u8,
                Opcode::MarkLocal as u8, 3,

                // $idx += 1
                Opcode::LLoad0 as u8,
                Opcode::IConst1 as u8,
                Opcode::IAdd as u8,
                Opcode::LStore0 as u8,

                // println(msg + a + i)
                // <recur>
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GLoad as u8,
                Opcode::LLoad2 as u8,
                Opcode::StrConcat as u8,
                Opcode::LLoad3 as u8,
                Opcode::StrConcat as u8,
                Opcode::Constant as u8, PRELUDE_PRINTLN_INDEX,
                Opcode::Invoke as u8, 1,
                Opcode::PopN as u8, 2,
                Opcode::JumpB as u8, 40,

                // Cleanup/end
                Opcode::Pop as u8,
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                new_string_obj("Row: "),
                Value::Str("msg".to_string()),
                Value::Str("arr".to_string()),
            ]),
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
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::Dup as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::New as u8, 1,
                Opcode::Init as u8,
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::GLoad as u8,
                Opcode::GetField as u8, 0,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("Person".to_string()),
                Value::Type(TypeValue { name: "Person".to_string(), methods: vec![], static_fields: vec![] }),
                new_string_obj("Ken"),
                Value::Str("ken".to_string()),
            ]),
        };
        assert_eq!(expected, chunk);

        // Accessing fields of structs
        let chunk = compile("\"hello\".length");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GetField as u8, 0,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                new_string_obj("hello"),
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_lambda_declaration_returns_unit_type() {
        let chunk = compile("\
          val abc = () => println(\"hello\")\
        ");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::GStore as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                new_string_obj("hello"),
                Value::Fn(FnValue {
                    name: "$anon_0".to_string(),
                    code: vec![
                        Opcode::Constant as u8, with_prelude_const_offset(0),
                        Opcode::Constant as u8, PRELUDE_PRINTLN_INDEX,
                        Opcode::Invoke as u8, 1,
                        Opcode::Return as u8,
                    ],
                    upvalues: vec![],
                    receiver: None,
                    has_return: false,
                }),
                Value::Str("abc".to_string()),
            ]),
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn compile_match_statement() {
        let chunk = compile("\
          val a: (String | Int)? = \"woo\"\n\
          match a {\n\
            None x => println(x)\n\
            _ x => println(x)\n\
          }
        ");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GLoad as u8,
                Opcode::Dup as u8,
                Opcode::Nil as u8,
                Opcode::Eq as u8,
                Opcode::JumpIfF as u8, 10,
                Opcode::MarkLocal as u8, 0, // x
                Opcode::LLoad0 as u8,
                Opcode::Constant as u8, PRELUDE_PRINTLN_INDEX,
                Opcode::Invoke as u8, 1,
                Opcode::Pop as u8,
                Opcode::Jump as u8, 8,
                Opcode::MarkLocal as u8, 0, // x
                Opcode::LLoad0 as u8,
                Opcode::Constant as u8, PRELUDE_PRINTLN_INDEX,
                Opcode::Invoke as u8, 1,
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                new_string_obj("woo"),
                Value::Str("a".to_string()),
            ]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\
          val a: (String | Int)? = \"woo\"\n\
          match a {\n\
            None => println(4)\n\
            _ x => println(x)\n\
          }
        ");
        let expected = Module {
            code: vec![
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::GLoad as u8,
                Opcode::Dup as u8,
                Opcode::Nil as u8,
                Opcode::Eq as u8,
                Opcode::JumpIfF as u8, 10,
                Opcode::MarkLocal as u8, 0,
                Opcode::IConst4 as u8,
                Opcode::Constant as u8, PRELUDE_PRINTLN_INDEX,
                Opcode::Invoke as u8, 1,
                Opcode::Pop as u8,
                Opcode::Jump as u8, 8,
                Opcode::MarkLocal as u8, 0, // x
                Opcode::LLoad0 as u8,
                Opcode::Constant as u8, PRELUDE_PRINTLN_INDEX,
                Opcode::Invoke as u8, 1,
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                new_string_obj("woo"),
                Value::Str("a".to_string()),
            ]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\
          type Person { name: String }\n\
          val a: String | Person = \"woo\"\n\
          match a {\n\
            Person p => println(p)\n\
            String s => println(s)\n\
          }
        ");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::GLoad as u8,
                Opcode::Dup as u8,
                Opcode::Typeof as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Eq as u8,
                Opcode::JumpIfF as u8, 10,
                Opcode::MarkLocal as u8, 0, // p
                Opcode::LLoad0 as u8,
                Opcode::Constant as u8, PRELUDE_PRINTLN_INDEX,
                Opcode::Invoke as u8, 1,
                Opcode::Pop as u8,
                Opcode::Jump as u8, 17,
                Opcode::Dup as u8,
                Opcode::Typeof as u8,
                Opcode::Constant as u8, PRELUDE_INT_INDEX,
                Opcode::Eq as u8,
                Opcode::JumpIfF as u8, 10,
                Opcode::MarkLocal as u8, 0, // s
                Opcode::LLoad0 as u8,
                Opcode::Constant as u8, PRELUDE_PRINTLN_INDEX,
                Opcode::Invoke as u8, 1,
                Opcode::Pop as u8,
                Opcode::Jump as u8, 0,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("Person".to_string()),
                Value::Type(TypeValue {
                    name: "Person".to_string(),
                    methods: vec![],
                    static_fields: vec![],
                }),
                new_string_obj("woo"),
                Value::Str("a".to_string()),
            ]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\
          enum Direction { Left, Right }\n\
          val d: Direction = Direction.Left\n\
          match d {\n\
            Direction.Left => println(\"Left\")\n\
            _ x => println(x)\n\
          }
        ");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::GetField as u8, 0,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::GLoad as u8,
                Opcode::Dup as u8,
                Opcode::IConst0 as u8,
                Opcode::Eq as u8,
                Opcode::JumpIfF as u8, 11,
                Opcode::MarkLocal as u8, 0,
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::Constant as u8, PRELUDE_PRINTLN_INDEX,
                Opcode::Invoke as u8, 1,
                Opcode::Pop as u8,
                Opcode::Jump as u8, 8,
                Opcode::MarkLocal as u8, 0, // x
                Opcode::LLoad0 as u8,
                Opcode::Constant as u8, PRELUDE_PRINTLN_INDEX,
                Opcode::Invoke as u8, 1,
                Opcode::Pop as u8,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("Direction".to_string()),
                Value::Enum(EnumValue {
                    name: "Direction".to_string(),
                    variants: vec![
                        (
                            "Left".to_string(),
                            EnumVariantObj {
                                enum_name: "Direction".to_string(),
                                name: "Left".to_string(),
                                idx: 0,
                                methods: vec![],
                                arity: 0,
                                values: None,
                            }
                        ),
                        (
                            "Right".to_string(),
                            EnumVariantObj {
                                enum_name: "Direction".to_string(),
                                name: "Right".to_string(),
                                idx: 1,
                                methods: vec![],
                                arity: 0,
                                values: None,
                            }
                        )
                    ],
                    methods: vec![],
                    static_fields: vec![],
                }),
                Value::Str("d".to_string()),
                new_string_obj("Left")
            ]),
        };
        assert_eq!(expected, chunk);

        let chunk = compile("\
          enum Foo { Bar(baz: Int) }\n\
          val f: Foo = Foo.Bar(baz: 24)\n\
          match f {\n\
            Foo.Bar(baz) => println(baz)
          }
        ");
        let expected = Module {
            code: vec![
                Opcode::IConst0 as u8,
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(1),
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GStore as u8,
                Opcode::Nil as u8,
                Opcode::Constant as u8, with_prelude_const_offset(2),
                Opcode::Constant as u8, with_prelude_const_offset(0),
                Opcode::GLoad as u8,
                Opcode::GetField as u8, 0,
                Opcode::Invoke as u8, 1,
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::GStore as u8,
                Opcode::Constant as u8, with_prelude_const_offset(3),
                Opcode::GLoad as u8,
                Opcode::Dup as u8,
                Opcode::IConst0 as u8,
                Opcode::Eq as u8,
                Opcode::JumpIfF as u8, 16,
                Opcode::MarkLocal as u8, 0, // $match_target
                Opcode::LLoad0 as u8,
                Opcode::GetField as u8, 0,
                Opcode::MarkLocal as u8, 1, // baz
                Opcode::LLoad1 as u8,
                Opcode::Constant as u8, PRELUDE_PRINTLN_INDEX,
                Opcode::Invoke as u8, 1,
                Opcode::PopN as u8, 2,
                Opcode::Jump as u8, 0,
                Opcode::Return as u8
            ],
            constants: with_prelude_consts(vec![
                Value::Str("Foo".to_string()),
                Value::Enum(EnumValue {
                    name: "Foo".to_string(),
                    variants: vec![
                        (
                            "Bar".to_string(),
                            EnumVariantObj {
                                enum_name: "Foo".to_string(),
                                name: "Bar".to_string(),
                                idx: 0,
                                methods: vec![],
                                arity: 1,
                                values: None,
                            }
                        ),
                    ],
                    methods: vec![],
                    static_fields: vec![],
                }),
                Value::Int(24),
                Value::Str("f".to_string()),
            ]),
        };
        assert_eq!(expected, chunk);
    }
}
