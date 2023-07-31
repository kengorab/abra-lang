use itertools::Itertools;
use crate::lexer::tokens::Token;
use crate::parser::ast::{BindingPattern, UnaryOp};
use crate::typechecker::typechecker2::{Enum, EnumId, FuncId, Function, ModuleId, PRELUDE_BOOL_TYPE_ID, PRELUDE_FLOAT_TYPE_ID, PRELUDE_INT_TYPE_ID, PRELUDE_MODULE_ID, PRELUDE_UNIT_TYPE_ID, PrimitiveType, Project, ScopeId, Struct, StructId, Type, TypedLiteral, TypedModule, TypedNode, TypeId, TypeKind, VariableAlias};

fn function_name(func: &Function) -> String {
    let FuncId(ScopeId(ModuleId(m_id), s_id), f_id) = func.id;
    format!("_{}_{}_{}__{}", m_id, s_id, f_id, func.name)
}

pub struct CCompiler2<W: std::io::Write> {
    output: W,
    temp_idx: usize,
}

impl<W: std::io::Write> CCompiler2<W> {
    pub fn new(output: W) -> Self {
        CCompiler2 { output, temp_idx: 0 }
    }

    fn next_temp_variable(&mut self) -> String {
        let temp_idx = self.temp_idx;
        self.temp_idx += 1;
        format!("_tmp__{}", temp_idx)
    }

    fn get_type_name_by_id(&self, project: &Project, type_id: &TypeId) -> String {
        let ty = project.get_type_by_id(type_id);
        match ty {
            Type::Primitive(PrimitiveType::Unit) => "AbraUnit".to_string(),
            Type::Primitive(PrimitiveType::Any) => "AbraAny".to_string(),
            Type::Primitive(PrimitiveType::Int) => "AbraInt".to_string(),
            Type::Primitive(PrimitiveType::Float) => "AbraFloat".to_string(),
            Type::Primitive(PrimitiveType::Bool) => "AbraBool".to_string(),
            Type::Primitive(PrimitiveType::String) => "AbraString".to_string(),
            Type::Generic(_, _) => todo!(),
            Type::GenericInstance(struct_id, generic_ids) => {
                if *struct_id == project.prelude_array_struct_id {
                    "AbraArray".to_string()
                } else if generic_ids.is_empty() {
                    self.get_struct_enum_name(project, &TypeKind::Struct(*struct_id))
                } else {
                    todo!()
                }
            }
            Type::GenericEnumInstance(_, _, _) |
            Type::Function(_, _, _, _) => todo!(),
            Type::Type(kind) => self.get_struct_enum_name(project, kind),
            Type::ModuleAlias => todo!(),
        }
    }

    fn get_struct_enum_name(&self, project: &Project, kind: &TypeKind) -> String {
        match kind {
            TypeKind::Struct(struct_id) => {
                let StructId(ModuleId(m_id), s_id) = struct_id;
                let struct_ = project.get_struct_by_id(struct_id);
                format!("_{}_0_{}__{}", m_id, s_id, struct_.name)
            }
            TypeKind::Enum(enum_id) => {
                let EnumId(ModuleId(m_id), e_id) = enum_id;
                let enum_ = project.get_enum_by_id(enum_id);
                format!("_{}_0_{}__{}", m_id, e_id, enum_.name)
            }
        }
    }

    fn get_module_entrypoint_name(&self, module: &TypedModule) -> String {
        format!("entrypoint__{}", module.id.0)
    }

    // --- EMITTER LOGIC START ---

    fn emit<S: AsRef<str>>(&mut self, str: S) {
        write!(self.output, "{}", str.as_ref()).expect("Output should be able to be written to")
    }

    fn emit_line<S: AsRef<str>>(&mut self, str: S) {
        writeln!(self.output, "{}", str.as_ref()).expect("Output should be able to be written to")
    }

    fn emit_newline(&mut self) {
        writeln!(self.output).expect("Output should be able to be written to");
    }

    fn emit_comment<S: AsRef<str>>(&mut self, comment: S) {
        writeln!(self.output, "// {}", comment.as_ref()).expect("Output should be able to be written to");
    }

    fn emit_fn_signature(&mut self, project: &Project, func_id: &FuncId) {
        let function = project.get_func_by_id(func_id);
        let ret_type = self.get_type_name_by_id(project, &function.return_type_id);
        let fn_name = function_name(&function);
        let params = function.params.iter()
            .map(|p| format!("{} {}", self.get_type_name_by_id(project, &p.type_id), &p.name))
            .join(", ");

        write!(self.output, "{} {}({})", ret_type, fn_name, params).expect("Output should be able to be written to");
    }

    fn emit_fn_predecl(&mut self, project: &Project, func_id: &FuncId) {
        self.emit_fn_signature(project, func_id);
        writeln!(self.output, ";").expect("Output should be able to be written to");
    }

    fn emit_struct_predecl(&mut self, project: &Project, struct_: &Struct) {
        writeln!(self.output, "struct {};", self.get_struct_enum_name(project, &TypeKind::Struct(struct_.id))).expect("Output should be able to be written to");
    }

    fn emit_struct_decl(&mut self, project: &Project, struct_: &Struct) {
        writeln!(self.output, "typedef struct {} {{", self.get_struct_enum_name(project, &TypeKind::Struct(struct_.id))).expect("Output should be able to be written to");
        for field in &struct_.fields {
            writeln!(self.output, "  {} {};", self.get_type_name_by_id(project, &field.type_id), &field.name).expect("Output should be able to be written to");
        }
        writeln!(self.output, "}} {};", self.get_struct_enum_name(project, &TypeKind::Struct(struct_.id))).expect("Output should be able to be written to");

        for func_id in &struct_.methods {
            let function = project.get_func_by_id(func_id);
            if !function.captured_vars.is_empty() { todo!("Need to implement closures") }
            self.emit_fn_predecl(project, func_id);
        }
    }

    fn emit_enum_predecl(&mut self, project: &Project, enum_: &Enum) {
        writeln!(self.output, "enum {};", self.get_struct_enum_name(project, &TypeKind::Enum(enum_.id))).expect("Output should be able to be written to");
    }

    fn emit_main_function(&mut self, project: &Project) {
        self.emit_line("int main(int argc, char** argv) {");
        self.emit_comment("Runtime initialization");
        self.emit_line(format!("init_vtable({});", project.modules.iter().map(|m| m.structs.len()).sum::<usize>()));

        self.emit_newline();
        self.emit_comment("Load and execute module code");
        for module in &project.modules {
            self.emit_comment(&module.name);
            writeln!(self.output, "{}();", self.get_module_entrypoint_name(&module)).expect("Output should be able to be written to");
        }
        writeln!(self.output, "}}").expect("Output should be able to be written to");
    }

    // --- EMITTER LOGIC END ---

    pub fn generate(&mut self, project: Project) {
        self.emit_line("#include \"prelude.h\"\n");

        self.emit_comment("Supply extern type_id constants for builtin prelude types");
        self.emit_line("const size_t TYPE_ID_NONE = 0;");
        self.emit_line(format!("const size_t TYPE_ID_INT = {};", project.prelude_int_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_FLOAT = {};", project.prelude_float_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_BOOL = {};", project.prelude_bool_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_STRING = {};", project.prelude_string_struct_id.1));
        self.emit_line(format!("const size_t TYPE_ID_ARRAY = {};", project.prelude_array_struct_id.1));
        self.emit_newline();

        for module in &project.modules {
            if module.id == PRELUDE_MODULE_ID { continue; }

            self.emit_comment(&module.name);
            self.compile_forward_declarations(&project, &module);

            self.compile_toplevel_code(&project, &module);
        }

        self.emit_newline();
        self.emit_comment("Entrypoint begins here");
        self.emit_main_function(&project);
    }

    fn compile_forward_declarations(&mut self, project: &Project, module: &TypedModule) {
        for struct_ in &module.structs {
            self.emit_struct_predecl(project, struct_);
        }
        if !module.structs.is_empty() { self.emit_newline(); }

        for enum_ in &module.enums {
            self.emit_enum_predecl(project, enum_);
        }
        if !module.enums.is_empty() { self.emit_newline(); }

        for func_id in &module.functions {
            let function = project.get_func_by_id(func_id);
            // Don't emit predecls for methods; let's organize it so those are next to the typedefs
            if function.has_self { continue; }

            self.emit_fn_predecl(project, func_id);
        }
        if !module.functions.is_empty() { self.emit_newline(); }

        for struct_ in &module.structs {
            self.emit_struct_decl(project, struct_);
            self.emit_newline();
        }
    }

    fn compile_toplevel_code(&mut self, project: &Project, module: &TypedModule) {
        self.emit_line(&format!("void {}() {{", self.get_module_entrypoint_name(&module)));
        for node in &module.code {
            self.compile_statement(project, node);
        }
        self.emit_line("}");
    }

    fn compile_statement(&mut self, project: &Project, node: &TypedNode) {
        match node {
            TypedNode::If { .. } => {}
            TypedNode::Match { .. } => {}
            TypedNode::BindingDeclaration { expr, vars, pattern, .. } => {
                if let Some(expr) = expr {
                    let expr_handle = self.compile_expression(project, expr);
                    self.compile_pattern_destructuring(project, &expr_handle,expr.as_ref().type_id(), pattern);
                } else {
                    for var_id in vars {
                        let var = project.get_var_by_id(var_id);
                        self.emit_line(format!("{} {};", self.get_type_name_by_id(project, &var.type_id), &var.name));
                    }
                }
            }
            TypedNode::ForLoop { .. } => {}
            TypedNode::WhileLoop { .. } => {}
            TypedNode::Break { .. } => {}
            TypedNode::Continue { .. } => {}
            TypedNode::Return { .. } => {}
            TypedNode::Import { .. } => {}
            n => {
                self.compile_expression(project, n);
            }
        }
    }

    fn compile_pattern_destructuring(&mut self, project: &Project, expr_handle: &String, expr_type: &TypeId, pattern: &BindingPattern) {
        match pattern {
            BindingPattern::Variable(tok) => {
                let var_name = Token::get_ident_name(tok);
                self.emit_line(format!("{}* {} = {};", self.get_type_name_by_id(project, expr_type), var_name, expr_handle));
            }
            BindingPattern::Tuple(_, _) |
            BindingPattern::Array(_, _, _) => todo!(),
        }
    }

    fn compile_expression(&mut self, project: &Project, node: &TypedNode) -> String {
        match node {
            TypedNode::Literal { value, .. } => match value {
                TypedLiteral::Int(i) => format!("AbraInt_make({})", i),
                TypedLiteral::Float(f) => format!("AbraFloat_make({})", f),
                TypedLiteral::Bool(b) => format!("AbraBool_make({})", b),
                TypedLiteral::String(s) => format!("AbraString_make({}, \"{}\")", s.len(), s),
            }
            TypedNode::Unary { expr, op, .. } => {
                let type_id = *expr.as_ref().type_id();

                let fn_name = if type_id == PRELUDE_INT_TYPE_ID {
                    "AbraInt_make"
                } else if type_id == PRELUDE_FLOAT_TYPE_ID {
                    "AbraFloat_make"
                } else if type_id == PRELUDE_BOOL_TYPE_ID {
                    "AbraBool_make"
                } else {
                    unreachable!()
                };

                let fn_arg = match (op, expr.as_ref()) {
                    (UnaryOp::Minus, TypedNode::Literal { value: TypedLiteral::Int(value), .. }) => format!("-{}", value),
                    (UnaryOp::Minus, TypedNode::Literal { value: TypedLiteral::Float(value), .. }) => format!("-{}", value),
                    (UnaryOp::Negate, TypedNode::Literal { value: TypedLiteral::Bool(value), .. }) => format!("!{}", value),
                    (UnaryOp::Minus, expr) => format!("-({})->value", self.compile_expression(project, expr)),
                    (UnaryOp::Negate, expr) => format!("!({})->value", self.compile_expression(project, expr)),
                };

                format!("{}({})", fn_name, fn_arg)
            }
            TypedNode::Binary { .. } => unimplemented!(),
            TypedNode::Grouped { .. } => unimplemented!(),
            TypedNode::Array { items, .. } => {
                let capacity = if items.is_empty() { 0 } else { items.len().next_power_of_two() };

                let handle = self.next_temp_variable();

                self.emit_line(format!("AbraArray* {} = AbraArray_make_with_capacity(/*length:*/ {}, /*capacity:*/{});", handle, items.len(), capacity));
                for (idx, item) in items.iter().enumerate() {
                    let expr_handle = self.compile_expression(project, &item);
                    self.emit_line(format!("AbraArray_set(/*self:*/{}, /*idx:*/{}, /*item:*/(AbraAny*){});", handle, idx, expr_handle));
                }

                handle
            }
            TypedNode::Tuple { .. } => unimplemented!(),
            TypedNode::Set { .. } => unimplemented!(),
            TypedNode::Map { .. } => unimplemented!(),
            TypedNode::Identifier { var_id, .. } => {
                let variable = project.get_var_by_id(var_id);
                match variable.alias {
                    VariableAlias::None => variable.name.clone(),
                    VariableAlias::Function(func_id) => {
                        let function = project.get_func_by_id(&func_id);
                        function_name(function)
                    }
                    VariableAlias::Type(_) => unimplemented!(),
                }
            }
            TypedNode::NoneValue { .. } => unimplemented!(),
            TypedNode::Invocation { target, arguments, type_id, .. } => {
                let num_arguments = arguments.len();
                let mut arg_values = Vec::with_capacity(num_arguments);
                for (_, argument) in arguments.iter().enumerate() {
                    let argument = argument.as_ref().expect("TODO: handle optional arguments");
                    let handle = self.compile_expression(project, &argument);
                    arg_values.push(handle);
                }
                let target_type_id = type_id;

                let handle = if *target_type_id != PRELUDE_UNIT_TYPE_ID {
                    let handle = self.next_temp_variable();
                    self.emit(format!("{} {} = ", self.get_type_name_by_id(project, target_type_id), handle));
                    Some(handle)
                } else { None };
                let target_handle = self.compile_expression(project, &*target);
                self.emit(target_handle);
                self.emit("(");
                self.emit(format!("{}, ", num_arguments));
                self.emit(arg_values.join(", "));
                self.emit_line(");");

                handle.unwrap_or_default()
            }
            TypedNode::Accessor { .. } => unimplemented!(),
            TypedNode::Indexing { .. } => unimplemented!(),
            TypedNode::Lambda { .. } => unimplemented!(),
            TypedNode::Assignment { .. } => unimplemented!(),
            n => unreachable!("Internal error: node is not an expression: {:?}", n),
        }
    }
}

#[cfg(test)]
mod test {
    use std::process::Command;
    use assert_cmd::cargo::CommandCargoExt;
    use itertools::{EitherOrBoth, Itertools};
    use crate::transpile::get_project_root::get_project_root;

    fn run_test_file(file_name: &str) {
        let rust_project_root = get_project_root().unwrap();

        let test_file_path = rust_project_root.join("abra_core").join("src").join("transpile").join("testv2").join(file_name);
        let test_file = std::fs::read_to_string(&test_file_path).unwrap();

        let mut cmd = Command::cargo_bin("abra").unwrap();
        cmd.arg("compile2").arg(&test_file_path);
        let output = cmd.output().unwrap();
        assert!(output.stderr.is_empty(), "Compilation error: {}", String::from_utf8(output.stderr).unwrap());

        let output = String::from_utf8(output.stdout).unwrap();
        let output = output.lines();

        let prefix = "/// Expect: ";
        let expectations = test_file.lines()
            .enumerate()
            .filter(|(_, line)| line.starts_with(prefix))
            .map(|(line_num, line)| (line_num + 1, line.replace(prefix, "")))
            .collect_vec();

        for pair in expectations.iter().zip_longest(output) {
            match pair {
                EitherOrBoth::Both((line_num, expected), actual) => {
                    assert_eq!(expected, actual, "Expectation mismatch at {}:{}", test_file_path.to_str().unwrap(), line_num);
                }
                EitherOrBoth::Left((line_num, expected)) => {
                    assert!(false, "Expected: {} (line {}), but reached end of output", expected, line_num);
                }
                EitherOrBoth::Right(actual) => {
                    assert!(false, "Received line: {}, but there were no more expectations", actual);
                }
            }
        }
    }

    #[test]
    fn builtin_values() {
        run_test_file("builtinValues.abra");
    }

    #[test]
    fn unary_ops() {
        run_test_file("unaryOps.abra");
    }

    #[test]
    fn variables() {
        run_test_file("variables.abra");
    }
}
