use itertools::Itertools;
use crate::typechecker::typechecker2::{Enum, EnumId, FuncId, Function, ModuleId, PRELUDE_MODULE_ID, PrimitiveType, Project, ScopeId, Struct, StructId, Type, TypedModule, TypeId, TypeKind};

pub struct CCompiler2 {
    project: Project,
}

#[derive(Debug)]
pub enum CGenError {}

fn function_name(func: &Function) -> String {
    let FuncId(ScopeId(ModuleId(m_id), s_id), f_id) = func.id;
    format!("{}_{}_{}__{}", m_id, s_id, f_id, func.name)
}

impl CCompiler2 {
    pub fn new(project: Project) -> Self {
        CCompiler2 { project }
    }

    fn get_type_name_by_id(&self, type_id: &TypeId) -> String {
        let ty = &self.project.get_type_by_id(type_id);
        match ty {
            Type::Primitive(PrimitiveType::Unit) => "AbraUnit".to_string(),
            Type::Primitive(PrimitiveType::Any) => "AbraAny".to_string(),
            Type::Primitive(PrimitiveType::Int) => "AbraInt".to_string(),
            Type::Primitive(PrimitiveType::Float) => "AbraFloat".to_string(),
            Type::Primitive(PrimitiveType::Bool) => "AbraBool".to_string(),
            Type::Primitive(PrimitiveType::String) => "AbraString".to_string(),
            Type::Generic(_, _) => todo!(),
            Type::GenericInstance(struct_id, generic_ids) => {
                if generic_ids.is_empty() {
                    self.get_struct_enum_name(&TypeKind::Struct(*struct_id))
                } else {
                    todo!()
                }
            }
            Type::GenericEnumInstance(_, _, _) |
            Type::Function(_, _, _, _) => todo!(),
            Type::Type(kind) => self.get_struct_enum_name(kind),
            Type::ModuleAlias => todo!(),
        }
    }

    fn get_struct_enum_name(&self, kind: &TypeKind) -> String {
        match kind {
            TypeKind::Struct(struct_id) => {
                let StructId(ModuleId(m_id), s_id) = struct_id;
                let struct_ = self.project.get_struct_by_id(struct_id);
                format!("{}_0_{}__{}", m_id, s_id, struct_.name)
            }
            TypeKind::Enum(enum_id) => {
                let EnumId(ModuleId(m_id), e_id) = enum_id;
                let enum_ = self.project.get_enum_by_id(enum_id);
                format!("{}_0_{}__{}", m_id, e_id, enum_.name)
            }
        }
    }

    // --- EMITTER LOGIC START ---

    fn emit_newline(&self) {
        println!();
    }

    fn emit_debug_info<S: AsRef<str>>(&self, comment: S) {
        println!("// {}", comment.as_ref());
    }

    fn emit_fn_signature(&self, func_id: &FuncId) {
        let function = self.project.get_func_by_id(func_id);
        let ret_type = self.get_type_name_by_id(&function.return_type_id);
        let fn_name = function_name(&function);
        let params = function.params.iter()
            .map(|p| format!("{} {}", self.get_type_name_by_id(&p.type_id), &p.name))
            .join(", ");

        print!("{} {}({})", ret_type, fn_name, params);
    }

    fn emit_fn_predecl(&self, func_id: &FuncId) {
        self.emit_fn_signature(&func_id);
        println!(";");
    }

    fn emit_struct_predecl(&self, struct_: &Struct) {
        println!("struct {};", self.get_struct_enum_name(&TypeKind::Struct(struct_.id)));
    }

    fn emit_struct_decl(&self, struct_: &Struct) {
        println!("typedef struct {} {{", self.get_struct_enum_name(&TypeKind::Struct(struct_.id)));
        for field in &struct_.fields {
            println!("  {} {};", self.get_type_name_by_id(&field.type_id), &field.name);
        }
        println!("}} {};", self.get_struct_enum_name(&TypeKind::Struct(struct_.id)));

        for func_id in &struct_.methods {
            let function = self.project.get_func_by_id(func_id);
            if !function.captured_vars.is_empty() { todo!("Need to implement closures") }
            self.emit_fn_predecl(func_id);
        }
    }

    fn emit_enum_predecl(&self, enum_: &Enum) {
        println!("enum {};", self.get_struct_enum_name(&TypeKind::Enum(enum_.id)));
    }

    // --- EMITTER LOGIC END ---

    pub fn generate(&mut self) -> Result<(), CGenError> {
        for module in &self.project.modules {
            if module.id == PRELUDE_MODULE_ID { continue; }

            self.emit_debug_info(&module.name);
            self.compile_forward_declarations(&module)?;
        }

        Ok(())
    }

    fn compile_forward_declarations(&self, module: &TypedModule) -> Result<(), CGenError> {
        for struct_ in &module.structs {
            self.emit_struct_predecl(struct_);
        }
        if !module.structs.is_empty() { self.emit_newline(); }

        for enum_ in &module.enums {
            self.emit_enum_predecl(enum_);
        }
        if !module.enums.is_empty() { self.emit_newline(); }

        for func_id in &module.functions {
            let function = self.project.get_func_by_id(func_id);
            // Don't emit predecls for methods; let's organize it so those are next to the typedefs
            if function.has_self { continue; }

            self.emit_fn_predecl(func_id);
        }
        if !module.functions.is_empty() { self.emit_newline(); }

        for struct_ in &module.structs {
            self.emit_struct_decl(struct_);
            self.emit_newline();
        }

        Ok(())
    }
}
