use std::path::PathBuf;
use std::process::{Command, ExitStatus};
use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::TargetMachine;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, FloatType, FunctionType, IntType, PointerType, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue};
use abra_core::typechecker::typechecker2::{PRELUDE_MODULE_ID, PRELUDE_UNIT_TYPE_ID, Project, TypedLiteral, TypedNode};

pub struct LLVMCompiler2<'a> {
    project: &'a Project,
    context: &'a Context,
    builder: Builder<'a>,
    main_module: Module<'a>,
    abra_main_fn: FunctionValue<'a>,

    // cached for convenience
    string_type: StructType<'a>,
}

impl<'a> LLVMCompiler2<'a> {
    pub fn compile(project: &Project, out_dir: &PathBuf) -> PathBuf {
        let context = Context::create();
        let compiler = LLVMCompiler2::new(project, &context);
        compiler.generate(project);

        let llvm_module_out_file = out_dir.join("_main.ll");
        compiler.main_module.print_to_file(&llvm_module_out_file).unwrap();

        let exec_out_file = out_dir.join("main");
        let cc_output = Command::new("cc")
            .arg(&llvm_module_out_file)
            .arg("-o")
            .arg(&exec_out_file)
            .arg("-Wno-override-module")
            .output()
            .unwrap();
        if !cc_output.stderr.is_empty() {
            eprintln!("{}", String::from_utf8(cc_output.stderr).unwrap());
        }
        if !cc_output.stdout.is_empty() {
            print!("{}", String::from_utf8(cc_output.stdout).unwrap());
        }

        exec_out_file
    }

    pub fn compile_and_run(project: &Project, out_dir: &PathBuf) -> ExitStatus {
        let exec_out_file = Self::compile(project, out_dir);

        let run_output = Command::new(&exec_out_file).output().unwrap();
        if !run_output.stderr.is_empty() {
            eprintln!("Error: {}", String::from_utf8(run_output.stderr).unwrap());
        }
        if !run_output.stdout.is_empty() {
            print!("{}", String::from_utf8(run_output.stdout).unwrap());
        }
        run_output.status
    }

    fn new(project: &'a Project, context: &'a Context) -> Self {
        let builder = context.create_builder();
        let main_module = context.create_module("__main");

        let triple = TargetMachine::get_default_triple();
        main_module.set_triple(&triple);

        let abra_main_fn = Self::start_abra_main(&context, &main_module, &builder);

        let string_type = context.opaque_struct_type("String");
        string_type.set_body(&[
            context.i32_type().into(), // type_id
            // context.i32_type().into(), // bytesize
            context.i32_type().into(), // length
            context.i8_type().ptr_type(AddressSpace::Generic).into(), // bytes
        ], false);

        Self {
            project,
            context,
            builder,
            main_module,
            abra_main_fn,
            string_type,
        }
    }

    // LLVM UTILS START

    fn bool(&self) -> IntType<'a> {
        self.context.bool_type()
    }

    fn const_bool(&self, value: bool) -> IntValue<'a> {
        self.bool().const_int(if value { 1 } else { 0 }, false)
    }

    fn i8(&self) -> IntType<'a> {
        self.context.i8_type()
    }

    fn i32(&self) -> IntType<'a> {
        self.context.i32_type()
    }

    fn const_i32(&self, value: u64) -> IntValue<'a> {
        self.i32().const_int(value, false)
    }

    fn i64(&self) -> IntType<'a> {
        self.context.i64_type()
    }

    fn const_i64(&self, value: u64) -> IntValue<'a> {
        self.i64().const_int(value, false)
    }

    fn f64(&self) -> FloatType<'a> {
        self.context.f64_type()
    }

    fn const_f64(&self, value: f64) -> FloatValue<'a> {
        self.f64().const_float(value)
    }

    fn ptr<T: BasicType<'a>>(&self, t: T) -> PointerType<'a> {
        t.ptr_type(AddressSpace::Generic)
    }

    fn null_ptr(&self) -> PointerValue<'a> {
        self.ptr(self.i8()).const_null()
    }

    fn fn_type<T: BasicType<'a>>(&self, ret: T, param_types: &[BasicMetadataTypeEnum<'a>]) -> FunctionType<'a> {
        ret.fn_type(param_types, false)
    }

    fn fn_type_variadic<T: BasicType<'a>>(&self, ret: T, param_types: &[BasicMetadataTypeEnum<'a>]) -> FunctionType<'a> {
        ret.fn_type(param_types, true)
    }

    // LLVM UTILS END

    fn start_abra_main(context: &'a Context, main_module: &Module<'a>, builder: &Builder<'a>) -> FunctionValue<'a> {
        let argc_t = context.i32_type();
        let argv_t = context.i8_type().ptr_type(AddressSpace::Generic).ptr_type(AddressSpace::Generic);
        let fn_type = context.void_type().fn_type(&[argc_t.into(), argv_t.into()], false);
        let abra_main_fn = main_module.add_function("_abra_main", fn_type, None);
        let block = context.append_basic_block(abra_main_fn, "");
        builder.position_at_end(block);

        abra_main_fn
    }

    fn end_abra_main(&self) {
        let b = self.abra_main_fn.get_last_basic_block().expect("abra main is guaranteed to have at least 1 block");
        self.builder.position_at_end(b);
        self.builder.build_return(None);
    }

    fn build_main_fn(&self) {
        let argc_t = self.i32();
        let argv_t = self.ptr(self.ptr(self.i8()));
        let main_fn_type = self.fn_type(self.i32(), &[argc_t.into(), argv_t.into()]);
        let entry_fn = self.main_module.add_function("main", main_fn_type, None);
        let block = self.context.append_basic_block(entry_fn, "");
        self.builder.position_at_end(block);
        self.builder.build_call(self.abra_main_fn, &[entry_fn.get_nth_param(0).unwrap().into(), entry_fn.get_nth_param(1).unwrap().into()], "");
        self.builder.build_return(Some(&self.const_i32(0).as_basic_value_enum()));
    }

    pub fn generate(&self, project: &Project) {
        self.generate_prelude();
        self.build_main_fn();

        let printf = self.main_module.add_function("printf", self.fn_type_variadic(self.i64(), &[self.ptr(self.i8()).into()]), None);

        for m in &project.modules {
            if m.id == PRELUDE_MODULE_ID { continue; }

            // The top-level code in a module is executed in the special $mod_{mod_id} fn...
            let mod_fn_name = format!("$mod_{}", m.id.0);
            let mod_fn_type = self.fn_type(self.bool(), &[]);
            let mod_fn = self.main_module.add_function(&mod_fn_name, mod_fn_type, None);
            let block = self.context.append_basic_block(mod_fn, "");
            self.builder.position_at_end(block);

            let num_nodes = m.code.len();
            for (idx, node) in m.code.iter().enumerate() {
                let res = self.visit_statement(node);
                if idx == num_nodes - 1 && *node.type_id() != PRELUDE_UNIT_TYPE_ID {
                    if let Some(res) = res {
                        let to_string_fn = self.main_module.get_function("Int#toString(self):String").unwrap();

                        let str_val = self.builder.build_call(to_string_fn, &[res.into()], "").try_as_basic_value().left().unwrap().into_pointer_value();
                        let chars_val = self.builder.build_struct_gep(str_val, 2, "String#chars").unwrap();

                        let fmt_str = self.builder.build_global_string_ptr("%s", "").as_basic_value_enum();
                        self.builder.build_call(printf, &[fmt_str.into(), chars_val.into()], "").try_as_basic_value().left().unwrap();
                    }
                }
            }

            // ...which returns true/false depending on whether it succeeded. For now, it will always
            // succeed, but when `try` is introduced, top-level code may fail.
            self.builder.build_return(Some(&self.const_bool(true)));

            // Call the $mod_{mod_id} fn in the _abra_main fn.
            self.builder.position_at_end(self.abra_main_fn.get_last_basic_block().unwrap());
            self.builder.build_call(mod_fn, &[], "");
        }

        self.end_abra_main();
    }

    // TODO: This function is only for initial bootstrapping purposes and will be replaced by a real prelude implementation in Abra
    fn generate_prelude(&self) {
        let snprintf = self.main_module.add_function("snprintf", self.fn_type_variadic(self.i64(), &[self.ptr(self.i8()).into(), self.i32().into(), self.ptr(self.i8()).into()]), None);
        let malloc = self.main_module.add_function("malloc", self.fn_type(self.ptr(self.i8()), &[self.i64().into()]), None);

        // Int#toString(self):String
        let fn_type = self.fn_type(self.ptr(self.string_type), &[self.i64().into()]);
        let func = self.main_module.add_function("Int#toString(self):String", fn_type, None);
        let block = self.context.append_basic_block(func, "");
        self.builder.position_at_end(block);
        let fmt_str = self.builder.build_global_string_ptr("%jd", "").as_basic_value_enum();
        let self_param = func.get_nth_param(0).unwrap();
        let len_val = self.builder.build_call(snprintf, &[self.null_ptr().into(), self.const_i32(0).into(), fmt_str.into(), self_param.into()], "len").try_as_basic_value().left().unwrap().into_int_value();
        let len_plus_1 = self.builder.build_int_add::<IntValue<'a>>(len_val.into(), self.const_i64(1).into(), "len_plus_1");
        let str_val = self.builder.build_call(malloc, &[len_plus_1.into()], "str").try_as_basic_value().left().unwrap();
        let len_val_plus_1_i32 = self.builder.build_int_cast(len_plus_1, self.i32(), "");
        self.builder.build_call(snprintf, &[str_val.into(), len_val_plus_1_i32.into(), fmt_str.into(), self_param.into()], "").try_as_basic_value().left().unwrap();

        let sizeof_string = unsafe { self.builder.build_gep(self.null_ptr().const_cast(self.ptr(self.string_type)), &[self.const_i32(1).into()], "sizeof(String)") };
        let sizeof_string = self.builder.build_ptr_to_int(sizeof_string, self.i64(), "sizeof(String) as i64");
        let str_mem = self.builder.build_pointer_cast(
            self.builder.build_call(malloc, &[sizeof_string.into()], "").try_as_basic_value().left().unwrap().into_pointer_value(),
            self.ptr(self.string_type),
            "str_mem",
        );
        // let str_mem = self.builder.build_pointer_cast(str_mem, self.ptr(self.string_type), "str_mem");
        let mem_cursor = unsafe { self.builder.build_struct_gep(str_mem, 0, "String#type_id") }.unwrap();
        self.builder.build_store(mem_cursor, self.const_i32(self.project.prelude_string_struct_id.1 as u64));
        let mem_cursor = unsafe { self.builder.build_struct_gep(str_mem, 1, "String#length") }.unwrap();
        self.builder.build_store(mem_cursor, self.builder.build_int_cast::<IntValue<'a>>(len_val.into(), self.i32(), ""));
        let mem_cursor = unsafe { self.builder.build_struct_gep(str_mem, 2, "String#chars") }.unwrap();
        self.builder.build_store(mem_cursor, str_val);

        // let ret_val = self.context.const_struct(&[
        //     self.const_i32(self.project.prelude_string_struct_id.1 as u64).into(),
        //     self.builder.build_int_cast::<IntValue<'a>>(len_val.into(), self.i32(), "").into(),
        //     str_val.into()
        // ], false);
        self.builder.build_return(Some(&str_mem.as_basic_value_enum()));
    }

    fn visit_statement(&self, node: &TypedNode) -> Option<BasicValueEnum<'a>> {
        match node {
            TypedNode::If { .. } |
            TypedNode::Match { .. } |
            TypedNode::FuncDeclaration(_) |
            TypedNode::TypeDeclaration(_) |
            TypedNode::EnumDeclaration(_) |
            TypedNode::BindingDeclaration { .. } |
            TypedNode::ForLoop { .. } |
            TypedNode::WhileLoop { .. } |
            TypedNode::Break { .. } |
            TypedNode::Continue { .. } |
            TypedNode::Return { .. } |
            TypedNode::Import { .. } => None,
            _ => Some(self.visit_expression(node)),
        }
    }

    fn visit_expression(&self, node: &TypedNode) -> BasicValueEnum<'a> {
        match node {
            TypedNode::Literal { value, .. } => {
                match value {
                    TypedLiteral::Int(v) => {
                        let int = self.const_i64((*v) as u64);
                        int.as_basic_value_enum()
                    }
                    TypedLiteral::Float(f) => {
                        let float = self.const_f64(*f);
                        float.as_basic_value_enum()
                    }
                    TypedLiteral::Bool(b) => {
                        let b = self.const_bool(*b);
                        b.as_basic_value_enum()
                    }
                    TypedLiteral::String(_) => todo!()
                }
            }
            TypedNode::Unary { .. } |
            TypedNode::Binary { .. } |
            TypedNode::Grouped { .. } |
            TypedNode::Array { .. } |
            TypedNode::Tuple { .. } |
            TypedNode::Set { .. } |
            TypedNode::Map { .. } |
            TypedNode::Identifier { .. } |
            TypedNode::NoneValue { .. } |
            TypedNode::Invocation { .. } |
            TypedNode::Accessor { .. } |
            TypedNode::Indexing { .. } |
            TypedNode::Lambda { .. } |
            TypedNode::Assignment { .. } => todo!(),
            _ => unreachable!("Node is not an expression and should have been handled in visit_statement")
        }
    }
}
