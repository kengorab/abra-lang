// Tests for the vm are in this file rather than in the vm file; that file is going to get massive
// and as will the tests, so it made sense I thought to split them out into a separate file.

#[cfg(test)]
mod tests {
    use crate::lexer::lexer::tokenize;
    use crate::parser::parser::parse;
    use crate::typechecker::typechecker::typecheck;
    use crate::vm::compiler::compile;
    use crate::vm::value::{Value, Obj};
    use crate::vm::vm::VM;

    fn interpret(input: &str) -> Option<Value> {
        let tokens = tokenize(&input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        let typed_ast = typecheck(ast).unwrap();
        let chunk = compile(typed_ast).unwrap();

        let mut vm = VM::new(&chunk);
        vm.run().unwrap()
    }

    #[test]
    fn interpret_nothing() {
        assert!(interpret("").is_none());
    }

    #[test]
    fn interpret_constants() {
        let result = interpret("1").unwrap();
        let expected = Value::Int(1);
        assert_eq!(expected, result);

        let result = interpret("1.23").unwrap();
        let expected = Value::Float(1.23);
        assert_eq!(expected, result);

        let result = interpret("true").unwrap();
        let expected = Value::Bool(true);
        assert_eq!(expected, result);

        let result = interpret("false").unwrap();
        let expected = Value::Bool(false);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_unary() {
        let result = interpret("-1").unwrap();
        let expected = Value::Int(-1);
        assert_eq!(expected, result);

        let result = interpret("-1.23").unwrap();
        let expected = Value::Float(-1.23);
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_binary() {
        let result = interpret("1 + 2 * 3.4 / 5").unwrap();
        let expected = Value::Float(2.36);
        assert_eq!(expected, result);

        let result = interpret("\"hello\" +  \" \"+24  + \" world\"").unwrap();
        let expected = Value::Obj(Obj::StringObj { value: Box::new("hello 24 world".to_string()) });
        assert_eq!(expected, result);
    }

    #[test]
    fn interpret_binary_boolean() {
        let result = interpret("true || false").unwrap();
        let expected = Value::Bool(true);
        assert_eq!(expected, result);

        let result = interpret("true && false").unwrap();
        let expected = Value::Bool(false);
        assert_eq!(expected, result);

        let result = interpret("true && false || true && true").unwrap();
        let expected = Value::Bool(true);
        assert_eq!(expected, result);
    }
}