use crate::lexer_test_utils::exec_test;

#[test]
fn test_ints() {
    exec_test("lexer/ints.abra");
    exec_test("lexer/ints_error_invalid_hex_first.abra");
    exec_test("lexer/ints_error_invalid_hex_eof.abra");
    exec_test("lexer/ints_error_invalid_binary_first.abra");
    exec_test("lexer/ints_error_invalid_binary_eof.abra");
}
