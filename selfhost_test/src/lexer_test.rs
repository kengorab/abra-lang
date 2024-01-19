use crate::lexer_test_utils::TestRunner;

#[test]
fn lexer_tests() {
    TestRunner::lexer_test_runner()
        // Ints
        .add_test("lexer/ints.abra")
        .add_test("lexer/ints_error_leading_zero.abra")
        .add_test("lexer/ints_error_invalid_hex_first.abra")
        .add_test("lexer/ints_error_invalid_hex_eof.abra")
        .add_test("lexer/ints_error_invalid_binary_first.abra")
        .add_test("lexer/ints_error_invalid_binary_eof.abra")
        // Floats
        .add_test("lexer/floats.abra")
        .add_test("lexer/floats_error_extra_period.abra")
        // Strings
        .add_test("lexer/strings.abra")
        .add_test("lexer/strings_error_unterminated_newline.abra")
        .add_test("lexer/strings_error_unterminated_eof.abra")
        .add_test("lexer/strings_error_unsupported_escape_sequence.abra")
        .add_test("lexer/strings_error_invalid_unicode_seq_length.abra")
        .add_test("lexer/strings_error_invalid_unicode_seq_eof.abra")
        .add_test("lexer/strings_error_invalid_unicode_seq_char.abra")
        .run_tests();
}
