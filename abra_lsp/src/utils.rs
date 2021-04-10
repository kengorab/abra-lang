use abra_core::common::display_error::DisplayError;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
use abra_core::parser::parse_error::ParseError;

pub fn abra_error_to_diagnostic(e: abra_core::Error, source: &String) -> Diagnostic {
    let range = match &e {
        abra_core::Error::LexerError(e) => e.get_range(),
        abra_core::Error::TypecheckerError(e) => e.get_token().get_range(),
        abra_core::Error::ParseError(e) => match e {
            ParseError::UnexpectedEof(range) => range.clone(),
            ParseError::UnexpectedToken(tok) |
            ParseError::ExpectedToken(_, tok) => tok.get_range()
        }
        abra_core::Error::InterpretError(_) => unreachable!()
    };

    let range = Range {
        start: Position { line: (range.start.line - 1) as u64, character: (range.start.col - 1) as u64 },
        end: Position { line: (range.end.line - 1) as u64, character: (range.end.col) as u64 },
    };

    Diagnostic {
        severity: Some(DiagnosticSeverity::Error),
        range,
        message: e.get_message(&source),
        ..Diagnostic::default()
    }
}
