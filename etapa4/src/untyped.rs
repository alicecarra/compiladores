use cfgrammar::Span;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;

#[derive(Debug)]
pub struct UntypedVar {
    pub line: usize,
    pub col: usize,
    pub name: String,
}

impl UntypedVar {
    pub fn new(span: Span, lexer: &dyn NonStreamingLexer<DefaultLexerTypes>) -> Self {
        let ((line, col), _) = lexer.line_col(span);
        let name = lexer.span_str(span).to_string();
        Self { line, col, name }
    }
}
