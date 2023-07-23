use cfgrammar::Span;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;

use crate::{errors::ParsingError, type_enum::Type};

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

pub fn try_type_inference(first_type: Type, second_type: Type) -> Result<Type, ParsingError> {
    if first_type == second_type {
        return Ok(first_type);
    }

    match first_type {
        Type::INT => match second_type {
            Type::INT => Ok(first_type), //Esse pattern deve nunca acontecer.
            Type::FLOAT => Ok(second_type),
            Type::BOOL => Ok(first_type),
            Type::UNKNOWN => Err(ParsingError::UnknownTypeInference(
                "Isso não deve acontecer".to_string(),
            )),
        },
        Type::FLOAT => match second_type {
            Type::UNKNOWN => Err(ParsingError::UnknownTypeInference(
                "Isso não deve acontecer".to_string(),
            )),
            _ => Ok(first_type),
        },
        Type::BOOL => match second_type {
            Type::INT => Ok(second_type),
            Type::FLOAT => Ok(second_type),
            Type::BOOL => Ok(first_type), //Esse pattern deve nunca acontecer.
            Type::UNKNOWN => Err(ParsingError::UnknownTypeInference(
                "Isso não deve acontecer".to_string(),
            )),
        },
        Type::UNKNOWN => Err(ParsingError::UnknownTypeInference(
            "Isso não deve acontecer".to_string(),
        )),
    }
}
