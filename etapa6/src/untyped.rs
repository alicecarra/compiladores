use cfgrammar::Span;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;

use crate::{
    ast::ASTNode, errors::ParsingError, get_symbol, symbol_table::SymbolEntry, type_enum::Type,
};

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UsageType {
    Variable,
    FunctionCall,
}

impl ToString for UsageType {
    fn to_string(&self) -> String {
        match self {
            UsageType::Variable => "variable".to_owned(),
            UsageType::FunctionCall => "function".to_owned(),
        }
    }
}

pub fn check_declaration(
    identifier: &ASTNode,
    lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    usage: UsageType,
) -> Result<SymbolEntry, ParsingError> {
    let symbol = get_symbol(identifier.span()?, lexer)?;
    match symbol.clone() {
        SymbolEntry::Variable(content) if usage != UsageType::Variable => {
            let declaration_line = content.line;
            let declaration_col = content.col;
            let ((usage_line, usage_col), _) = lexer.line_col(identifier.span()?);

            Err(ParsingError::VariableError(format!(
                "Tentando utilizar identificador \"{}\" como {} em {},{}. O identficador foi declarado em {},{}.",
                content.value,
                usage.to_string(),
                usage_line,
                usage_col,
                declaration_line,
                declaration_col
            )))
        }
        SymbolEntry::Function(content) if usage != UsageType::FunctionCall => {
            let declaration_line = content.common.line;
            let declaration_col = content.common.col;
            let ((usage_line, usage_col), _) = lexer.line_col(identifier.span()?);

            Err(ParsingError::FunctionError(format!(
                "Tentando utilizar identificador \"{}\" como {} em {},{}. O identficador foi declarado em {},{}.",
                content.common.value,
                usage.to_string(),
                usage_line,
                usage_col,
                declaration_line,
                declaration_col
            )))
        }
        _ => Ok(symbol),
    }
}

#[derive(Debug)]
pub struct LocalDeclrAux {
    pub variables: Vec<UntypedVar>,
    pub node: ASTNode,
}

impl LocalDeclrAux {
    pub fn new(variables: Vec<UntypedVar>, node: ASTNode) -> Self {
        Self { variables, node }
    }
    pub fn with_vars(variables: Vec<UntypedVar>) -> Self {
        Self {
            variables,
            node: ASTNode::None(Vec::new()),
        }
    }
}
