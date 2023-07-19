use std::collections::HashMap;

use cfgrammar::Span;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;

use crate::errors::ParsingError;

#[derive(Debug, Clone)]
pub enum SymbolEntry {
    LiteralInteger(IntegerSymbol),
    LiteralFloat(FloatSymbol),
    LiteralBoolean(BoolSymbol),
    Variable(CommonAttrs),
    Function(SymbolFn),
    None,
}

impl SymbolEntry {
    pub fn type_to_str(&self) -> &'static str {
        match self {
            SymbolEntry::LiteralInteger(_) => "literal int",
            SymbolEntry::LiteralFloat(_) => "literal float",
            SymbolEntry::LiteralBoolean(_) => "literal bool",
            SymbolEntry::Variable(_) => "variable",
            SymbolEntry::Function(_) => "function",
            SymbolEntry::None => "NONE",
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            SymbolEntry::Variable(symbol) => symbol.ty.clone(),
            SymbolEntry::Function(symbol) => symbol.common.ty.clone(),
            SymbolEntry::LiteralInteger(_) => Type::INT,
            SymbolEntry::LiteralFloat(_) => Type::FLOAT,
            SymbolEntry::LiteralBoolean(_) => Type::BOOL,
            SymbolEntry::None => Type::UNKNOWN,
        }
    }

    pub fn get_line_col(&self) -> (usize, usize) {
        match self {
            SymbolEntry::LiteralInteger(content) => (content.line, content.col),
            SymbolEntry::LiteralFloat(content) => (content.line, content.col),
            SymbolEntry::LiteralBoolean(content) => (content.line, content.col),
            SymbolEntry::Variable(content) => (content.line, content.col),
            SymbolEntry::Function(content) => (content.common.line, content.common.col),
            SymbolEntry::None => (0, 0),
        }
    }

    pub fn from_untyped_var(variable: UntypedVar, ty: Type) -> Self {
        SymbolEntry::Variable(CommonAttrs {
            line: variable.line,
            col: variable.col,
            size: ty.get_size(),
            val: variable.name,
            ty,
        })
    }

    pub fn is_literal(&self) -> bool {
        !matches!(self, SymbolEntry::Variable(_) | SymbolEntry::Function(_))
    }

    pub fn from_lit_span(span: Span, lexer: &dyn NonStreamingLexer<DefaultLexerTypes>) -> Self {
        let str = lexer.span_str(span);
        let ((line, col), _) = lexer.line_col(span);
        if let Ok(lit) = str.parse::<u32>() {
            Self::LiteralInteger(IntegerSymbol {
                line,
                col,
                size: Type::INT.get_size(),
                val: lit,
            })
        } else if let Ok(lit) = str.parse::<f64>() {
            Self::LiteralFloat(FloatSymbol {
                line,
                col,
                size: Type::FLOAT.get_size(),
                val: lit,
                val_string: str.to_string(),
            })
        } else {
            let lit = str.parse::<bool>().unwrap();
            Self::LiteralBoolean(BoolSymbol {
                line,
                col,
                size: Type::BOOL.get_size(),
                val: lit,
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct IntegerSymbol {
    pub line: usize,
    pub col: usize,
    pub size: u32,
    pub val: u32,
}

#[derive(Debug, Clone)]
pub struct FloatSymbol {
    pub line: usize,
    pub col: usize,
    pub size: u32,
    pub val: f64,
    pub val_string: String,
}

#[derive(Debug, Clone)]
pub struct BoolSymbol {
    pub line: usize,
    pub col: usize,
    pub size: u32,
    pub val: bool,
}

#[derive(Debug, Clone)]
pub struct CommonAttrs {
    pub line: usize,
    pub col: usize,
    pub size: u32,
    pub val: String,
    pub ty: Type,
}

impl CommonAttrs {
    pub fn new(
        name: String,
        ty: Type,
        span: Span,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Self {
        let ((line, col), _) = lexer.line_col(span);
        let val = name;
        let size = ty.get_size();
        Self {
            line,
            col,
            size,
            val,
            ty,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolFn {
    pub common: CommonAttrs,
    pub args: Option<Vec<SymbolEntry>>,
}

impl SymbolFn {
    pub fn new(
        name: String,
        ty: Type,
        span: Span,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
        args: Option<Vec<SymbolEntry>>,
    ) -> Self {
        let mut common = CommonAttrs::new(name, ty, span, lexer);
        common.size = 0;
        Self { common, args }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    INT,
    FLOAT,
    BOOL,
    UNKNOWN,
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::INT => "int".to_owned(),
            Type::FLOAT => "float".to_owned(),
            Type::BOOL => "bool".to_owned(),
            Type::UNKNOWN => "UNKNOWN".to_owned(),
        }
    }
}

impl Type {
    pub fn get_size(&self) -> u32 {
        match self {
            Type::INT => 4,
            Type::FLOAT => 8,
            Type::BOOL => 1,
            Type::UNKNOWN => 0,
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable(pub HashMap<String, SymbolEntry>);

impl SymbolTable {
    pub fn new() -> Self {
        Self(HashMap::default())
    }

    pub fn add_symbol(&mut self, key: String, symbol: SymbolEntry) -> Result<(), ParsingError> {
        if let Some(declared) = self.0.get(&key) {
            if declared.is_literal() {
                return Ok(());
            }

            let (s_line, s_col) = symbol.get_line_col();
            let (line, col) = declared.get_line_col();
            return Err(ParsingError::DeclaredError(format!(
                "{} at line {s_line}, col {s_col}: {} with identifier \"{key}\" was first declared at line {}, col {}.",
                symbol.type_to_str(), declared.type_to_str(), line, col
            )));
        }

        self.0.insert(key, symbol);

        Ok(())
    }

    pub fn get(&self, key: &String) -> Option<&SymbolEntry> {
        self.0.get(key)
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

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
