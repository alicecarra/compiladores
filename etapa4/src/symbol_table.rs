use std::collections::HashMap;

use cfgrammar::Span;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;

use crate::{errors::ParsingError, type_enum::Type, untyped::UntypedVar};

#[derive(Debug, Clone)]
pub enum SymbolEntry {
    LiteralInteger(IntegerSymbol),
    LiteralFloat(FloatSymbol),
    LiteralBoolean(BoolSymbol),
    Variable(CommonAttrs),
    Function(FunctionSymbol),
    None,
}

impl SymbolEntry {
    pub fn type_to_str(&self) -> &'static str {
        match self {
            SymbolEntry::LiteralInteger(_) => "Integer literal",
            SymbolEntry::LiteralFloat(_) => "Float literal",
            SymbolEntry::LiteralBoolean(_) => "Boolean literal",
            SymbolEntry::Variable(_) => "Variable",
            SymbolEntry::Function(_) => "Function",
            SymbolEntry::None => "None :(",
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            SymbolEntry::Variable(symbol) => symbol.val_type.clone(),
            SymbolEntry::Function(symbol) => symbol.common.val_type.clone(),
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
            val_type: ty,
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
    pub val_type: Type,
}

impl CommonAttrs {
    pub fn new(
        name: String,
        val_type: Type,
        span: Span,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Self {
        let ((line, col), _) = lexer.line_col(span);
        let val = name;
        let size = val_type.get_size();
        Self {
            line,
            col,
            size,
            val,
            val_type,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    pub common: CommonAttrs,
    pub args: Option<Vec<SymbolEntry>>,
}

impl FunctionSymbol {
    pub fn new(
        name: String,
        function_type: Type,
        span: Span,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
        args: Option<Vec<SymbolEntry>>,
    ) -> Self {
        let mut common = CommonAttrs::new(name, function_type, span, lexer);
        common.size = 0;
        Self { common, args }
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
                "{} em {s_line}, col {s_col}: {} com identificador \"{key}\" foi declarado em linha {}, coluna {}.",
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
