use std::collections::HashMap;

use cfgrammar::Span;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;

use crate::{errors::ParsingError, type_enum::Type, untyped::UntypedVar, SCOPE_VEC};

#[derive(Debug, Clone)]
pub enum SymbolEntry {
    LiteralInteger(IntegerSymbol),
    LiteralFloat(FloatSymbol),
    LiteralBoolean(BoolSymbol),
    Variable(CommonAttrs),
    Function(FunctionSymbol),
    None,
}

pub fn check_global(symbol: &SymbolEntry) -> bool {
    SCOPE_VEC.with(|stack| stack.borrow().is_global(symbol))
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
            offset: 0_u32,
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

    pub fn get_label(&self) -> String {
        match self {
            SymbolEntry::Function(content) => content.label.clone(),
            symbol => {
                panic!("Somente funções possuem label. Tentando extrair label de: {symbol:#?}")
            }
        }
    }

    pub fn size(&self) -> u32 {
        match self {
            SymbolEntry::LiteralInteger(_) => 0,
            SymbolEntry::LiteralFloat(_) => 0,
            SymbolEntry::LiteralBoolean(_) => 0,
            SymbolEntry::Variable(var) => var.size,
            SymbolEntry::Function(_) => 0,
            SymbolEntry::None => 0,
        }
    }

    pub fn get_key(&self) -> String {
        match self {
            SymbolEntry::LiteralInteger(content) => content.val.to_string(),
            SymbolEntry::LiteralFloat(content) => content.val_string.clone(),
            SymbolEntry::LiteralBoolean(content) => content.val.to_string(),
            SymbolEntry::Variable(content) => content.val.to_string(),
            SymbolEntry::Function(content) => content.common.val.to_string(),
            SymbolEntry::None => "".to_string(),
        }
    }

    pub fn update_offset(&mut self, offset: u32) {
        if let SymbolEntry::Variable(var) = self {
            var.offset += offset;
        }
    }

    pub fn offset(&self) -> u32 {
        match self {
            SymbolEntry::LiteralInteger(_) => 0,
            SymbolEntry::LiteralFloat(_) => 0,
            SymbolEntry::LiteralBoolean(_) => 0,
            SymbolEntry::Variable(var) => var.offset,
            SymbolEntry::Function(_) => 0,
            SymbolEntry::None => 0,
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
    pub offset: u32,
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
            offset: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    pub common: CommonAttrs,
    pub args: Option<Vec<SymbolEntry>>,
    pub label: String,
}

impl FunctionSymbol {
    pub fn new(
        name: String,
        function_type: Type,
        span: Span,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
        args: Option<Vec<SymbolEntry>>,
        label: String,
    ) -> Self {
        let mut common = CommonAttrs::new(name, function_type, span, lexer);
        common.size = 0;
        Self {
            common,
            args,
            label,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ScopeType {
    Global,
    Function,
    Inner,
}

#[derive(Debug)]
pub struct SymbolTable {
    pub table: HashMap<String, SymbolEntry>,
    pub children: Vec<Box<SymbolTable>>,
    pub name: Option<String>,
    pub scope_type: ScopeType,
    pub offset: u32,
}

impl SymbolTable {
    pub fn new(scope_type: ScopeType) -> Self {
        Self {
            name: None,
            table: HashMap::default(),
            children: Vec::new(),
            scope_type,
            offset: 0_u32,
        }
    }

    pub fn change_base_offset(&mut self, offset: u32) {
        self.offset += offset;
        self.table = self
            .table
            .clone()
            .into_iter()
            .map(|(key, mut symbol)| {
                symbol.update_offset(offset);
                (key, symbol)
            })
            .collect();
    }

    pub fn add_symbol(&mut self, key: String, symbol: SymbolEntry) -> Result<(), ParsingError> {
        if let Some(declared) = self.table.get(&key) {
            if declared.is_literal() {
                return Ok(());
            }

            let (s_line, s_col) = symbol.get_line_col();
            let (line, col) = declared.get_line_col();
            return Err(ParsingError::DeclaredError(format!(
                "{} em ({s_line},{s_col}): {} com identificador ({key}) foi declarado em linha ({},{}).",
                symbol.type_to_str(), declared.type_to_str(), line, col
            )));
        }

        self.table.insert(key, symbol);

        Ok(())
    }

    pub fn add_name(&mut self, name: String) {
        self.name = Some(name);
    }

    pub fn get(&self, key: &String) -> Option<&SymbolEntry> {
        self.table.get(key)
    }

    pub fn add_child_table(&mut self, table: SymbolTable) {
        self.children.push(Box::new(table));
    }

    pub fn get_size(&self) -> u32 {
        let mut size = self
            .table
            .values()
            .map(|symbol| symbol.size())
            .reduce(|acc, size| acc + size)
            .unwrap_or(0);
        size = size.max(
            self.children
                .iter()
                .map(|table| table.get_size())
                .reduce(|acc, size| acc.max(size))
                .unwrap_or(0),
        );

        size
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new(ScopeType::Global)
    }
}
