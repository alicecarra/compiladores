use cfgrammar::Span;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;

use crate::{
    errors::ParsingError,
    symbol_table::{SymbolEntry, SymbolTable},
};

#[derive(Debug)]
#[repr(transparent)]

pub struct ScopeVec(pub Vec<SymbolTable>);

impl ScopeVec {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn get_symbol(
        &self,
        span: Span,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<SymbolEntry, ParsingError> {
        let key = lexer.span_str(span).to_string();
        for table in self.0.iter().rev() {
            match table.get(&key) {
                Some(symbol) => return Ok(symbol.clone()),
                None => continue,
            }
        }
        let ((line, col), _) = lexer.line_col(span);

        Err(ParsingError::UndeclaredError(format!(
            "Identificador: \"{key}\" em {line}, {col} não foi declarado."
        )))
    }

    pub fn new_scope(&mut self) {
        self.0.push(SymbolTable::new());
    }

    pub fn pop_scope(&mut self) {
        self.0.pop();
    }

    pub fn add_symbol(&mut self, symbol: SymbolEntry) -> Result<(), ParsingError> {
        let scope_table = self.0.last_mut().ok_or(ParsingError::NoScope)?;
        match &symbol {
            SymbolEntry::LiteralInteger(content) => {
                let key = content.val.to_string();
                scope_table.add_symbol(key, symbol)?;
            }
            SymbolEntry::LiteralFloat(content) => {
                let key = content.val.to_string();
                scope_table.add_symbol(key, symbol)?;
            }
            SymbolEntry::LiteralBoolean(content) => {
                let key = content.val.to_string();
                scope_table.add_symbol(key, symbol)?;
            }
            SymbolEntry::Variable(content) => {
                scope_table.add_symbol(content.val.clone(), symbol)?
            }
            SymbolEntry::Function(content) => {
                scope_table.add_symbol(content.common.val.clone(), symbol)?
            }
            SymbolEntry::None => return Ok(()),
        }
        Ok(())
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }
}

impl Default for ScopeVec {
    fn default() -> Self {
        Self::new()
    }
}
