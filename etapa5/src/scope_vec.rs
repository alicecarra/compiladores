use cfgrammar::Span;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;

use crate::{
    errors::ParsingError,
    symbol_table::{ScopeType, SymbolEntry, SymbolTable},
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
            "Identificador: ({key}) em ({line},{col}) não foi declarado."
        )))
    }

    pub fn new_scope(&mut self, scope_type: ScopeType) {
        let mut table = SymbolTable::new(scope_type.clone());
        match scope_type {
            ScopeType::Inner => table.change_base_offset(self.0.last().unwrap().offset),
            _ => (),
        }
        self.0.push(table);
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

    pub fn get_function_size(&self, name: String) -> Result<u32, ParsingError> {
        let global_table = self.0.first().ok_or(ParsingError::NoScope)?;
        let function_table = global_table
            .children
            .iter()
            .filter(|table| table.name == Some(name.clone()))
            .collect::<Vec<_>>();
        let function_table = function_table
            .first()
            .ok_or(ParsingError::UndeclaredError(name))?;
        Ok(function_table.get_size())
    }

    pub fn is_global(&self, symbol: &SymbolEntry) -> bool {
        for table in self.0.iter().rev() {
            match table.get(&symbol.get_key()) {
                Some(_) => match table.scope_type {
                    ScopeType::Global => return true,
                    ScopeType::Function => return false,
                    ScopeType::Inner => return false,
                },
                None => continue,
            }
        }
        false
    }

    pub fn get_variable_offset(
        &self,
        function_name: String,
    ) -> Result<Vec<(String, u32)>, ParsingError> {
        let mut deslocs = vec![];
        if let Some(global_table) = self.0.first() {
            let function_symbol = global_table
                .get(&function_name)
                .ok_or(ParsingError::UndeclaredError(function_name.clone()))?;

            if let Some(args) = match function_symbol {
                SymbolEntry::Function(symbol) => symbol.args.clone(),
                _ => panic!("Erro extremo ao buscar entrada de símbolo da função."),
            } {
                let function_table = global_table
                    .children
                    .iter()
                    .filter(|table| table.name == Some(function_name.clone()))
                    .collect::<Vec<_>>();
                let function_table = function_table
                    .first()
                    .ok_or(ParsingError::UndeclaredError(function_name))?;

                args.iter().for_each(|arg| {
                    let key = arg.get_key();
                    let symbol = function_table.get(&key).unwrap();
                    deslocs.push((key, symbol.offset()));
                });
            }
        }

        Ok(deslocs)
    }
}

impl Default for ScopeVec {
    fn default() -> Self {
        Self::new()
    }
}
