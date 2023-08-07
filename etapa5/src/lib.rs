use std::cell::RefCell;

use cfgrammar::Span;
use errors::ParsingError;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;
use scope_vec::ScopeVec;
use symbol_table::{check_global, ScopeType, SymbolEntry};

pub mod ast;
pub mod errors;
pub mod iloc;
pub mod scope_vec;
pub mod symbol_table;
pub mod type_enum;
pub mod untyped;

thread_local!(pub static SCOPE_VEC: RefCell<ScopeVec> = RefCell::new(ScopeVec::new()));
thread_local!(pub static TEMP_COUNTER: RefCell<u32> = RefCell::new(0));
thread_local!(pub static LABEL_COUNTER: RefCell<u32> = RefCell::new(0));

pub fn new_scope(scope_type: ScopeType) {
    SCOPE_VEC.with(|stack| stack.borrow_mut().new_scope(scope_type));
}

pub fn end_scope() {
    SCOPE_VEC.with(|stack| stack.borrow_mut().pop_scope());
}

pub fn get_symbol(
    _span: Span,
    _lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
) -> Result<SymbolEntry, ParsingError> {
    SCOPE_VEC.with(|stack| stack.borrow().get_symbol(_span, _lexer))
}

pub fn clear_stack() {
    SCOPE_VEC.with(|stack| stack.borrow_mut().clear());
}

pub fn add_symbol(_symbol_entry: SymbolEntry) -> Result<(), ParsingError> {
    SCOPE_VEC.with(|stack| stack.borrow_mut().add_symbol(_symbol_entry))?;
    Ok(())
}

pub fn get_temporary() -> String {
    let temp_val = TEMP_COUNTER.with(|counter| counter.borrow().clone());
    TEMP_COUNTER.with(|counter| *counter.borrow_mut() += 1);

    format!("r{temp_val}")
}

pub fn get_function_label(name: String) -> Result<String, ParsingError> {
    return SCOPE_VEC.with(|stack| {
        let label = stack
            .borrow()
            .0
            .first()
            .ok_or(ParsingError::NoScope)?
            .get(&name)
            .ok_or(ParsingError::UndeclaredError(name))?
            .get_label();
        Ok(label)
    });
}

pub fn get_function_size(name: String) -> Result<u32, ParsingError> {
    return SCOPE_VEC.with(|stack| stack.borrow_mut().get_function_size(name));
}

pub fn get_register(symbol: &SymbolEntry) -> String {
    match check_global(symbol) {
        true => "rbss".to_string(),
        false => "rfp".to_string(),
    }
}

pub fn get_variable_offset(name: String) -> Result<Vec<(String, u32)>, ParsingError> {
    return SCOPE_VEC.with(|stack| stack.borrow_mut().get_variable_offset(name));
}

pub fn get_new_label() -> String {
    let temp_val = LABEL_COUNTER.with(|counter| counter.borrow().clone());
    LABEL_COUNTER.with(|counter| *counter.borrow_mut() += 1);

    format!("L{temp_val}")
}
