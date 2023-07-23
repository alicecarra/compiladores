use std::cell::RefCell;

use cfgrammar::Span;
use errors::ParsingError;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;
use scope_vec::ScopeVec;
use symbol_table::SymbolEntry;

pub mod ast;
pub mod errors;
pub mod scope_vec;
pub mod symbol_table;
pub mod type_enum;
pub mod untyped;

thread_local!(pub static SCOPE_VEC: RefCell<ScopeVec> = RefCell::new(ScopeVec::new()));

pub fn new_scope() {
    SCOPE_VEC.with(|stack| stack.borrow_mut().new_scope());
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
