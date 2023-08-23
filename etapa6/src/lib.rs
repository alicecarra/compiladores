use std::cell::RefCell;

use cfgrammar::Span;
use errors::ParsingError;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;
use scope_vec::ScopeVec;
use symbol_table::{ScopeType, SymbolEntry};

pub mod asm;
pub mod ast;
pub mod errors;
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

pub fn add_symbol(_symbol_entry: SymbolEntry) -> Result<(), ParsingError> {
    SCOPE_VEC.with(|stack| stack.borrow_mut().add_symbol(_symbol_entry))?;
    Ok(())
}

pub fn get_temporary() -> String {
    let temp_val = TEMP_COUNTER.with(|counter| *counter.borrow());
    TEMP_COUNTER.with(|counter| *counter.borrow_mut() += 1);

    format!("r{temp_val}")
}

pub fn get_new_temporary() -> Result<String, ParsingError> {
    SCOPE_VEC.with(|stack| stack.borrow_mut().get_register())
}

pub fn add_name_to_last_symbol_table(name: String) {
    SCOPE_VEC.with(|stack| stack.borrow_mut().add_name_to_last_symbol_table(name));
}

pub fn get_new_label() -> String {
    let temp_val = LABEL_COUNTER.with(|counter| *counter.borrow());
    LABEL_COUNTER.with(|counter| *counter.borrow_mut() += 1);

    format!("L{temp_val}")
}

pub fn free_registers_temporary() -> Vec<&'static str> {
    vec![
        "%ebx", "%ecx", "%r8d", "%r9d", "%r10d", "%r11d", "%r12d", "%r13d", "%r14d", "%r15d",
        "%esi", "%edi",
    ]
}
