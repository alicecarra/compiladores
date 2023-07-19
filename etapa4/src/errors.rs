#[derive(Debug, thiserror::Error)]
pub enum ParsingError {
    #[error("Attempted use of undeclared variable. {0}")]
    UndeclaredError(String),
    #[error("Attempting to declare an already declared {0}")]
    DeclaredError(String),
    #[error("{0}")]
    VariableError(String),
    #[error("{0}")]
    FunctionError(String),

    #[error("Span get error: {0}")]
    SpanError(String),
    #[error("Add next to None node error: {0}")]
    AddNextToNone(String),
    #[error("Coerse unkonwn type error: {0}")]
    CoerseUnknown(String),
    #[error("No scope defined")]
    NoScope,
}

impl ParsingError {
    pub fn to_err_code(&self) -> u8 {
        match self {
            ParsingError::UndeclaredError(_) => 10,
            ParsingError::DeclaredError(_) => 11,
            ParsingError::VariableError(_) => 20,
            ParsingError::FunctionError(_) => 21,
            _ => 1,
        }
    }
}
