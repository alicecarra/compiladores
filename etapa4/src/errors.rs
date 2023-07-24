#[derive(Debug, thiserror::Error)]
pub enum ParsingError {
    #[error("Tentativa de uso de variável não declarada. {0}")]
    UndeclaredError(String),
    #[error("Tentativa de declarar variável já declarada. {0}")]
    DeclaredError(String),
    #[error("{0}")]
    VariableError(String),
    #[error("{0}")]
    FunctionError(String),

    #[error("Erro de span: {0}")]
    SpanError(String),
    #[error("Add next realizado em none. {0}")]
    AddNextToNone(String),
    #[error("Erro de tipo desconhecido. {0}")]
    UnknownTypeInference(String),
    #[error("Sem escopo definido.")]
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
