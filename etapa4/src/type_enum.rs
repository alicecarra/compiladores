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
