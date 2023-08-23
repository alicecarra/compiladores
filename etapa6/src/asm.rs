use crate::symbol_table::SymbolEntry;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AssemblerInstruction {
    Arithmetic(FullOperator),
    Division(DivisionInstruction),
    MoveImediate(OneInputOneOutput),
    Move(Move),
    LoadOffset(LoadOffset),
    StoreImediate(OneInputOneOutput),
    StoreOffset(OneInputTwoOutput),
    CompareRegister(CompareRegister),
    Compare(CompareInstruction),
    Jump(Jump),
    StackInstruction(StackInstruction),
    Nop(Option<String>),
    SingleInstruction(Option<String>, String),
    Directive(Directive),
    Halt,
    Empty,
}

pub static RETURN_AND_RBP_OFFSET: u32 = 16;

impl AssemblerInstruction {
    pub fn add_arithmetic_instruction(&mut self, name: String) {
        match self {
            AssemblerInstruction::Arithmetic(op) => op.name = name,
            _ => panic!("Nomes não devem ser adicionados a instruções que não são aritméticas."),
        }
    }

    pub fn code_string(&self) -> String {
        match self {
            AssemblerInstruction::Arithmetic(op) => op.code_string(),
            AssemblerInstruction::Division(instruction) => instruction.code_string(),
            AssemblerInstruction::MoveImediate(op) => op.code_string(),
            AssemblerInstruction::Move(instruction) => instruction.code_string(),
            AssemblerInstruction::LoadOffset(op) => op.code_string(),
            AssemblerInstruction::StoreImediate(op) => op.code_string(),
            AssemblerInstruction::StoreOffset(op) => op.code_string(),
            AssemblerInstruction::CompareRegister(op) => op.code_str(),
            AssemblerInstruction::Compare(op) => op.code_string(),
            AssemblerInstruction::Jump(op) => op.code_string(),
            AssemblerInstruction::StackInstruction(instruction) => instruction.code_string(),
            AssemblerInstruction::Nop(label) => {
                let mut code_string = "".to_string();
                if let Some(label) = label {
                    code_string += &format!("{label}:\n");
                }
                code_string += "\tnop\n";
                code_string
            }
            AssemblerInstruction::SingleInstruction(label, name) => {
                let mut code_string = "".to_string();
                if let Some(label) = label {
                    code_string += &format!("{label}:\n");
                }
                code_string += &format!("\t{name}\n");
                code_string
            }
            AssemblerInstruction::Directive(directive) => directive.code_str(),
            AssemblerInstruction::Halt => "\thlt\n".to_string(),
            AssemblerInstruction::Empty => "".to_string(),
        }
    }

    pub fn add_label(self, label: String) -> Self {
        match self {
            AssemblerInstruction::Arithmetic(mut instruction) => {
                instruction.add_label(label);
                AssemblerInstruction::Arithmetic(instruction)
            }
            AssemblerInstruction::Division(_) => todo!(),
            AssemblerInstruction::MoveImediate(mut instruction) => {
                instruction.add_label(label);
                AssemblerInstruction::MoveImediate(instruction)
            }
            AssemblerInstruction::Move(mut instruction) => {
                instruction.add_label(label);
                AssemblerInstruction::Move(instruction)
            }
            AssemblerInstruction::LoadOffset(mut instruction) => {
                instruction.add_label(label);
                AssemblerInstruction::LoadOffset(instruction)
            }
            AssemblerInstruction::StoreImediate(mut instruction) => {
                instruction.add_label(label);
                AssemblerInstruction::StoreImediate(instruction)
            }
            AssemblerInstruction::StoreOffset(mut instruction) => {
                instruction.add_label(label);
                AssemblerInstruction::StoreOffset(instruction)
            }
            AssemblerInstruction::CompareRegister(mut instruction) => {
                instruction.add_label(label);
                AssemblerInstruction::CompareRegister(instruction)
            }
            AssemblerInstruction::Compare(mut instruction) => {
                instruction.add_label(label);
                AssemblerInstruction::Compare(instruction)
            }
            AssemblerInstruction::Jump(mut instruction) => {
                instruction.add_label(label);
                AssemblerInstruction::Jump(instruction)
            }
            AssemblerInstruction::StackInstruction(mut instruction) => {
                instruction.add_label(label);
                AssemblerInstruction::StackInstruction(instruction)
            }
            AssemblerInstruction::Nop(_) => AssemblerInstruction::Nop(Some(label)),
            AssemblerInstruction::SingleInstruction(_, name) => {
                AssemblerInstruction::SingleInstruction(Some(label), name)
            }
            AssemblerInstruction::Directive(mut dir) => {
                dir.add_label(label);
                AssemblerInstruction::Directive(dir)
            }
            AssemblerInstruction::Halt => AssemblerInstruction::Halt,
            AssemblerInstruction::Empty => AssemblerInstruction::Empty,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FullOperator {
    pub name: String,
    pub first_operand: String,
    pub second_operand: String,
    pub label: Option<String>,
}

impl FullOperator {
    pub fn new(name: String, first_operand: String, second_operand: String) -> Self {
        Self {
            name,
            first_operand,
            second_operand,
            label: None,
        }
    }

    pub fn code_string(&self) -> String {
        let mut code_str = "".to_string();
        if let Some(label) = &self.label {
            code_str += &format!("{label}:\n");
        }
        code_str += &format!(
            "\t{}\t{}, {}\n",
            self.name, self.first_operand, self.second_operand
        );
        code_str
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Directive {
    pub name: String,
    pub first_operand: Option<String>,
    pub second_operand: Option<String>,
    pub label: Option<String>,
}

impl Directive {
    pub fn new(
        name: String,
        first_operand: Option<String>,
        second_operand: Option<String>,
    ) -> Self {
        Self {
            name,
            first_operand,
            second_operand,
            label: None,
        }
    }

    pub fn code_str(&self) -> String {
        let mut code_str = "".to_string();
        if let Some(label) = &self.label {
            code_str += &format!("{label}:\n");
        }
        code_str += &format!("\t.{}", self.name);

        if let Some(op1) = &self.first_operand {
            code_str += &format!("\t{}", op1);
        }

        if let Some(op2) = &self.second_operand {
            code_str += &format!(", {}", op2);
        }
        code_str += "\n";
        code_str
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Move {
    pub name: String,
    pub source: String,
    pub destination: String,
    pub label: Option<String>,
}

impl Move {
    pub fn new(name: String, source: String, destination: String) -> Self {
        Self {
            name,
            source,
            destination,
            label: None,
        }
    }

    pub fn code_string(&self) -> String {
        let mut code_string = "".to_string();
        if let Some(label) = &self.label {
            code_string += &format!("{label}:\n");
        }
        code_string += &format!("\t{}\t{}, {}\n", self.name, self.source, self.destination);
        code_string
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CompareRegister {
    pub first_register: String,
    pub second_register: String,
    pub label: Option<String>,
}

impl CompareRegister {
    pub fn new(first_register: String, second_register: String) -> Self {
        Self {
            first_register,
            second_register,
            label: None,
        }
    }

    pub fn code_str(&self) -> String {
        let mut code_str = "".to_string();
        if let Some(label) = &self.label {
            code_str += &format!("{label}:\n");
        }
        code_str += &format!(
            "\tcmp\t\t{}, {}\n",
            self.first_register, self.second_register
        );
        code_str
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LoadOffset {
    pub name: String,
    pub offset: String,
    pub first_register: String,
    pub second_register: String,
    pub label: Option<String>,
}

impl LoadOffset {
    pub fn new(
        name: String,
        desl: String,
        first_register: String,
        second_register: String,
    ) -> Self {
        Self {
            name,
            offset: desl,
            first_register,
            second_register,
            label: None,
        }
    }

    pub fn code_string(&self) -> String {
        let mut code_string = "".to_string();
        if let Some(label) = &self.label {
            code_string += &format!("{label}:\n");
        }
        code_string += &format!(
            "\t{}\t{}({}), {}\n",
            self.name, self.offset, self.first_register, self.second_register
        );
        code_string
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CompareInstruction {
    pub name: String,
    pub destination: String,
    pub label: Option<String>,
}

impl CompareInstruction {
    pub fn new(name: String, destination: String) -> Self {
        Self {
            name,
            destination,
            label: None,
        }
    }

    pub fn code_string(&self) -> String {
        let mut code_string = "".to_string();
        if let Some(label) = &self.label {
            code_string += &format!("{label}:\n");
        }
        code_string += &format!("\t{}\t\t{}\n", self.name, self.destination);
        code_string
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct OneInputOneOutput {
    pub name: String,
    pub operand: String,
    pub destination: String,
    pub label: Option<String>,
}

impl OneInputOneOutput {
    pub fn new(name: String, operand: String, destination: String) -> Self {
        Self {
            name,
            operand,
            destination,
            label: None,
        }
    }

    pub fn code_string(&self) -> String {
        let mut code_string = "".to_string();
        if let Some(label) = &self.label {
            code_string += &format!("{label}:\n");
        }
        code_string += &format!("\t{}\t{}, {}\n", self.name, self.operand, self.destination);
        code_string
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct OneInputTwoOutput {
    pub name: String,
    pub operand: String,
    pub destination: String,
    pub offset: String,
    pub label: Option<String>,
}

impl OneInputTwoOutput {
    pub fn new(name: String, operand: String, destination: String, offset: String) -> Self {
        Self {
            name,
            operand,
            destination,
            offset,
            label: None,
        }
    }

    pub fn code_string(&self) -> String {
        let mut code_string = "".to_string();
        if let Some(label) = &self.label {
            code_string += &format!("{label}:\n");
        }
        code_string += &format!(
            "\t{}\t{}, {}({})\n",
            self.name, self.operand, self.offset, self.destination
        );
        code_string
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Jump {
    pub destination: String,
    pub label: Option<String>,
}

impl Jump {
    pub fn new(destination: String) -> Self {
        Self {
            destination,
            label: None,
        }
    }

    pub fn code_string(&self) -> String {
        let mut code_string = "".to_string();
        if let Some(label) = &self.label {
            code_string += &format!("{label}:\n");
        }
        code_string += &format!("\tjmp\t\t{}\n", self.destination);
        code_string
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Not {
    pub destination: String,
    pub label: Option<String>,
}

impl Not {
    pub fn new(destination: String) -> Self {
        Self {
            destination,
            label: None,
        }
    }

    pub fn code_string(&self) -> String {
        let mut code_string = "".to_string();
        if let Some(label) = &self.label {
            code_string += &format!("{label}:\n");
        }
        code_string += &format!("\tnot\t\t{}\n", self.destination);
        code_string
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DivisionInstruction {
    pub destination: String,
    pub label: Option<String>,
}

impl DivisionInstruction {
    pub fn new(destination: String) -> Self {
        Self {
            destination,
            label: None,
        }
    }

    pub fn code_string(&self) -> String {
        let mut code_string = "".to_string();
        if let Some(label) = &self.label {
            code_string += &format!("{label}:\n");
        }
        code_string += &format!("\tidiv\t{}\n", self.destination);
        code_string
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StackInstruction {
    pub name: String,
    pub destination: String,
    pub label: Option<String>,
}

impl StackInstruction {
    pub fn new(name: String, destination: String) -> Self {
        Self {
            name,
            destination,
            label: None,
        }
    }

    pub fn code_string(&self) -> String {
        let mut code_string = "".to_string();
        if let Some(label) = &self.label {
            code_string += &format!("{label}:\n");
        }
        code_string += &format!("\t{}\t{}\n", self.name, self.destination);
        code_string
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

pub fn generate_global_declaration_code(symbol: &SymbolEntry) -> Vec<AssemblerInstruction> {
    let mut code = vec![];

    let text_directive =
        AssemblerInstruction::Directive(Directive::new("text".to_string(), None, None));
    let globl_directive = AssemblerInstruction::Directive(Directive::new(
        "globl".to_string(),
        Some(symbol.name()),
        None,
    ));
    let bss_directive =
        AssemblerInstruction::Directive(Directive::new("bss".to_string(), None, None));
    let align_directive = AssemblerInstruction::Directive(Directive::new(
        "align".to_string(),
        Some(symbol.size().to_string()),
        None,
    ));
    let type_directive = AssemblerInstruction::Directive(Directive::new(
        "type".to_string(),
        Some(symbol.name()),
        Some("@object".to_string()),
    ));
    let size_directive = AssemblerInstruction::Directive(Directive::new(
        "size".to_string(),
        Some(symbol.name()),
        Some(symbol.size().to_string()),
    ));
    let zero_directive = AssemblerInstruction::Directive(Directive::new(
        "zero".to_string(),
        Some(symbol.size().to_string()),
        None,
    ))
    .add_label(symbol.name());

    code.push(text_directive);
    code.push(globl_directive);
    code.push(bss_directive);
    code.push(align_directive);
    code.push(type_directive);
    code.push(size_directive);
    code.push(zero_directive);

    code
}
