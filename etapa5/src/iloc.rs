#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ILOC {
    Arithmetic(FullOperation),
    LoadImediate(OneInputOneOutput),
    LoadOffSet(FullOperation),
    StoreImediate(OneInputOneOutput),
    StoreOffSet(OneInputTwoOutput),
    RegisterCopy(OneInputOneOutput),
    Compare(CompareInstruction),
    ConditionalBranch(OneInputTwoOutput),
    Jump(Jump),
    Nop(Option<String>),
    Empty,
}

impl ILOC {
    pub fn add_arithmetic_instruction(&mut self, name: String) {
        match self {
            ILOC::Arithmetic(op) => op.name = name,
            _ => panic!("Não deveria ser possível adicionar nome nessa instrução."),
        }
    }

    pub fn print(&self) {
        match self {
            ILOC::Arithmetic(op) => op.print(),
            ILOC::LoadImediate(op) => op.print(),
            ILOC::LoadOffSet(op) => op.print(),
            ILOC::StoreImediate(op) => op.print(),
            ILOC::StoreOffSet(op) => op.print(),
            ILOC::RegisterCopy(op) => op.print(),
            ILOC::Compare(op) => op.print(),
            ILOC::ConditionalBranch(op) => op.print(),
            ILOC::Jump(op) => op.print(),
            ILOC::Nop(label) => {
                if let Some(label) = label {
                    print!("{label}: ");
                }
                println!("nop");
            }
            ILOC::Empty => (),
        }
    }

    pub fn add_label(self, label: String) -> Self {
        match self {
            ILOC::Arithmetic(mut inst) => {
                inst.add_label(label);
                ILOC::Arithmetic(inst)
            }
            ILOC::LoadImediate(mut inst) => {
                inst.add_label(label);
                ILOC::LoadImediate(inst)
            }
            ILOC::LoadOffSet(mut inst) => {
                inst.add_label(label);
                ILOC::LoadOffSet(inst)
            }
            ILOC::StoreImediate(mut inst) => {
                inst.add_label(label);
                ILOC::StoreImediate(inst)
            }
            ILOC::StoreOffSet(mut inst) => {
                inst.add_label(label);
                ILOC::StoreOffSet(inst)
            }
            ILOC::RegisterCopy(mut inst) => {
                inst.add_label(label);
                ILOC::RegisterCopy(inst)
            }
            ILOC::Compare(mut inst) => {
                inst.add_label(label);
                ILOC::Compare(inst)
            }
            ILOC::ConditionalBranch(mut inst) => {
                inst.add_label(label);
                ILOC::ConditionalBranch(inst)
            }
            ILOC::Jump(mut inst) => {
                inst.add_label(label);
                ILOC::Jump(inst)
            }
            ILOC::Nop(_) => ILOC::Nop(Some(label)),
            ILOC::Empty => ILOC::Empty,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FullOperation {
    pub name: String,
    pub first_operand: String,
    pub second_operand: String,
    pub destination: String,
    pub label: Option<String>,
}

impl FullOperation {
    pub fn new(
        name: String,
        first_operand: String,
        second_operation: String,
        destination: String,
    ) -> Self {
        Self {
            name,
            first_operand,
            second_operand: second_operation,
            destination,
            label: None,
        }
    }

    pub fn print(&self) {
        if let Some(label) = &self.label {
            print!("{label}: ");
        }
        println!(
            "{} {}, {} => {}",
            self.name, self.first_operand, self.second_operand, self.destination
        );
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CompareInstruction {
    pub name: String,
    pub first_operand: String,
    pub second_operand: String,
    pub destination: String,
    pub label: Option<String>,
}

impl CompareInstruction {
    pub fn new(
        name: String,
        first_operand: String,
        second_operation: String,
        destination: String,
    ) -> Self {
        Self {
            name,
            first_operand,
            second_operand: second_operation,
            destination,
            label: None,
        }
    }

    pub fn print(&self) {
        if let Some(label) = &self.label {
            print!("{label}: ");
        }
        println!(
            "{} {}, {} -> {}",
            self.name, self.first_operand, self.second_operand, self.destination
        );
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

    pub fn print(&self) {
        if let Some(label) = &self.label {
            print!("{label}: ");
        }
        println!("{} {} => {}", self.name, self.operand, self.destination);
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

    pub fn print(&self) {
        if let Some(label) = &self.label {
            print!("{label}: ");
        }
        println!(
            "{} {} => {}, {}",
            self.name, self.operand, self.destination, self.offset
        );
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Jump {
    pub name: String,
    pub destination: String,
    pub label: Option<String>,
}

impl Jump {
    pub fn new(name: String, destination: String) -> Self {
        Self {
            name,
            destination,
            label: None,
        }
    }

    pub fn print(&self) {
        if let Some(label) = &self.label {
            print!("{label}: ");
        }
        println!("{} => {}", self.name, self.destination);
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}
