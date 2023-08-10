use cfgrammar::Span;
use lrlex::{DefaultLexerTypes, LRNonStreamingLexer};
use lrpar::NonStreamingLexer;

use crate::{
    errors::ParsingError,
    get_new_label, get_register, get_symbol, get_temporary,
    iloc::{CompareInstruction, FullOperation, Jump, OneInputOneOutput, OneInputTwoOutput, ILOC},
    type_enum::Type,
    untyped::try_type_inference,
};

#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode {
    FunctionDeclaration(FunctionDeclaration),
    InitializedVariable(InitializedVariable),
    AssignmentCommand(AssignmentCommand),
    FunctionCallCommand(FunctionCallCommand),
    ReturnCommand(ReturnCommand),
    WhileCommand(WhileCommand),
    IfCommand(IfCommand),
    OrExpression(BinaryOperation),
    AndExpression(BinaryOperation),
    EqualExpression(BinaryOperation),
    NotEqualExpression(BinaryOperation),
    LessThanExpression(BinaryOperation),
    GreaterThanExpression(BinaryOperation),
    LessEqualExpression(BinaryOperation),
    GreaterEqualExpression(BinaryOperation),
    AdditionExpression(BinaryOperation),
    SubtractionExpression(BinaryOperation),
    MultiplyExpression(BinaryOperation),
    DivisionExpression(BinaryOperation),
    ModExpression(BinaryOperation),
    NegateExpression(UnaryOperation),
    MinusExpression(UnaryOperation),
    LiteralInt(LiteralInt),
    LiteralFloat(LiteralFloat),
    LiteralBool(LiteralBool),
    Identifier(Identifier),
    None,
}

impl ASTNode {
    pub fn add_next(self, next: Box<ASTNode>) -> Result<Self, ParsingError> {
        let ast_node = match self {
            ASTNode::InitializedVariable(mut node) => {
                node.add_next(next)?;
                ASTNode::InitializedVariable(node)
            }
            ASTNode::AssignmentCommand(mut node) => {
                node.add_next(next);
                ASTNode::AssignmentCommand(node)
            }
            ASTNode::FunctionCallCommand(mut node) => {
                node.add_next(next);
                ASTNode::FunctionCallCommand(node)
            }
            ASTNode::WhileCommand(mut node) => {
                node.add_next(next);
                ASTNode::WhileCommand(node)
            }
            ASTNode::IfCommand(mut node) => {
                node.add_next(next);
                ASTNode::IfCommand(node)
            }
            ASTNode::OrExpression(mut node) => {
                node.add_next(next);
                ASTNode::OrExpression(node)
            }
            ASTNode::AndExpression(mut node) => {
                node.add_next(next);
                ASTNode::AdditionExpression(node)
            }
            ASTNode::EqualExpression(mut node) => {
                node.add_next(next);
                ASTNode::EqualExpression(node)
            }
            ASTNode::NotEqualExpression(mut node) => {
                node.add_next(next);
                ASTNode::NotEqualExpression(node)
            }
            ASTNode::LessThanExpression(mut node) => {
                node.add_next(next);
                ASTNode::LessThanExpression(node)
            }
            ASTNode::GreaterThanExpression(mut node) => {
                node.add_next(next);
                ASTNode::GreaterThanExpression(node)
            }
            ASTNode::LessEqualExpression(mut node) => {
                node.add_next(next);
                ASTNode::LessEqualExpression(node)
            }
            ASTNode::GreaterEqualExpression(mut node) => {
                node.add_next(next);
                ASTNode::GreaterEqualExpression(node)
            }
            ASTNode::AdditionExpression(mut node) => {
                node.add_next(next);
                ASTNode::AdditionExpression(node)
            }
            ASTNode::SubtractionExpression(mut node) => {
                node.add_next(next);
                ASTNode::SubtractionExpression(node)
            }
            ASTNode::MultiplyExpression(mut node) => {
                node.add_next(next);
                ASTNode::MultiplyExpression(node)
            }
            ASTNode::DivisionExpression(mut node) => {
                node.add_next(next);
                ASTNode::DivisionExpression(node)
            }
            ASTNode::ModExpression(mut node) => {
                node.add_next(next);
                ASTNode::ModExpression(node)
            }
            ASTNode::NegateExpression(mut node) => {
                node.add_next(next);
                ASTNode::NegateExpression(node)
            }
            ASTNode::MinusExpression(mut node) => {
                node.add_next(next);
                ASTNode::MinusExpression(node)
            }
            ASTNode::LiteralInt(mut node) => {
                node.add_next(next);
                ASTNode::LiteralInt(node)
            }
            ASTNode::LiteralFloat(mut node) => {
                node.add_next(next);
                ASTNode::LiteralFloat(node)
            }
            ASTNode::LiteralBool(mut node) => {
                node.add_next(next);
                ASTNode::LiteralBool(node)
            }
            ASTNode::Identifier(mut node) => {
                node.add_next(next);
                ASTNode::Identifier(node)
            }
            ast_node => Err(ParsingError::AddNextToNone(format!(
                "{:#?} não deveria ter nó filho.",
                ast_node
            )))?,
        };
        Ok(ast_node)
    }

    pub fn label_to_string(&self, lexer: &LRNonStreamingLexer<DefaultLexerTypes>) -> String {
        if *self == ASTNode::None {
            // Nó vazio não possui label para exibir.
            return "".to_string();
        }

        self.label(lexer)
    }

    pub fn node_to_string(&self, lexer: &LRNonStreamingLexer<DefaultLexerTypes>) -> String {
        let mut current_string = self.label_to_string(lexer);

        match self {
            ASTNode::FunctionDeclaration(node) => {
                current_string += &node.first_command.parent_to_string(self);
                current_string += &node.next_function.parent_to_string(self);

                current_string += &node.first_command.node_to_string(lexer);
                current_string += &node.next_function.node_to_string(lexer);
            }
            ASTNode::InitializedVariable(node) => {
                current_string += &node.identifier.parent_to_string(self);
                current_string += &node.literal.parent_to_string(self);

                if let Some(next) = &node.next {
                    current_string += &next.parent_to_string(self);
                }

                current_string += &node.identifier.node_to_string(lexer);
                current_string += &node.literal.node_to_string(lexer);
                if let Some(next) = &node.next {
                    current_string += &next.node_to_string(lexer);
                }
            }
            ASTNode::AssignmentCommand(node) => {
                current_string += &node.identifier.parent_to_string(self);
                current_string += &node.expression.parent_to_string(self);
                current_string += &node.next.parent_to_string(self);

                current_string += &node.identifier.node_to_string(lexer);
                current_string += &node.expression.node_to_string(lexer);
                current_string += &node.next.node_to_string(lexer);
            }
            ASTNode::FunctionCallCommand(node) => {
                current_string += &node.expression.parent_to_string(self);
                current_string += &node.next.parent_to_string(self);

                current_string += &node.expression.node_to_string(lexer);
                current_string += &node.next.node_to_string(lexer);
            }
            ASTNode::ReturnCommand(node) => {
                current_string += &node.expression.parent_to_string(self);

                current_string += &node.expression.node_to_string(lexer);
            }
            ASTNode::WhileCommand(node) => {
                current_string += &node.expression.parent_to_string(self);
                current_string += &node.first_command.parent_to_string(self);
                current_string += &node.next.parent_to_string(self);

                current_string += &node.expression.node_to_string(lexer);
                current_string += &node.first_command.node_to_string(lexer);
                current_string += &node.next.node_to_string(lexer);
            }
            ASTNode::IfCommand(node) => {
                current_string += &node.expression.parent_to_string(self);
                current_string += &node.true_first_command.parent_to_string(self);
                current_string += &node.false_first_command.parent_to_string(self);
                current_string += &node.next.parent_to_string(self);

                current_string += &node.expression.node_to_string(lexer);
                current_string += &node.true_first_command.node_to_string(lexer);
                current_string += &node.false_first_command.node_to_string(lexer);
                current_string += &node.next.node_to_string(lexer);
            }
            ASTNode::OrExpression(node)
            | ASTNode::AndExpression(node)
            | ASTNode::EqualExpression(node)
            | ASTNode::NotEqualExpression(node)
            | ASTNode::LessThanExpression(node)
            | ASTNode::GreaterThanExpression(node)
            | ASTNode::LessEqualExpression(node)
            | ASTNode::GreaterEqualExpression(node)
            | ASTNode::AdditionExpression(node)
            | ASTNode::SubtractionExpression(node)
            | ASTNode::MultiplyExpression(node)
            | ASTNode::DivisionExpression(node)
            | ASTNode::ModExpression(node) => {
                current_string += &node.child_left.parent_to_string(self);
                current_string += &node.child_right.parent_to_string(self);
                current_string += &node.next.parent_to_string(self);

                current_string += &node.child_left.node_to_string(lexer);
                current_string += &node.child_right.node_to_string(lexer);
                current_string += &node.next.node_to_string(lexer);
            }
            ASTNode::NegateExpression(node) | ASTNode::MinusExpression(node) => {
                current_string += &node.child.parent_to_string(self);
                current_string += &node.next.parent_to_string(self);

                current_string += &node.child.node_to_string(lexer);
                current_string += &node.next.node_to_string(lexer);
            }
            ASTNode::LiteralInt(node) => {
                current_string += &node.next.parent_to_string(self);
                current_string += &node.next.node_to_string(lexer);
            }
            ASTNode::LiteralFloat(node) => {
                current_string += &node.next.parent_to_string(self);
                current_string += &node.next.node_to_string(lexer);
            }
            ASTNode::LiteralBool(node) => {
                current_string += &node.next.parent_to_string(self);
                current_string += &node.next.node_to_string(lexer);
            }
            ASTNode::Identifier(node) => {
                current_string += &node.next.parent_to_string(self);
                current_string += &node.next.node_to_string(lexer);
            }
            ASTNode::None => { /* Não é um no na pratica. */ }
        }
        current_string
    }

    fn parent_to_string(&self, parent: &ASTNode) -> String {
        if *self == ASTNode::None {
            // Nó vazio não possui pai para exibir.
            return "".to_string();
        }
        format!(
            "{}, {}\n",
            parent.hex_address_to_str(),
            self.hex_address_to_str()
        )
    }

    fn hex_address_to_str(&self) -> String {
        match self {
            ASTNode::FunctionDeclaration(node) => format!("{node:p}"),
            ASTNode::InitializedVariable(node) => format!("{node:p}"),
            ASTNode::AssignmentCommand(node) => format!("{node:p}"),
            ASTNode::FunctionCallCommand(node) => format!("{node:p}"),
            ASTNode::ReturnCommand(node) => format!("{node:p}"),
            ASTNode::WhileCommand(node) => format!("{node:p}"),
            ASTNode::IfCommand(node) => format!("{node:p}"),
            ASTNode::OrExpression(node) => format!("{node:p}"),
            ASTNode::AndExpression(node) => format!("{node:p}"),
            ASTNode::EqualExpression(node) => format!("{node:p}"),
            ASTNode::NotEqualExpression(node) => format!("{node:p}"),
            ASTNode::LessThanExpression(node) => format!("{node:p}"),
            ASTNode::GreaterThanExpression(node) => format!("{node:p}"),
            ASTNode::LessEqualExpression(node) => format!("{node:p}"),
            ASTNode::GreaterEqualExpression(node) => format!("{node:p}"),
            ASTNode::AdditionExpression(node) => format!("{node:p}"),
            ASTNode::SubtractionExpression(node) => format!("{node:p}"),
            ASTNode::MultiplyExpression(node) => format!("{node:p}"),
            ASTNode::DivisionExpression(node) => format!("{node:p}"),
            ASTNode::ModExpression(node) => format!("{node:p}"),
            ASTNode::NegateExpression(node) => format!("{node:p}"),
            ASTNode::MinusExpression(node) => format!("{node:p}"),
            ASTNode::LiteralInt(node) => format!("{node:p}"),
            ASTNode::LiteralFloat(node) => format!("{node:p}"),
            ASTNode::LiteralBool(node) => format!("{node:p}"),
            ASTNode::Identifier(node) => format!("{node:p}"),
            ASTNode::None => "".to_owned(),
        }
    }

    fn label(&self, lexer: &LRNonStreamingLexer<DefaultLexerTypes>) -> String {
        let label = match self {
            ASTNode::FunctionDeclaration(node) => lexer.span_str(node.name).to_owned(),
            ASTNode::InitializedVariable(_) => "<=".to_owned(),
            ASTNode::AssignmentCommand(_) => "=".to_owned(),
            ASTNode::FunctionCallCommand(node) => format!(
                "call {}",
                lexer.span_str(
                    node.name
                        .span()
                        .expect("Não foi possível resgatar o nome da função.")
                )
            ),
            ASTNode::ReturnCommand(_) => "return".to_owned(),
            ASTNode::WhileCommand(_) => "while".to_owned(),
            ASTNode::IfCommand(_) => "if".to_owned(),
            ASTNode::OrExpression(_) => "|".to_owned(),
            ASTNode::AndExpression(_) => "&".to_owned(),
            ASTNode::EqualExpression(_) => "==".to_owned(),
            ASTNode::NotEqualExpression(_) => "!=".to_owned(),
            ASTNode::LessThanExpression(_) => "<".to_owned(),
            ASTNode::GreaterThanExpression(_) => ">".to_owned(),
            ASTNode::LessEqualExpression(_) => "<=".to_owned(),
            ASTNode::GreaterEqualExpression(_) => ">=".to_owned(),
            ASTNode::AdditionExpression(_) => "+".to_owned(),
            ASTNode::SubtractionExpression(_) => "-".to_owned(),
            ASTNode::MultiplyExpression(_) => "*".to_owned(),
            ASTNode::DivisionExpression(_) => "/".to_owned(),
            ASTNode::ModExpression(_) => "%".to_owned(),
            ASTNode::NegateExpression(_) => "!".to_owned(),
            ASTNode::MinusExpression(_) => "-".to_owned(),
            ASTNode::LiteralInt(node) => lexer.span_str(node.span).to_owned(),
            ASTNode::LiteralFloat(node) => lexer.span_str(node.span).to_owned(),
            ASTNode::LiteralBool(node) => lexer.span_str(node.span).to_owned(),
            ASTNode::Identifier(node) => lexer.span_str(node.span).to_owned(),
            ASTNode::None => return "".to_owned(),
        };
        format!("{} [label=\"{}\"];", self.hex_address_to_str(), label)
    }

    pub fn span(&self) -> Result<Span, ParsingError> {
        match self {
            ASTNode::FunctionDeclaration(node) => Ok(node.span),
            ASTNode::InitializedVariable(node) => Ok(node.span),
            ASTNode::AssignmentCommand(node) => Ok(node.span),
            ASTNode::FunctionCallCommand(node) => Ok(node.span),
            ASTNode::ReturnCommand(node) => Ok(node.span),
            ASTNode::WhileCommand(node) => Ok(node.span),
            ASTNode::IfCommand(node) => Ok(node.span),
            ASTNode::OrExpression(node) => Ok(node.span),
            ASTNode::AndExpression(node) => Ok(node.span),
            ASTNode::EqualExpression(node) => Ok(node.span),
            ASTNode::NotEqualExpression(node) => Ok(node.span),
            ASTNode::LessThanExpression(node) => Ok(node.span),
            ASTNode::GreaterThanExpression(node) => Ok(node.span),
            ASTNode::LessEqualExpression(node) => Ok(node.span),
            ASTNode::GreaterEqualExpression(node) => Ok(node.span),
            ASTNode::AdditionExpression(node) => Ok(node.span),
            ASTNode::SubtractionExpression(node) => Ok(node.span),
            ASTNode::MultiplyExpression(node) => Ok(node.span),
            ASTNode::DivisionExpression(node) => Ok(node.span),
            ASTNode::ModExpression(node) => Ok(node.span),
            ASTNode::NegateExpression(node) => Ok(node.span),
            ASTNode::MinusExpression(node) => Ok(node.span),
            ASTNode::LiteralInt(node) => Ok(node.span),
            ASTNode::LiteralFloat(node) => Ok(node.span),
            ASTNode::LiteralBool(node) => Ok(node.span),
            ASTNode::Identifier(node) => Ok(node.span),
            ASTNode::None => Err(ParsingError::SpanError(
                "Nó vazio não possui span.".to_string(),
            )),
        }
    }

    fn get_type(&self) -> Type {
        match self {
            ASTNode::FunctionDeclaration(_) => Type::UNKNOWN,
            ASTNode::InitializedVariable(node) => node.variable_type.clone(),
            ASTNode::AssignmentCommand(node) => node.variable_type.clone(),
            ASTNode::FunctionCallCommand(node) => node.variable_type.clone(),
            ASTNode::ReturnCommand(node) => node.variable_type.clone(),
            ASTNode::WhileCommand(node) => node.variable_type.clone(),
            ASTNode::IfCommand(node) => node.variable_type.clone(),
            ASTNode::OrExpression(node) => node.variable_type.clone(),
            ASTNode::AndExpression(node) => node.variable_type.clone(),
            ASTNode::EqualExpression(node) => node.variable_type.clone(),
            ASTNode::NotEqualExpression(node) => node.variable_type.clone(),
            ASTNode::LessThanExpression(node) => node.variable_type.clone(),
            ASTNode::GreaterThanExpression(node) => node.variable_type.clone(),
            ASTNode::LessEqualExpression(node) => node.variable_type.clone(),
            ASTNode::GreaterEqualExpression(node) => node.variable_type.clone(),
            ASTNode::AdditionExpression(node) => node.variable_type.clone(),
            ASTNode::SubtractionExpression(node) => node.variable_type.clone(),
            ASTNode::MultiplyExpression(node) => node.variable_type.clone(),
            ASTNode::DivisionExpression(node) => node.variable_type.clone(),
            ASTNode::ModExpression(node) => node.variable_type.clone(),
            ASTNode::NegateExpression(node) => node.variable_type.clone(),
            ASTNode::MinusExpression(node) => node.variable_type.clone(),
            ASTNode::LiteralInt(_) => Type::INT,
            ASTNode::LiteralFloat(_) => Type::FLOAT,
            ASTNode::LiteralBool(_) => Type::BOOL,
            ASTNode::Identifier(node) => node.variable_type.clone(),
            ASTNode::None => Type::UNKNOWN,
        }
    }

    pub fn update_type(
        self,
        next_type: Type,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<Self, ParsingError> {
        match self {
            ASTNode::InitializedVariable(node) => {
                let node = node.update_type(next_type, lexer)?;
                Ok(ASTNode::InitializedVariable(node))
            }
            _ => Ok(self),
        }
    }

    pub fn generate_my_code(
        self,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<Self, ParsingError> {
        match self {
            ASTNode::InitializedVariable(mut node) => {
                node.generate_my_code(lexer)?;
                Ok(ASTNode::InitializedVariable(node))
            }
            _ => Ok(self), // other nodes should already have generated code
        }
    }

    pub fn code(&self) -> Vec<ILOC> {
        match self {
            ASTNode::FunctionDeclaration(node) => node.code.clone(),
            ASTNode::InitializedVariable(node) => node.code.clone(),
            ASTNode::AssignmentCommand(node) => node.code.clone(),
            ASTNode::FunctionCallCommand(node) => node.code.clone(),
            ASTNode::ReturnCommand(node) => node.code.clone(),
            ASTNode::IfCommand(node) => node.code.clone(),
            ASTNode::WhileCommand(node) => node.code.clone(),
            ASTNode::EqualExpression(node) => node
                .code
                .clone()
                .into_iter()
                .map(|instruction| match instruction {
                    ILOC::Compare(mut instruction) => {
                        if instruction.name.is_empty() {
                            instruction.name = "cmp_EQ".to_string();
                        }
                        ILOC::Compare(instruction)
                    }
                    ILOC::Arithmetic(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "cmp_EQ".to_string();
                        }
                        ILOC::Arithmetic(inst)
                    }
                    instruction => instruction,
                })
                .collect(),
            ASTNode::NotEqualExpression(node) => node
                .code
                .clone()
                .into_iter()
                .map(|instruction| match instruction {
                    ILOC::Compare(mut instruction) => {
                        if instruction.name.is_empty() {
                            instruction.name = "cmp_NE".to_string();
                        }
                        ILOC::Compare(instruction)
                    }
                    ILOC::Arithmetic(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "cmp_NE".to_string();
                        }
                        ILOC::Arithmetic(inst)
                    }
                    instruction => instruction,
                })
                .collect(),
            ASTNode::LessThanExpression(node) => node
                .code
                .clone()
                .into_iter()
                .map(|instruction| match instruction {
                    ILOC::Compare(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "cmp_LT".to_string();
                        }
                        ILOC::Compare(inst)
                    }
                    ILOC::Arithmetic(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "cmp_LT".to_string();
                        }
                        ILOC::Arithmetic(inst)
                    }
                    instruction => instruction,
                })
                .collect(),
            ASTNode::GreaterThanExpression(node) => node
                .code
                .clone()
                .into_iter()
                .map(|instruction| match instruction {
                    ILOC::Compare(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "cmp_GT".to_string();
                        }
                        ILOC::Compare(inst)
                    }
                    ILOC::Arithmetic(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "cmp_GT".to_string();
                        }
                        ILOC::Arithmetic(inst)
                    }
                    instruction => instruction,
                })
                .collect(),
            ASTNode::LessEqualExpression(node) => node
                .code
                .clone()
                .into_iter()
                .map(|instruction| match instruction {
                    ILOC::Compare(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "cmp_LE".to_string();
                        }
                        ILOC::Compare(inst)
                    }
                    ILOC::Arithmetic(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "cmp_LE".to_string();
                        }
                        ILOC::Arithmetic(inst)
                    }
                    instruction => instruction,
                })
                .collect(),
            ASTNode::GreaterEqualExpression(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    ILOC::Compare(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "cmp_GE".to_string();
                        }
                        ILOC::Compare(inst)
                    }
                    ILOC::Arithmetic(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "cmp_GE".to_string();
                        }
                        ILOC::Arithmetic(inst)
                    }
                    instruction => instruction,
                })
                .collect(),
            ASTNode::OrExpression(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    ILOC::Arithmetic(mut instruction) => {
                        if instruction.name.is_empty() {
                            instruction.name = "or".to_string();
                        }
                        ILOC::Arithmetic(instruction)
                    }
                    instruction => instruction,
                })
                .collect(),
            ASTNode::AndExpression(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    ILOC::Arithmetic(mut instruction) => {
                        if instruction.name.is_empty() {
                            instruction.name = "and".to_string();
                        }
                        ILOC::Arithmetic(instruction)
                    }
                    instruction => instruction,
                })
                .collect(),
            ASTNode::AdditionExpression(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    ILOC::Arithmetic(mut instruction) => {
                        if instruction.name.is_empty() {
                            instruction.name = "add".to_string();
                        }
                        ILOC::Arithmetic(instruction)
                    }
                    instruction => instruction,
                })
                .collect(),
            ASTNode::SubtractionExpression(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    ILOC::Arithmetic(mut instruction) => {
                        if instruction.name.is_empty() {
                            instruction.name = "sub".to_string();
                        }
                        ILOC::Arithmetic(instruction)
                    }
                    instruction => instruction,
                })
                .collect(),
            ASTNode::MultiplyExpression(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    ILOC::Arithmetic(mut instruction) => {
                        if instruction.name.is_empty() {
                            instruction.name = "mult".to_string();
                        }
                        ILOC::Arithmetic(instruction)
                    }
                    instruction => instruction,
                })
                .collect(),
            ASTNode::DivisionExpression(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    ILOC::Arithmetic(mut instruction) => {
                        if instruction.name.is_empty() {
                            instruction.name = "div".to_string();
                        }
                        ILOC::Arithmetic(instruction)
                    }
                    instruction => instruction,
                })
                .collect(),
            ASTNode::ModExpression(node) => node.code.clone(),
            ASTNode::NegateExpression(node) => node.code.clone(),
            ASTNode::MinusExpression(node) => node.code.clone(),
            ASTNode::LiteralInt(node) => node.code.clone(),
            ASTNode::LiteralFloat(node) => node.code.clone(),
            ASTNode::LiteralBool(node) => node.code.clone(),
            ASTNode::Identifier(node) => node.code.clone(),
            ASTNode::None => vec![ILOC::Empty],
        }
    }

    pub fn temporary(&self) -> String {
        match self {
            ASTNode::FunctionCallCommand(node) => node.temporary.clone(),
            ASTNode::OrExpression(node) => node.temporary.clone(),
            ASTNode::AndExpression(node) => node.temporary.clone(),
            ASTNode::EqualExpression(node) => node.temporary.clone(),
            ASTNode::NotEqualExpression(node) => node.temporary.clone(),
            ASTNode::LessThanExpression(node) => node.temporary.clone(),
            ASTNode::GreaterThanExpression(node) => node.temporary.clone(),
            ASTNode::LessEqualExpression(node) => node.temporary.clone(),
            ASTNode::GreaterEqualExpression(node) => node.temporary.clone(),
            ASTNode::AdditionExpression(node) => node.temporary.clone(),
            ASTNode::SubtractionExpression(node) => node.temporary.clone(),
            ASTNode::MultiplyExpression(node) => node.temporary.clone(),
            ASTNode::DivisionExpression(node) => node.temporary.clone(),
            ASTNode::LiteralInt(node) => node.temporary.clone(),
            ASTNode::Identifier(node) => node.temporary.clone(),
            ASTNode::FunctionDeclaration(_) => unimplemented!("Implementação não realizada"),
            ASTNode::InitializedVariable(_) => unimplemented!("Implementação não realizada"),
            ASTNode::AssignmentCommand(_) => unimplemented!("Implementação não realizada"),
            ASTNode::ReturnCommand(_) => unimplemented!("Implementação não realizada"),
            ASTNode::IfCommand(_) => unimplemented!("Implementação não realizada"),
            ASTNode::WhileCommand(_) => unimplemented!("Implementação não realizada"),
            ASTNode::ModExpression(_) => unimplemented!("Implementação não realizada"),
            ASTNode::NegateExpression(_) => unimplemented!("Implementação não realizada"),
            ASTNode::MinusExpression(_) => unimplemented!("Implementação não realizada"),
            ASTNode::LiteralBool(_) => unimplemented!("Implementação não realizada"),
            ASTNode::LiteralFloat(_) => unimplemented!("Implementação não realizada"),
            ASTNode::None => unimplemented!("Implementação não realizada"),
        }
    }

    pub fn is_initialized_variable(&self) -> bool {
        matches!(self, ASTNode::InitializedVariable(_))
    }

    pub fn generate_load(
        &mut self,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<(), ParsingError> {
        match self {
            ASTNode::Identifier(node) => node.generate_load(lexer),
            _ => Ok(()),
        }
    }

    pub fn generate_initial_code(&mut self) -> Result<(), ParsingError> {
        {
            let mut code = vec![];
            match self {
                ASTNode::FunctionDeclaration(node) => {
                    code.extend(node.code.clone());
                    node.code = code;
                    Ok(())
                }
                _ => Err(ParsingError::NotRootFunction),
            }?;
        }
        Ok(())
    }

    pub fn get_temporary_vecs(&self) -> Vec<String> {
        match self {
            ASTNode::EqualExpression(expr) => expr.get_temporary_vec(),
            ASTNode::NotEqualExpression(expr) => expr.get_temporary_vec(),
            ASTNode::LessThanExpression(expr) => expr.get_temporary_vec(),
            ASTNode::GreaterThanExpression(expr) => expr.get_temporary_vec(),
            ASTNode::LessEqualExpression(expr) => expr.get_temporary_vec(),
            ASTNode::GreaterEqualExpression(expr) => expr.get_temporary_vec(),
            ASTNode::OrExpression(expr) => expr.get_temporary_vec(),
            ASTNode::AndExpression(expr) => expr.get_temporary_vec(),
            ASTNode::AdditionExpression(expr) => expr.get_temporary_vec(),
            ASTNode::SubtractionExpression(expr) => expr.get_temporary_vec(),
            ASTNode::MultiplyExpression(expr) => expr.get_temporary_vec(),
            ASTNode::DivisionExpression(expr) => expr.get_temporary_vec(),
            ASTNode::ModExpression(expr) => expr.get_temporary_vec(),
            ASTNode::LiteralInt(expr) => expr.get_temporary_vec(),
            ASTNode::Identifier(expr) => expr.get_temps(),
            _ => vec![],
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclaration {
    pub span: Span,
    pub first_command: Box<ASTNode>,
    pub next_function: Box<ASTNode>,
    pub name: Span,
    pub code: Vec<ILOC>,
}

impl FunctionDeclaration {
    pub fn new(
        span: Span,
        comm: Box<ASTNode>,
        name: Span,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<Self, ParsingError> {
        let code = {
            let symbol = get_symbol(name, lexer)?;
            let label = symbol.get_label();

            let mut code = vec![ILOC::Nop(Some(label))];

            code.extend(comm.code());

            code
        };

        Ok(Self {
            span,
            first_command: comm,
            next_function: Box::new(ASTNode::None),
            name,
            code,
        })
    }

    pub fn add_next_function(&mut self, next_fn: Box<ASTNode>) {
        self.next_function = next_fn;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InitializedVariable {
    pub span: Span,
    pub identifier: Box<ASTNode>,
    pub literal: Box<ASTNode>,
    pub next: Option<Box<ASTNode>>,
    pub variable_type: Type,
    pub code: Vec<ILOC>,
}

impl InitializedVariable {
    pub fn new(
        span: Span,
        identifier: Box<ASTNode>,
        literal: Box<ASTNode>,
        next: Option<Box<ASTNode>>,
    ) -> Self {
        let variable_type = Type::UNKNOWN;
        Self {
            span,
            identifier,
            literal,
            next,
            variable_type,
            code: Vec::new(),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) -> Result<(), ParsingError> {
        match &self.next {
            Some(node) => {
                self.next = match **node {
                    ASTNode::None => Some(next.clone()),
                    _ => Some(Box::new(node.clone().add_next(next.clone())?)),
                }
            }
            None => self.next = Some(next.clone()),
        }
        self.code.extend(next.code());
        Ok(())
    }

    pub fn update_type(
        self,
        variable_type: Type,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<Self, ParsingError> {
        let mut node = self.clone();
        try_type_inference(variable_type.clone(), node.literal.get_type())?;
        node.variable_type = variable_type.clone();
        if let Some(next) = self.next {
            let next = next.update_type(variable_type, lexer)?;
            node.next = Some(Box::new(next));
        }
        Ok(node)
    }

    pub fn generate_my_code(
        &mut self,
        _lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<(), ParsingError> {
        {
            if let Some(next) = self.next.clone() {
                let next = next.generate_my_code(_lexer)?;
                self.next = Some(Box::new(next));
            }

            let symbol = get_symbol(self.identifier.span()?, _lexer)?;

            let reg = get_register(&symbol);
            let inst = ILOC::StoreOffSet(OneInputTwoOutput::new(
                "storeAI".to_string(),
                self.literal.temporary(),
                reg,
                symbol.offset().to_string(),
            ));
            let mut code = vec![];
            code.extend(self.literal.code());
            code.push(inst);
            if let Some(next) = self.next.clone() {
                code.extend(next.code());
            }
            self.code = code;
        };
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AssignmentCommand {
    pub span: Span,
    pub identifier: Box<ASTNode>,
    pub expression: Box<ASTNode>,
    pub next: Box<ASTNode>,
    pub variable_type: Type,
    pub code: Vec<ILOC>,
}

impl AssignmentCommand {
    pub fn new(
        span: Span,
        identifier: Box<ASTNode>,
        expression: Box<ASTNode>,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<Self, ParsingError> {
        try_type_inference(identifier.get_type(), expression.get_type())?;
        let variable_type = identifier.get_type();

        let code = {
            let symbol = get_symbol(identifier.span()?, lexer)?;
            let reg = get_register(&symbol);
            let inst = ILOC::StoreOffSet(OneInputTwoOutput::new(
                "storeAI".to_string(),
                expression.temporary(),
                reg,
                symbol.offset().to_string(),
            ));
            let mut code = vec![];

            code.extend(expression.code());
            code.push(inst);
            code
        };

        Ok(Self {
            span,
            identifier,
            expression,
            next: Box::new(ASTNode::None),
            variable_type,
            code,
        })
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        self.code.extend(next.code());
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCallCommand {
    pub span: Span,
    pub expression: Box<ASTNode>,
    pub next: Box<ASTNode>,
    pub name: Box<ASTNode>,
    pub variable_type: Type,
    pub code: Vec<ILOC>,
    pub temporary: String,
}

impl FunctionCallCommand {
    pub fn new(
        span: Span,
        expression: Box<ASTNode>,
        identifier: Box<ASTNode>,
    ) -> Result<Self, ParsingError> {
        let variable_type = identifier.get_type();

        let temporary = get_temporary();

        Ok(Self {
            span,
            expression,
            name: identifier,
            next: Box::new(ASTNode::None),
            variable_type,
            code: vec![],
            temporary,
        })
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        self.code.extend(next.code());
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnCommand {
    pub span: Span,
    pub expression: Box<ASTNode>,
    pub variable_type: Type,
    pub code: Vec<ILOC>,
}

impl ReturnCommand {
    pub fn new(span: Span, expression: Box<ASTNode>) -> Self {
        let variable_type = expression.get_type();

        Self {
            span,
            expression,
            variable_type,
            code: vec![],
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileCommand {
    pub span: Span,
    pub expression: Box<ASTNode>,
    pub first_command: Box<ASTNode>,
    pub next: Box<ASTNode>,
    pub variable_type: Type,
    pub code: Vec<ILOC>,
}

impl WhileCommand {
    pub fn new(span: Span, expression: Box<ASTNode>, first_command: Box<ASTNode>) -> Self {
        let variable_type = expression.get_type();

        let code = {
            let label_expr = get_new_label();
            let label_true = get_new_label();
            let label_later = get_new_label();

            let temp = get_temporary();
            let op_temp = get_temporary();
            let nop_expr = ILOC::Nop(Some(label_expr.clone()));
            let load_op = ILOC::LoadImediate(OneInputOneOutput::new(
                "loadI".to_string(),
                0.to_string(),
                temp.clone(),
            ));
            let cmp_ne = ILOC::Compare(CompareInstruction::new(
                "cmp_NE".to_string(),
                expression.temporary(),
                temp,
                op_temp.clone(),
            ));
            let cbr = ILOC::ConditionalBranch(OneInputTwoOutput::new(
                "cbr".to_string(),
                op_temp,
                label_true.clone(),
                label_later.clone(),
            ));
            let true_nop = ILOC::Nop(Some(label_true));
            let jump_back = ILOC::Jump(Jump::new("jumpI".to_string(), label_expr));
            let later_nop = ILOC::Nop(Some(label_later));

            let mut code = vec![];
            code.push(nop_expr);
            code.extend(expression.code());
            code.push(load_op);
            code.push(cmp_ne);
            code.push(cbr);
            code.push(true_nop);
            code.extend(first_command.code());
            code.push(jump_back);
            code.push(later_nop);

            code
        };

        Self {
            span,
            expression,
            first_command,
            next: Box::new(ASTNode::None),
            variable_type,
            code,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        self.code.extend(next.code());
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfCommand {
    pub span: Span,
    pub expression: Box<ASTNode>,
    pub true_first_command: Box<ASTNode>,
    pub false_first_command: Box<ASTNode>,
    pub next: Box<ASTNode>,
    pub variable_type: Type,
    pub code: Vec<ILOC>,
}

impl IfCommand {
    pub fn new(
        span: Span,
        expression: Box<ASTNode>,
        true_first_command: Box<ASTNode>,
        false_first_command: Box<ASTNode>,
    ) -> Self {
        let variable_type = expression.get_type();

        let code = {
            let label_true = get_new_label();
            let label_false = get_new_label();
            let label_later = get_new_label();

            let temp = get_temporary();
            let op_temp = get_temporary();
            let load_op = ILOC::LoadImediate(OneInputOneOutput::new(
                "loadI".to_string(),
                0.to_string(),
                temp.clone(),
            ));
            let cmp_ne = ILOC::Compare(CompareInstruction::new(
                "cmp_NE".to_string(),
                expression.temporary(),
                temp,
                op_temp.clone(),
            ));
            let cbr = ILOC::ConditionalBranch(OneInputTwoOutput::new(
                "cbr".to_string(),
                op_temp,
                label_true.clone(),
                label_false.clone(),
            ));
            let true_nop = ILOC::Nop(Some(label_true));
            let jump_later = ILOC::Jump(Jump::new("jumpI".to_string(), label_later.clone()));
            let false_nop = ILOC::Nop(Some(label_false));
            let later_nop = ILOC::Nop(Some(label_later));

            let mut code = vec![];
            code.extend(expression.code());
            code.push(load_op);
            code.push(cmp_ne);
            code.push(cbr);
            code.push(true_nop);
            code.extend(true_first_command.code());
            code.push(jump_later);
            code.push(false_nop);
            code.extend(false_first_command.code());
            code.push(later_nop);
            code
        };

        Self {
            span,
            expression,
            true_first_command,
            false_first_command,
            next: Box::new(ASTNode::None),
            variable_type,
            code,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        self.code.extend(next.code());
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryOperation {
    pub span: Span,
    pub child_left: Box<ASTNode>,
    pub child_right: Box<ASTNode>,
    pub next: Box<ASTNode>,
    pub variable_type: Type,
    temporary: String,
    code: Vec<ILOC>,
}

impl BinaryOperation {
    pub fn new(
        span: Span,
        child_left: Box<ASTNode>,
        child_right: Box<ASTNode>,
    ) -> Result<Self, ParsingError> {
        let variable_type = try_type_inference(child_left.get_type(), child_right.get_type())?;

        let temporary = get_temporary();
        let code = {
            let inst = ILOC::Arithmetic(FullOperation::new(
                "".to_string(),
                child_left.temporary(),
                child_right.temporary(),
                temporary.clone(),
            ));
            let mut code = vec![];

            code.extend(child_left.code());
            code.extend(child_right.code());
            code.push(inst);
            code
        };
        Ok(Self {
            span,
            child_left,
            child_right,
            next: Box::new(ASTNode::None),
            variable_type,
            code,
            temporary,
        })
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        self.code.extend(next.code());
    }

    pub fn get_temporary_vec(&self) -> Vec<String> {
        let mut temps = vec![self.temporary.clone()];
        temps.extend(self.next.get_temporary_vecs());
        temps
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryOperation {
    pub span: Span,
    pub child: Box<ASTNode>,
    pub next: Box<ASTNode>,
    pub variable_type: Type,
    code: Vec<ILOC>,
    temporary: String,
}

impl UnaryOperation {
    pub fn new(span: Span, child: Box<ASTNode>) -> Self {
        let variable_type = child.get_type();
        Self {
            span,
            child,
            next: Box::new(ASTNode::None),
            variable_type,
            code: vec![],
            temporary: "".to_string(),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        self.code.extend(next.code());
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LiteralInt {
    pub span: Span,
    pub line: usize,
    pub next: Box<ASTNode>,
    code: Vec<ILOC>,
    temporary: String,
}

impl LiteralInt {
    pub fn new(span: Span, line: usize, lexer: &dyn NonStreamingLexer<DefaultLexerTypes>) -> Self {
        let val = lexer.span_str(span).to_string();
        let temporary = get_temporary();
        let code = {
            let inst = ILOC::LoadImediate(OneInputOneOutput::new(
                "loadI".to_string(),
                val,
                temporary.clone(),
            ));
            vec![inst]
        };
        Self {
            span,
            line,
            next: Box::new(ASTNode::None),
            code,
            temporary,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        self.code.extend(next.code());
    }

    pub fn get_temporary_vec(&self) -> Vec<String> {
        let mut temps = vec![self.temporary.clone()];
        temps.extend(self.next.get_temporary_vecs());
        temps
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LiteralFloat {
    pub line: usize,
    pub span: Span,
    pub next: Box<ASTNode>,
    code: Vec<ILOC>,
}

impl LiteralFloat {
    pub fn new(span: Span, line: usize) -> Self {
        Self {
            span,
            line,
            next: Box::new(ASTNode::None),
            code: vec![],
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        self.code.extend(next.code());
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LiteralBool {
    pub span: Span,
    pub line: usize,
    pub next: Box<ASTNode>,
    code: Vec<ILOC>,
}

impl LiteralBool {
    pub fn new(span: Span, line: usize) -> Self {
        Self {
            span,
            line,
            next: Box::new(ASTNode::None),
            code: vec![],
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        self.code.extend(next.code());
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub span: Span,
    pub line: usize,
    pub next: Box<ASTNode>,
    pub variable_type: Type,
    code: Vec<ILOC>,
    temporary: String,
}

impl Identifier {
    pub fn new(span: Span, line: usize, lexer: &dyn NonStreamingLexer<DefaultLexerTypes>) -> Self {
        let variable_type = match get_symbol(span, lexer) {
            Ok(symbol) => symbol.get_type(),
            Err(_) => Type::UNKNOWN,
        };

        Self {
            span,
            next: Box::new(ASTNode::None),
            line,
            variable_type,
            code: vec![],
            temporary: "".to_string(),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        self.code.extend(next.code());
    }

    pub fn generate_load(
        &mut self,
        _lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<(), ParsingError> {
        {
            let symbol = get_symbol(self.span, _lexer)?;
            let desloc = symbol.offset().to_string();
            let reg = get_register(&symbol);
            self.temporary = get_temporary();
            self.code = vec![ILOC::LoadOffSet(FullOperation::new(
                "loadAI".to_string(),
                reg,
                desloc,
                self.temporary.clone(),
            ))];
        };
        Ok(())
    }

    pub fn get_temps(&self) -> Vec<String> {
        let mut temps = vec![self.temporary.clone()];
        temps.extend(self.next.get_temporary_vecs());
        temps
    }
}
