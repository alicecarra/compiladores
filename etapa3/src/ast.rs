use anyhow::bail;
use cfgrammar::Span;
use lrlex::{DefaultLexerTypes, LRNonStreamingLexer};
use lrpar::NonStreamingLexer;

#[derive(Debug, Clone)]
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
    pub fn add_next(self, next: Box<ASTNode>) -> Result<Self, anyhow::Error> {
        let ast_node = match self {
            ASTNode::InitializedVariable(mut node) => {
                node.add_next(next);
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
            ast_node => bail!("Erro: {:#?} não deve ter filhos.", ast_node),
        };
        Ok(ast_node)
    }

    pub fn span(&self) -> Result<Span, anyhow::Error> {
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
            ASTNode::None => bail!("Não há span"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub span: Span,
    pub comm: Box<ASTNode>,
    pub next_fn: Box<ASTNode>,
    pub name: Span,
}

impl FunctionDeclaration {
    pub fn new(span: Span, comm: Box<ASTNode>, name: Span) -> Self {
        Self {
            span,
            comm,
            next_fn: Box::new(ASTNode::None),
            name,
        }
    }

    pub fn add_next_fn(&mut self, next_fn: Box<ASTNode>) {
        self.next_fn = next_fn;
    }
}

#[derive(Debug, Clone)]
pub struct InitializedVariable {
    pub span: Span,
    pub ident: Box<ASTNode>,
    pub lit: Box<ASTNode>,
    pub next: Option<Box<ASTNode>>,
}

impl InitializedVariable {
    pub fn new(
        span: Span,
        ident: Box<ASTNode>,
        lit: Box<ASTNode>,
        next: Option<Box<ASTNode>>,
    ) -> Self {
        Self {
            span,
            ident,
            lit,
            next,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) -> Result<(), anyhow::Error> {
        match &self.next {
            Some(node) => self.next = Some(Box::new(node.clone().add_next(next)?)),
            None => self.next = Some(next),
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct AssignmentCommand {
    pub span: Span,
    pub ident: Box<ASTNode>,
    pub expr: Box<ASTNode>,
    pub next: Box<ASTNode>,
}

impl AssignmentCommand {
    pub fn new(span: Span, ident: Box<ASTNode>, expr: Box<ASTNode>) -> Self {
        Self {
            span,
            ident,
            expr,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCallCommand {
    pub span: Span,
    pub expr: Box<ASTNode>,
    pub next: Box<ASTNode>,
    pub name: Span,
}

impl FunctionCallCommand {
    pub fn new(span: Span, expr: Box<ASTNode>, name: Span) -> Self {
        Self {
            span,
            expr,
            name,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, Clone)]
pub struct ReturnCommand {
    pub span: Span,
    pub expr: Box<ASTNode>,
}

impl ReturnCommand {
    pub fn new(span: Span, expr: Box<ASTNode>) -> Self {
        Self { span, expr }
    }
}

#[derive(Debug, Clone)]
pub struct WhileCommand {
    pub span: Span,
    pub expr: Box<ASTNode>,
    pub fst_comm: Box<ASTNode>,
    pub next: Box<ASTNode>,
}

impl WhileCommand {
    pub fn new(span: Span, expr: Box<ASTNode>, fst_comm: Box<ASTNode>) -> Self {
        Self {
            span,
            expr,
            fst_comm,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, Clone)]
pub struct IfCommand {
    pub span: Span,
    pub expr: Box<ASTNode>,
    pub true_fst_comm: Box<ASTNode>,
    pub false_fst_comm: Box<ASTNode>,
    pub next: Box<ASTNode>,
}

impl IfCommand {
    pub fn new(
        span: Span,
        expr: Box<ASTNode>,
        true_fst_comm: Box<ASTNode>,
        false_fst_comm: Box<ASTNode>,
    ) -> Self {
        Self {
            span,
            expr,
            true_fst_comm,
            false_fst_comm,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, Clone)]
pub struct BinaryOperation {
    pub span: Span,
    pub child_left: Box<ASTNode>,
    pub child_right: Box<ASTNode>,
    pub next: Box<ASTNode>,
}

impl BinaryOperation {
    pub fn new(span: Span, child_left: Box<ASTNode>, child_right: Box<ASTNode>) -> Self {
        Self {
            span,
            child_left,
            child_right,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOperation {
    pub span: Span,
    pub child: Box<ASTNode>,
    pub next: Box<ASTNode>,
}

impl UnaryOperation {
    pub fn new(span: Span, child: Box<ASTNode>) -> Self {
        Self {
            span,
            child,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, Clone)]
pub struct LiteralInt {
    pub span: Span,
    pub next: Box<ASTNode>,
}

impl LiteralInt {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, Clone)]
pub struct LiteralFloat {
    pub span: Span,
    pub next: Box<ASTNode>,
}

impl LiteralFloat {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, Clone)]
pub struct LiteralBool {
    pub span: Span,
    pub next: Box<ASTNode>,
}

impl LiteralBool {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub span: Span,
    pub next: Box<ASTNode>,
}

impl Identifier {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}
