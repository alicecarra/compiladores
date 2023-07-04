use anyhow::bail;
use cfgrammar::Span;
use lrlex::{DefaultLexerTypes, LRNonStreamingLexer};
use lrpar::NonStreamingLexer;

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
    pub fn add_next(self, next: Box<ASTNode>) -> Result<Self, anyhow::Error> {
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
            ast_node => bail!("Erro: {:#?} não deve ter filhos.", ast_node),
        };
        Ok(ast_node)
    }

    pub fn print_label(&self, lexer: &LRNonStreamingLexer<DefaultLexerTypes>) {
        if *self == ASTNode::None {
            // Nó vazio não possui label para exibir.
            return;
        }

        println!("{}", self.label(lexer));
    }

    pub fn print(&self, lexer: &LRNonStreamingLexer<DefaultLexerTypes>) {
        self.print_label(lexer);
        match self {
            ASTNode::FunctionDeclaration(node) => {
                node.comm.print_parent(self);
                node.next_function.print_parent(self);

                node.comm.print(lexer);
                node.next_function.print(lexer);
            }
            ASTNode::InitializedVariable(node) => {
                node.ident.print_parent(self);
                node.lit.print_parent(self);
                if let Some(next) = &node.next {
                    next.print_parent(self);
                }

                node.ident.print(lexer);
                node.lit.print(lexer);
                if let Some(next) = &node.next {
                    next.print(lexer);
                }
            }
            ASTNode::AssignmentCommand(node) => {
                node.ident.print_parent(self);
                node.expr.print_parent(self);
                node.next.print_parent(self);

                node.ident.print(lexer);
                node.expr.print(lexer);
                node.next.print(lexer);
            }
            ASTNode::FunctionCallCommand(node) => {
                node.expr.print_parent(self);
                node.next.print_parent(self);

                node.expr.print(lexer);
                node.next.print(lexer);
            }
            ASTNode::ReturnCommand(node) => {
                node.expr.print_parent(self);

                node.expr.print(lexer);
            }
            ASTNode::WhileCommand(node) => {
                node.expr.print_parent(self);
                node.fst_comm.print_parent(self);
                node.next.print_parent(self);

                node.expr.print(lexer);
                node.fst_comm.print(lexer);
                node.next.print(lexer);
            }
            ASTNode::IfCommand(node) => {
                node.expr.print_parent(self);
                node.true_fst_comm.print_parent(self);
                node.false_fst_comm.print_parent(self);
                node.next.print_parent(self);

                node.expr.print(lexer);
                node.true_fst_comm.print(lexer);
                node.false_fst_comm.print(lexer);
                node.next.print(lexer);
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
                node.child_left.print_parent(self);
                node.child_right.print_parent(self);
                node.next.print_parent(self);

                node.child_left.print(lexer);
                node.child_right.print(lexer);
                node.next.print(lexer);
            }
            ASTNode::NegateExpression(node) | ASTNode::MinusExpression(node) => {
                node.child.print_parent(self);
                node.next.print_parent(self);

                node.child.print(lexer);
                node.next.print(lexer);
            }
            ASTNode::LiteralInt(node) => {
                node.next.print_parent(self);
                node.next.print(lexer);
            }
            ASTNode::LiteralFloat(node) => {
                node.next.print_parent(self);
                node.next.print(lexer);
            }
            ASTNode::LiteralBool(node) => {
                node.next.print_parent(self);
                node.next.print(lexer);
            }
            ASTNode::Identifier(node) => {
                node.next.print_parent(self);
                node.next.print(lexer);
            }
            ASTNode::None => { /* Não é um no na pratica. */ }
        }
    }

    fn print_parent(&self, parent: &ASTNode) {
        if *self == ASTNode::None {
            // Nó vazio não possui pai para exibir.
            return;
        }
        println!("{}, {}", parent.addr(), self.addr());
    }

    fn addr(&self) -> String {
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
            ASTNode::FunctionCallCommand(node) => format!("call {}", lexer.span_str(node.name)),
            ASTNode::ReturnCommand(_) => "return".to_owned(),
            ASTNode::WhileCommand(_) => "while".to_owned(),
            ASTNode::IfCommand(_) => "if".to_owned(),
            ASTNode::OrExpression(_) => "||".to_owned(),
            ASTNode::AndExpression(_) => "&&".to_owned(),
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
        format!("{} [label=\"{}\"];", self.addr(), label)
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

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclaration {
    pub span: Span,
    pub comm: Box<ASTNode>,
    pub next_function: Box<ASTNode>,
    pub name: Span,
}

impl FunctionDeclaration {
    pub fn new(span: Span, comm: Box<ASTNode>, name: Span) -> Self {
        Self {
            span,
            comm,
            next_function: Box::new(ASTNode::None),
            name,
        }
    }

    pub fn add_next_fn(&mut self, next_fn: Box<ASTNode>) {
        self.next_function = next_fn;
    }
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnCommand {
    pub span: Span,
    pub expr: Box<ASTNode>,
}

impl ReturnCommand {
    pub fn new(span: Span, expr: Box<ASTNode>) -> Self {
        Self { span, expr }
    }
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct LiteralInt {
    pub span: Span,
    pub line: usize,
    pub next: Box<ASTNode>,
}

impl LiteralInt {
    pub fn new(span: Span, line: usize) -> Self {
        Self {
            span,
            line,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LiteralFloat {
    pub line: usize,
    pub span: Span,
    pub next: Box<ASTNode>,
}

impl LiteralFloat {
    pub fn new(span: Span, line: usize) -> Self {
        Self {
            span,
            line,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LiteralBool {
    pub span: Span,
    pub line: usize,
    pub next: Box<ASTNode>,
}

impl LiteralBool {
    pub fn new(span: Span, line: usize) -> Self {
        Self {
            span,
            line,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub span: Span,
    pub line: usize,
    pub next: Box<ASTNode>,
}

impl Identifier {
    pub fn new(span: Span, line: usize) -> Self {
        Self {
            span,
            line,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}
