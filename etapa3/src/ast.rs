use cfgrammar::Span;
use lrlex::{DefaultLexerTypes, LRNonStreamingLexer};
use lrpar::NonStreamingLexer;

pub enum ASTNode {
    LiteralInt(LiteralInt),
    LiteralFloat(LiteralFloat),
    LiteralChar(LiteralChar),
    LiteralBool(LiteralBool),
    Identifier(Identifier),
    None,
}

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

pub struct LiteralChar {
    pub span: Span,
    pub next: Box<ASTNode>,
}

impl LiteralChar {
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
