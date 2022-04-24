use std::fmt;
use std::hash::Hash;

use crate::sym::Symbol;
use crate::Span;

use super::{Block, TyKind};

#[derive(PartialEq, Eq, Hash)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Add,
    Sub,
    Mul,
    Div,
}

impl BinOpKind {
    pub fn as_str(self) -> &'static str {
        match self {
            BinOpKind::Equal => "==",
            BinOpKind::NotEqual => "!=",
            BinOpKind::Less => "<",
            BinOpKind::LessEqual => "<=",
            BinOpKind::Greater => ">",
            BinOpKind::GreaterEqual => ">=",
            BinOpKind::Add => "+",
            BinOpKind::Sub => "-",
            BinOpKind::Mul => "*",
            BinOpKind::Div => "/",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOpKind {
    Minus,
    Not,
}

pub struct UnOp {
    pub kind: UnOpKind,
    pub span: Span,
}

impl UnOpKind {
    fn as_char(self) -> char {
        match self {
            UnOpKind::Minus => '-',
            UnOpKind::Not => '!',
        }
    }
}

#[derive(Clone, Copy)]
pub struct TotalF64(pub f64);

impl PartialEq for TotalF64 {
    fn eq(&self, other: &TotalF64) -> bool {
        self.0.to_bits() == other.0.to_bits()
    }
}

impl Eq for TotalF64 {}

impl fmt::Debug for TotalF64 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Hash for TotalF64 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state)
    }
}

#[derive(PartialEq, Eq, Debug, Hash)]
pub enum LiteralKind {
    Int(u128),
    String(Symbol),
    Float(TotalF64),
    Bool(bool),
}

impl LiteralKind {
    pub fn ty(&self) -> TyKind {
        match self {
            LiteralKind::Int(_) => TyKind::I32,
            LiteralKind::String(_) => TyKind::String,
            LiteralKind::Float(_) => TyKind::F32,
            LiteralKind::Bool(_) => TyKind::Bool,
        }
    }
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralKind::Int(i) => write!(f, "{}", i),
            LiteralKind::String(s) => write!(f, "\"{}\"", s),
            LiteralKind::Float(float) => write!(f, "{}", float.0),
            LiteralKind::Bool(b) => write!(f, "{}", b),
        }
    }
}

#[derive(PartialEq, Eq, Hash)]
pub struct Literal {
    pub kind: LiteralKind,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Else {
    ElseIf(Box<ExprIf>, Span),
    Else(Block),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprIf {
    pub expr: Box<Expr>,
    pub block: Block,
    pub else_: Option<Else>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprWhile {
    pub while_: Span,
    pub expr: Box<Expr>,
    pub block: Block,
}

#[derive(Debug)]
#[derive(PartialEq, Eq, Hash)]
pub enum ExprKind {
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    UnOp(UnOpKind, Box<Expr>),
    Group(Box<Expr>, Span),
    Literal(Literal),
    Ident(Symbol),
    Block(Block),
    Assignment { lhs: Box<Expr>, rhs: Box<Expr> },
    If(ExprIf),
    While(ExprWhile),
    Call { callee: Box<Expr>, args: Vec<Expr> },
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl ExprKind {
    pub fn has_block(&self) -> bool {
        match self {
            ExprKind::BinOp(_, _, _) => false,
            ExprKind::UnOp(_, _) => false,
            ExprKind::Literal(_) => false,
            ExprKind::Ident(_) => false,
            ExprKind::Assignment { .. } => false,
            ExprKind::Call { .. } => false,
            ExprKind::Group(_, _) => false,
            ExprKind::Block(_) => true,
            ExprKind::If(_) => true,
            ExprKind::While { .. } => true,
        }
    }
}
