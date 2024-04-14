use super::{Item, Resolution};
use crate::ast::{BinOpKind, TotalF64, TyKind, UnOpKind};
use crate::lex::Ident;
use crate::sym::Symbol;
use crate::Id;

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum DefKind {
    Local(LocalDecl),
    Fn(Ident),
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Expr {
    BinOp(BinOpKind, Box<Expr>, Box<Expr>, TyKind),
    UnOp(UnOpKind, Box<Expr>, TyKind),
    Block(Block),
    Call {
        callee: Resolution,
        args: Vec<(Expr, TyKind)>,
        ret: TyKind,
    },
    If {
        cond: Box<Expr>,
        then: Block,
    },
    While {
        cond: Box<Expr>,
        body: Block,
    },
    Assign {
        to: Resolution,
        rvalue: Box<Expr>,
    },
    Literal(Literal),
    Group(Box<Expr>),
    Return(Box<Expr>, TyKind),
    Resolved(Resolution),
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum Literal {
    Int(u128),
    String(Symbol),
    Float(TotalF64),
    Bool(bool),
    Unit,
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Stmt {
    Local(LocalDecl),
    Expr(Expr),
    Item(Item),
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct LocalDecl {
    pub id: Id,
    pub ty: TyKind,
    pub initializer: Option<Expr>,
}
